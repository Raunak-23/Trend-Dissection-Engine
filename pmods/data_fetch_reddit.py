import praw
import os
import json
import datetime
import random
from textblob import TextBlob
from googleapiclient.discovery import build
import numpy as np
from scipy.optimize import curve_fit

# -----------------------------------------------------------
# 1️⃣  YouTube Setup — Fetch Trending Topics
# -----------------------------------------------------------
YOUTUBE_API_KEY = os.environ.get("YOUTUBE_API_KEY")
youtube = build("youtube", "v3", developerKey=YOUTUBE_API_KEY)

def get_youtube_trending(region_code="US", max_results=5):
    request = youtube.videos().list(
        part="snippet,statistics",
        chart="mostPopular",
        regionCode=region_code,
        maxResults=max_results
    )
    response = request.execute()
    trending_topics = []
    for item in response.get("items", []):
        trending_topics.append({
            "title": item["snippet"]["title"],
            "category": item["snippet"].get("categoryId", "Unknown"),
            "publishedAt": item["snippet"]["publishedAt"]
        })
    return trending_topics


# -----------------------------------------------------------
# 2️⃣  Reddit Setup
# -----------------------------------------------------------
reddit = praw.Reddit(
    client_id=os.environ.get("REDDIT_CLIENT_ID"),
    client_secret=os.environ.get("REDDIT_CLIENT_SECRET"),
    user_agent=os.environ.get("REDDIT_USER_AGENT")
)


# -----------------------------------------------------------
# 3️⃣  Helper: Logistic fit to estimate trend lifespan
# -----------------------------------------------------------
def logistic(t, K, r, t0):
    return K / (1 + np.exp(-r * (t - t0)))

def fit_logistic(times, counts):
    if len(times) < 4:
        return None
    try:
        K0 = max(counts) * 1.5
        r0 = 0.5
        t00 = np.mean(times)
        popt, _ = curve_fit(logistic, times, counts, p0=[K0, r0, t00], maxfev=5000)
        K, r, t0 = popt
        # Compute saturation time (90% of K)
        ts_90 = t0 - (1 / r) * np.log((1 / 0.9) - 1)
        lifespan_hours = max(ts_90 - min(times), 0)
        return lifespan_hours
    except Exception:
        return None


# -----------------------------------------------------------
# 4️⃣  Reddit Fetch + Metric Computation
# -----------------------------------------------------------
def analyze_reddit_posts(topic, limit=20):
    posts = reddit.subreddit("all").search(topic, limit=limit, sort="relevance")
    data = []
    for post in posts:
        title = post.title
        score = post.score
        num_comments = post.num_comments
        upvote_ratio = getattr(post, "upvote_ratio", 0)
        created_utc = post.created_utc
        post_age_hours = (datetime.datetime.utcnow() - datetime.datetime.utcfromtimestamp(created_utc)).total_seconds() / 3600
        engagement_velocity = (score + num_comments) / (post_age_hours + 1)
        sentiment = TextBlob(title).sentiment.polarity

        data.append({
            "title": title,
            "score": score,
            "num_comments": num_comments,
            "upvote_ratio": upvote_ratio,
            "post_age_hours": round(post_age_hours, 2),
            "engagement_velocity": round(engagement_velocity, 2),
            "sentiment": round(sentiment, 3),
            "created_utc": created_utc,
            "url": post.url
        })
    return data


# -----------------------------------------------------------
# 5️⃣  Combine Everything — Build JSON
# -----------------------------------------------------------
def build_trend_dataset(region="US"):
    youtube_topics = get_youtube_trending(region)
    now_iso = datetime.datetime.utcnow().isoformat() + "Z"
    trend_dataset = []

    for yt_item in youtube_topics:
        topic = yt_item["title"]
        category = yt_item.get("category", "Unknown")

        reddit_data = analyze_reddit_posts(topic)
        if not reddit_data:
            continue

        # Aggregate Reddit metrics
        avg_score = np.mean([p["score"] for p in reddit_data])
        avg_comments = np.mean([p["num_comments"] for p in reddit_data])
        avg_sentiment = np.mean([p["sentiment"] for p in reddit_data])
        avg_velocity = np.mean([p["engagement_velocity"] for p in reddit_data])

        # --- 🔹 New advanced metrics ---
        # 1. Trend Score (weighted composite)
        trend_score = avg_velocity * (1 + avg_sentiment) * avg_score

        # 2. Trend Lifespan (approximate logistic growth)
        # Build cumulative posts timeline for fitting
        times = [p["post_age_hours"] for p in reddit_data]
        counts = list(range(1, len(times) + 1))
        lifespan_hours = fit_logistic(np.array(times), np.array(counts))
        if lifespan_hours is None:
            # Fallback: heuristic = post_age span
            lifespan_hours = max(times) - min(times) if len(times) > 1 else 12.0

        # 3. External interest index (placeholder, simulate for now)
        external_interest_index = random.randint(50, 100)

        trend_dataset.append({
            "topic": topic,
            "platform_source": "YouTube",
            "category": category,
            "time_fetched": now_iso,
            "trend_score": round(trend_score, 2),
            "trend_lifespan_hours": round(lifespan_hours, 2),
            "external_interest_index": external_interest_index,
            "reddit_metrics": {
                "average_score": round(avg_score, 2),
                "average_comments": round(avg_comments, 2),
                "average_sentiment": round(avg_sentiment, 3),
                "average_velocity": round(avg_velocity, 2),
                "post_count": len(reddit_data)
            },
            "reddit_posts": reddit_data
        })

    return trend_dataset


# -----------------------------------------------------------
# 6️⃣  Save to JSON
# -----------------------------------------------------------
if __name__ == "__main__":
    dataset = build_trend_dataset(region="US")

    with open("enhanced_trend_data.json", "w", encoding="utf-8") as f:
        json.dump(dataset, f, ensure_ascii=False, indent=4)

    print("✅ Enhanced trend data collected → 'enhanced_trend_data.json'")

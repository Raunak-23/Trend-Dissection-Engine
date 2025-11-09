import requests
import praw
import json
import time
import os
import random
import warnings
import numpy as np
from datetime import datetime, timezone
from zoneinfo import ZoneInfo
from textblob import TextBlob
from googleapiclient.discovery import build
from scipy.optimize import curve_fit
from scipy.optimize import OptimizeWarning

warnings.filterwarnings("ignore", category=RuntimeWarning)
warnings.filterwarnings("ignore", category=OptimizeWarning)

# -----------------------------------------------------------
# JSON datetime converter
# -----------------------------------------------------------
def _dt_converter(o):
    if isinstance(o, datetime):
        return o.isoformat()
    raise TypeError(f"Object of type {type(o)} is not JSON serializable")


# -----------------------------------------------------------
# 1️⃣  API Keys and Setup
# -----------------------------------------------------------
YOUTUBE_API_KEY = "AIzaSyDl9m-5zGJq8gINDMGVZqlWoDt8Ajopxkw"
NEWS_API_KEY = "818c81d17eef445c8a8434e4d97032da"

youtube = build("youtube", "v3", developerKey=YOUTUBE_API_KEY)

reddit = praw.Reddit(
    client_id="5bxU6C_DISug5u0yb0cHZA",
    client_secret="l_2cGmBUJ6uTOsLM4Ya2tbf5P4rE-g",
    user_agent="trend_analyzer_bot by u/FunTonight482"
)


# -----------------------------------------------------------
# 2️⃣  Global YouTube — Fetch Trending Topics
# -----------------------------------------------------------
def get_global_youtube_trending(max_results_per_region=5):
    regions = ["US"]
    trending_topics = []

    for region in regions:
        try:
            request = youtube.videos().list(
                part="snippet,statistics",
                chart="mostPopular",
                regionCode=region,
                maxResults=max_results_per_region
            )
            response = request.execute()
            for item in response.get("items", []):
                title = item["snippet"]["title"]
                trending_topics.append({
                    "title": title,
                    "category": item["snippet"].get("categoryId", "Unknown"),
                    "publishedAt": item["snippet"]["publishedAt"],
                    "region": region,
                    "platform_source": "YouTube"
                })
        except Exception as e:
            print(f"⚠️ YouTube API error for {region}: {e}")

    # Remove duplicates based on title
    seen = set()
    unique_trending = []
    for t in trending_topics:
        if t["title"] not in seen:
            unique_trending.append(t)
            seen.add(t["title"])

    print(f"✅ Global YouTube trending fetched from {len(regions)} regions → {len(unique_trending)} unique topics.")
    return unique_trending


# -----------------------------------------------------------
# 3️⃣  NewsAPI — Multi-category Indian News with fallback
# -----------------------------------------------------------
def get_top_news(region_code="us", limit=5):
    categories = ["business", "technology", "entertainment", "sports", "health"]
    all_articles = []

    for cat in categories:
        url = f"https://newsapi.org/v2/top-headlines?country={region_code}&category={cat}&pageSize={limit}&apiKey={NEWS_API_KEY}"
        response = requests.get(url)
        if response.status_code == 429:
            print("⚠️ NewsAPI rate limit hit. Waiting 20 seconds...")
            time.sleep(20)
            response = requests.get(url)

        if response.status_code == 200:
            data = response.json()
            articles = data.get("articles", [])
            for a in articles:
                all_articles.append({
                    "title": a.get("title"),
                    "description": a.get("description"),
                    "source": a.get("source", {}).get("name"),
                    "url": a.get("url"),
                    "publishedAt": a.get("publishedAt"),
                    "category": cat,
                    "platform_source": "News"
                })
        time.sleep(1)

    # Fallback if no Indian articles
    if not all_articles:
        print("⚠️ No top headlines for US — fetching global trending news instead.")
        url = f"https://newsapi.org/v2/everything?q=trending&pageSize={limit}&apiKey={NEWS_API_KEY}"
        response = requests.get(url)
        data = response.json()
        all_articles = [
            {
                "title": a.get("title"),
                "description": a.get("description"),
                "source": a.get("source", {}).get("name"),
                "url": a.get("url"),
                "publishedAt": a.get("publishedAt"),
                "category": "General",
                "platform_source": "News"
            }
            for a in data.get("articles", [])
        ]

    print(f"✅ NewsAPI fetched {len(all_articles)} Indian/global articles.")
    return all_articles


# -----------------------------------------------------------
# 4️⃣  Reddit Analysis — Metrics Computation
# -----------------------------------------------------------
def analyze_reddit_posts(topic, limit=10):
    posts = reddit.subreddit("all").search(topic, limit=limit, sort="relevance")
    data = []
    for post in posts:
        created_utc = post.created_utc
        post_age_hours = (datetime.now(timezone.utc) -
                          datetime.fromtimestamp(created_utc, tz=timezone.utc)).total_seconds() / 3600
        score = post.score
        comments = post.num_comments
        sentiment = TextBlob(post.title).sentiment.polarity
        velocity = (score + comments) / (post_age_hours + 1)

        data.append({
            "title": post.title,
            "score": score,
            "num_comments": comments,
            "upvote_ratio": getattr(post, "upvote_ratio", 0),
            "sentiment": round(sentiment, 3),
            "engagement_velocity": round(velocity, 2),
            "post_age_hours": round(post_age_hours, 2),
            "url": post.url
        })
    return data


# -----------------------------------------------------------
# 5️⃣  Logistic Fit (Trend Lifespan Estimation)
# -----------------------------------------------------------
def logistic(t, K, r, t0):
    return K / (1 + np.exp(-r * (t - t0)))

def fit_logistic(times, counts):
    if len(times) < 4:
        return None
    try:
        K0 = max(counts) * 1.2
        r0 = 0.4
        t00 = np.mean(times)
        popt, _ = curve_fit(logistic, times, counts, p0=[K0, r0, t00], maxfev=4000)
        K, r, t0 = popt
        ts_90 = t0 - (1 / r) * np.log((1 / 0.9) - 1)
        return max(ts_90 - min(times), 0)
    except Exception:
        return None


# -----------------------------------------------------------
# 6️⃣  Build Combined Dataset (Global YouTube + News + Reddit)
# -----------------------------------------------------------
def build_trend_dataset():
    youtube_topics = get_global_youtube_trending()
    news_topics = get_top_news(region_code="us")
    now_ist = datetime.now(ZoneInfo("Asia/Kolkata"))
    now_iso = now_ist.isoformat()
    dataset = []

    all_topics = youtube_topics + news_topics

    for item in all_topics:
        topic = item["title"]
        platform = item.get("platform_source", "Unknown")
        category = item.get("category", "General")

        reddit_data = analyze_reddit_posts(topic)
        if not reddit_data:
            continue

        avg_score = np.mean([p["score"] for p in reddit_data])
        avg_comments = np.mean([p["num_comments"] for p in reddit_data])
        avg_sentiment = np.mean([p["sentiment"] for p in reddit_data])
        avg_velocity = np.mean([p["engagement_velocity"] for p in reddit_data])
        trend_score = (avg_velocity * 0.4 + avg_comments * 0.3 +
                       avg_score * 0.2) * (1 + avg_sentiment)

        times = [p["post_age_hours"] for p in reddit_data]
        counts = list(range(1, len(times) + 1))
        lifespan_hours = fit_logistic(np.array(times), np.array(counts))
        if lifespan_hours is None:
            lifespan_hours = max(
                times) - min(times) if len(times) > 1 else 12.0

        external_interest_index = random.randint(50, 100)

        dataset.append({
            "topic": topic,
            "platform_source": platform,
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

    return dataset


# -----------------------------------------------------------
# 7️⃣  Save to JSON + Summary Report
# -----------------------------------------------------------
if __name__ == "__main__":
    os.makedirs("data_raw", exist_ok=True)
    dataset = build_trend_dataset()

    date_str = datetime.now().strftime("%Y-%m-%d")
    filename = f"data_raw/trends_{date_str}.json"

    with open(filename, "w", encoding="utf-8") as f:
        json.dump(dataset, f, ensure_ascii=False,
                  indent=4, default=_dt_converter)

    total_youtube = sum(1 for d in dataset if d["platform_source"] == "YouTube")
    total_news = sum(1 for d in dataset if d["platform_source"] == "News")
    total_reddit_posts = sum(len(item["reddit_posts"])
                             for item in dataset)

    print(f"✅ Trend data saved → {filename}")
    print(
        f"📊 Summary: {total_youtube} YouTube topics (US), {total_news} News topics (US), {total_reddit_posts} Reddit posts fetched.")

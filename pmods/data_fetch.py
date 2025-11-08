import os
import json
import argparse
import logging
from datetime import datetime
import googleapiclient.discovery
import praw
import pandas as pd
from tqdm import tqdm
import requests

# API Keys via env (fall back to empty)
YOUTUBE_API_KEY = os.environ.get("YOUTUBE_API_KEY", "")
REDDIT_CLIENT_ID = os.environ.get("REDDIT_CLIENT_ID", "")
REDDIT_CLIENT_SECRET = os.environ.get("REDDIT_CLIENT_SECRET", "")
REDDIT_USER_AGENT = os.environ.get("REDDIT_USER_AGENT", "TrendRBot/1.0")
NEWSAPI_KEY = os.environ.get("NEWSAPI_KEY", "")

def setup_youtube():
    """Initialize YouTube API client"""
    try:
        if not YOUTUBE_API_KEY:
            raise ValueError("YOUTUBE_API_KEY is not set")
        youtube = googleapiclient.discovery.build(
            "youtube", "v3", developerKey=YOUTUBE_API_KEY
        )
        return youtube
    except Exception as e:
        logging.error(f"Error setting up YouTube API: {e}")
        return None

def setup_reddit():
    """Initialize Reddit API client"""
    try:
        if not (REDDIT_CLIENT_ID and REDDIT_CLIENT_SECRET and REDDIT_USER_AGENT):
            raise ValueError("Reddit credentials are not fully set")
        reddit = praw.Reddit(
            client_id=REDDIT_CLIENT_ID,
            client_secret=REDDIT_CLIENT_SECRET,
            user_agent=REDDIT_USER_AGENT
        )
        return reddit
    except Exception as e:
        logging.error(f"Error setting up Reddit API: {e}")
        return None

def get_youtube_trending(youtube, region_code="IN", max_results=50):
    """Fetch trending videos from YouTube"""
    try:
        request = youtube.videos().list(
            part="snippet,contentDetails,statistics",
            chart="mostPopular",
            regionCode=region_code,
            maxResults=max_results
        )
        response = request.execute()
        
        trending_videos = []
        for item in response.get('items', []):
            snippet = item.get('snippet', {})
            statistics = item.get('statistics', {})
            video = {
                'video_id': item.get('id'),
                'title': snippet.get('title', ''),
                'channel_title': snippet.get('channelTitle', ''),
                'published_at': snippet.get('publishedAt'),
                'view_count': int(statistics.get('viewCount', 0) or 0),
                'like_count': int(statistics.get('likeCount', 0) or 0),
                'comment_count': int(statistics.get('commentCount', 0) or 0),
                'description': snippet.get('description', ''),
                'tags': snippet.get('tags', []),
                'category_id': snippet.get('categoryId') or item.get('categoryId')
            }
            trending_videos.append(video)
        
        return trending_videos
    except Exception as e:
        logging.error(f"Error fetching YouTube trending: {e}")
        return []

def get_reddit_insights(reddit, topic, limit=10):
    """Search Reddit for a topic and get insights"""
    try:
        search_topic = ' '.join((topic or '').split()[:8])
        subreddits = ['all', 'popular', 'videos', 'trending']
        posts = []
        seen_urls = set()
        for subreddit in subreddits:
            try:
                for submission in reddit.subreddit(subreddit).search(search_topic, limit=limit, sort='relevance'):
                    if submission.url not in seen_urls:
                        post = {
                            'title': submission.title,
                            'score': int(getattr(submission, 'score', 0) or 0),
                            'upvote_ratio': float(getattr(submission, 'upvote_ratio', 0) or 0),
                            'num_comments': int(getattr(submission, 'num_comments', 0) or 0),
                            'created_utc': float(getattr(submission, 'created_utc', 0) or 0),
                            'subreddit': str(getattr(submission, 'subreddit', '')),
                            'url': getattr(submission, 'url', ''),
                            'engagement': (int(getattr(submission, 'score', 0) or 0) + int(getattr(submission, 'num_comments', 0) or 0))
                        }
                        posts.append(post)
                        seen_urls.add(submission.url)
            except Exception as subreddit_error:
                logging.warning(f"Error searching subreddit {subreddit}: {subreddit_error}")
                continue
        return posts
    except Exception as e:
        logging.error(f"Error fetching Reddit insights for {topic}: {e}")
        return []

def get_news_insights(topic, page_size=20):
    """Search NewsAPI for a topic and get insights"""
    if not NEWSAPI_KEY:
        logging.warning("NEWSAPI_KEY not set; skipping NewsAPI.")
        return []
    try:
        search_topic = ' '.join((topic or '').split()[:8])
        url = "https://newsapi.org/v2/everything"
        params = {
            'q': search_topic,
            'pageSize': page_size,
            'language': 'en',
            'sortBy': 'relevancy',
            'apiKey': NEWSAPI_KEY
        }
        response = requests.get(url, params=params, timeout=30)
        response.raise_for_status()
        data = response.json()
        return data.get("articles", [])
    except requests.exceptions.RequestException as e:
        logging.error(f"Error fetching NewsAPI for {topic}: {e}")
        return []

def save_data(data, filename, output_dir="data-raw", latest_path=None):
    """Save data to JSON file and optionally update latest pointer"""
    try:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        os.makedirs(output_dir, exist_ok=True)
        filepath = os.path.join(output_dir, f"{filename}_{timestamp}.json")
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
        logging.info(f"Data saved to {filepath}")
        if latest_path:
            try:
                with open(latest_path, 'w', encoding='utf-8') as f:
                    json.dump(data, f, indent=2, ensure_ascii=False)
                logging.info(f"Updated latest pointer: {latest_path}")
            except Exception as e:
                logging.warning(f"Failed to update latest pointer: {e}")
        return filepath
    except Exception as e:
        logging.error(f"Error saving data: {e}")
        return None

def parse_args():
    p = argparse.ArgumentParser(description="Fetch trending data and Reddit insights")
    p.add_argument("--region", default="IN", help="YouTube region code")
    p.add_argument("--max-results", type=int, default=50, help="Max YouTube results")
    p.add_argument("--output-dir", default="data-raw", help="Output directory for raw data")
    p.add_argument("--latest-path", default="data-raw/latest.json", help="Stable latest JSON path")
    p.add_argument("--log-level", default="INFO", help="Logging level")
    return p.parse_args()


def main():
    args = parse_args()
    logging.basicConfig(level=getattr(logging, args.log_level.upper(), logging.INFO),
                        format='%(asctime)s %(levelname)s %(message)s')

    logging.info("Initializing YouTube API...")
    youtube = setup_youtube()
    if not youtube:
        logging.error("Failed to initialize YouTube API. Check YOUTUBE_API_KEY.")
        return 1

    logging.info("Initializing Reddit API...")
    reddit = setup_reddit()
    if not reddit:
        logging.error("Failed to initialize Reddit API. Check Reddit credentials.")
        return 1

    try:
        reddit.user.me()
        logging.info("Reddit authentication successful")
    except Exception as e:
        logging.error(f"Reddit authentication failed: {e}")
        return 1

    logging.info("Fetching YouTube trending videos...")
    trending_videos = get_youtube_trending(youtube, region_code=args.region, max_results=args.max_results)
    if not trending_videos:
        logging.error("No trending videos found")
        return 1

    youtube_file = save_data(trending_videos, "youtube_trending", output_dir=args.output_dir, latest_path=args.latest_path)

    logging.info("Fetching insights for trending topics...")
    enriched_data = []
    for video in tqdm(trending_videos, desc="Enriching Topics"):
        video_data = video.copy()
        video_data['reddit_insights'] = get_reddit_insights(reddit, video.get('title', ''))
        video_data['news_insights'] = get_news_insights(video.get('title', ''))
        enriched_data.append(video_data)

    enriched_file = save_data(enriched_data, "enriched_trends", output_dir=args.output_dir)

    total_videos = len(trending_videos)
    total_reddit_posts = sum(len(v.get('reddit_insights', [])) for v in enriched_data)
    total_news_articles = sum(len(v.get('news_insights', [])) for v in enriched_data)

    summary = {
        'timestamp': datetime.now().isoformat(),
        'total_trending_videos': total_videos,
        'total_reddit_posts': total_reddit_posts,
        'total_news_articles': total_news_articles,
        'youtube_data_file': youtube_file,
        'enriched_data_file': enriched_file
    }
    logging.info(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
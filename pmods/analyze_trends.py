import os
import json
import argparse
import logging
import pandas as pd
import numpy as np
from datetime import datetime
from textblob import TextBlob
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

def load_data(path: str):
    """Load data from a given JSON path"""
    try:
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except Exception as e:
        logging.error(f"Error loading data from {path}: {e}")
        return None

def analyze_sentiment(text):
    """Analyze sentiment of text using TextBlob"""
    try:
        return TextBlob(text or '').sentiment.polarity
    except Exception as e:
        logging.warning(f"Sentiment analysis failed: {e}")
        return 0

def calculate_engagement_metrics(video):
    """Calculate engagement metrics for a video"""
    try:
        view_count = int(video.get('view_count', 0))
        like_count = int(video.get('like_count', 0))
        comment_count = int(video.get('comment_count', 0))
        
        engagement_rate = (like_count + comment_count) / view_count if view_count > 0 else 0
        reddit_engagement = sum(post['engagement'] for post in video.get('reddit_insights', []))
        
        return {
            'total_engagement': view_count + like_count + comment_count,
            'engagement_rate': engagement_rate,
            'reddit_engagement': reddit_engagement
        }
    except Exception as e:
        print(f"Error calculating engagement metrics: {e}")
        return {
            'total_engagement': 0,
            'engagement_rate': 0,
            'reddit_engagement': 0
        }

def analyze_trends(data):
    """Analyze trends in the data"""
    analysis_results = []
    
    for video in data:
        # Basic metrics
        metrics = calculate_engagement_metrics(video)
        
        # Sentiment analysis
        youtube_sentiment = analyze_sentiment(video['title'] + ' ' + video['description'])
        reddit_sentiments = [analyze_sentiment(post['title']) for post in video.get('reddit_insights', [])]
        avg_reddit_sentiment = np.mean(reddit_sentiments) if reddit_sentiments else 0
        
        # Combine results
        analysis = {
            'video_id': video['video_id'],
            'title': video['title'],
            'channel': video['channel_title'],
            'views': video['view_count'],
            'likes': video['like_count'],
            'comments': video['comment_count'],
            'total_engagement': metrics['total_engagement'],
            'engagement_rate': metrics['engagement_rate'],
            'reddit_engagement': metrics['reddit_engagement'],
            'youtube_sentiment': youtube_sentiment,
            'reddit_sentiment': avg_reddit_sentiment,
            'reddit_posts': len(video.get('reddit_insights', [])),
            'category_id': video['category_id']
        }
        
        analysis_results.append(analysis)
    
    return pd.DataFrame(analysis_results)

def generate_visualizations(df, output_dir="visualizations"):
    """Generate visualizations from the analysis"""
    os.makedirs(output_dir, exist_ok=True)

    plt.style.use('seaborn-v0_8')

    plt.figure(figsize=(12, 6))
    sns.histplot(data=df, x='total_engagement', bins=30)
    plt.title('Distribution of Total Engagement')
    plt.xlabel('Total Engagement')
    plt.yscale('log')
    plt.savefig(os.path.join(output_dir, 'engagement_distribution.png'))
    plt.close()

    plt.figure(figsize=(8, 8))
    sns.scatterplot(data=df, x='total_engagement', y='reddit_engagement')
    plt.title('YouTube vs Reddit Engagement')
    plt.xlabel('YouTube Engagement')
    plt.ylabel('Reddit Engagement')
    plt.savefig(os.path.join(output_dir, 'platform_correlation.png'))
    plt.close()

    plt.figure(figsize=(10, 6))
    sns.boxplot(data=df[['youtube_sentiment', 'reddit_sentiment']])
    plt.title('Sentiment Distribution Across Platforms')
    plt.savefig(os.path.join(output_dir, 'sentiment_comparison.png'))
    plt.close()

    plt.figure(figsize=(12, 6))
    df['category_id'].value_counts().plot(kind='bar')
    plt.title('Distribution of Video Categories')
    plt.xlabel('Category ID')
    plt.ylabel('Count')
    plt.savefig(os.path.join(output_dir, 'category_distribution.png'))
    plt.close()

def generate_report(df, output_dir="data-clean"):
    """Generate analysis report"""
    os.makedirs(output_dir, exist_ok=True)

    summary = {
        'total_videos': int(len(df)),
        'total_views': int(df['views'].sum()),
        'avg_engagement_rate': float(df['engagement_rate'].mean()),
        'avg_youtube_sentiment': float(df['youtube_sentiment'].mean()),
        'avg_reddit_sentiment': float(df['reddit_sentiment'].mean()),
        'top_categories': {str(k): int(v) for k, v in df['category_id'].value_counts().to_dict().items()},
        'timestamp': datetime.now().isoformat()
    }

    with open(os.path.join(output_dir, 'analysis_summary.json'), 'w') as f:
        json.dump(summary, f, indent=2)

    df.to_csv(os.path.join(output_dir, 'detailed_analysis.csv'), index=False)

    return summary

def parse_args():
    p = argparse.ArgumentParser(description="Analyze trends from enriched data")
    p.add_argument("--input", default="data-raw/latest.json", help="Input enriched JSON path")
    p.add_argument("--out-dir", default="data-clean", help="Output directory for reports")
    p.add_argument("--log-level", default="INFO", help="Logging level")
    return p.parse_args()


def main():
    args = parse_args()
    logging.basicConfig(level=getattr(logging, args.log_level.upper(), logging.INFO),
                        format='%(asctime)s %(levelname)s %(message)s')

    logging.info("Loading data...")
    data = load_data(args.input)
    if not data:
        logging.error("Failed to load data")
        return 1

    logging.info("Analyzing trends...")
    df = analyze_trends(data)

    logging.info("Generating visualizations...")
    generate_visualizations(df)

    logging.info("Generating report...")
    summary = generate_report(df, output_dir=args.out_dir)

    logging.info("Analysis complete")
    logging.info(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
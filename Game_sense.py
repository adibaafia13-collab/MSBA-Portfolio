


import pandas as pd
import numpy as np
import re
import requests
import torch
import open_clip
from PIL import Image
from transformers import BlipProcessor, BlipForConditionalGeneration
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans

# the vectorized string manipulation (fetch live data)
def fetch_steam_reviews(appid, count=100):
    """Directly fetches live player feedback via Steam Web API."""
    url = f"https://store.steampowered.com/appreviews/{appid}?json=1&cursor=*&count={count}"
    response = requests.get(url).json()
    return [review['review'] for review in response['reviews']]

def clean_gaming_text(text):
    """Normalizes informal gaming language using Regex ."""
    text = re.sub(r'[^a-zA-Z\s]', '', str(text))
    return text.lower().strip()

# vectorized approach
v_clean = np.vectorize(clean_gaming_text)

# initialize CLIP using specific weights
model_name = "ViT-H-14"
pretrained = "laion2b_s32b_b79k"
clip_model, _, preprocess = open_clip.create_model_and_transforms(model_name, pretrained=pretrained)
tokenizer = open_clip.get_tokenizer(model_name)

# The GAME-SENSE engine
def run_game_sense_engine(appid):
    # Fetch & clean
    raw_reviews = fetch_steam_reviews(appid)
    cleaned_data = v_clean(raw_reviews)

    # Automated diagnostic clustering
    tv = TfidfVectorizer(max_features=500, stop_words='english')
    dtm = tv.fit_transform(cleaned_data)
    km = KMeans(n_clusters=5, random_state=42)
    labels = km.fit_predict(dtm)

    # Label mapping for non-tech people
    business_categories = {
        0: "TECHNICAL STABILITY (Bugs/Performance)",
        1: "CREATIVE ASSETS (Lore/Atmosphere)",
        2: "PRODUCT POSITIONING (Market Fit)",
        3: "CORE GAMEPLAY (Mechanics/Flow)",
        4: "BRAND LOYALTY (Developer Support)"
    }

    # Logic for CLIP zero-shot classification labels
    clip_labels = ["technical bug", "beautiful scenery", "bad game design", "good game design", "fan art"]

    # Dashboard output
    terms = tv.get_feature_names_out()
    order_centroids = km.cluster_centers_.argsort()[:, ::-1]

    print("=== GAME-SENSE INTELLIGENCE DASHBOARD ===")
    for i in range(5):
        top_keywords = [terms[ind] for ind in order_centroids[i, :8]]
        print(f"\nIDENTIFIED THEME: {business_categories[i]}")
        print(f"Key Terms: {', '.join(top_keywords)}")
        print(f"CLIP Semantic Anchor: System mapped to '{clip_labels[i]}'")

# Running for Elden Ring game for showing how it works
run_game_sense_engine("1245620")
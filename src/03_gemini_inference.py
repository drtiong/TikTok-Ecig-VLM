import os
import json
import time
import datetime
import pandas as pd
from google import genai
from google.genai import types

API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    raise ValueError("GEMINI_API_KEY is missing.")

client = genai.Client(api_key=API_KEY)

MODEL_ID = "gemini-3.1-pro-preview"
PROMPT_VERSION = "v1_codebook"
CACHE_INFO_FILE = "cache_info.json"

# Relative Paths
VIDEO_FOLDER = os.path.join("data", "test_run")
CACHE_FOLDER = os.path.join("outputs", "frame_cache")
FINAL_CSV = os.path.join("outputs", f"results_{PROMPT_VERSION}.csv")
JSON_LOG = os.path.join("outputs", f"log_{PROMPT_VERSION}.json")

# ==========================================
# HELPER FUNCTIONS
# ==========================================
def clean_json(text):
    s = text.find('{')
    e = text.rfind('}')
    return text[s:e + 1] if s != -1 else text.strip()

def export_to_csv(results_dict, primary_path):
    rows = []
    cats = ["product_placement", "youth_appeal", "viral_marketing", "event_sponsorship", "localization"]
    for fname, analysis in results_dict.items():
        row = {'filename': fname}
        for cat in cats:
            details = analysis.get(cat, {})
            row[f"{cat}_result"] = details.get('result', 'no')
            row[f"{cat}_reasoning"] = details.get('reasoning', 'No cues.')
        rows.append(row)
    pd.DataFrame(rows).to_csv(primary_path, index=False)

def get_cached_frames(v_name):
    """Loads frames previously saved by 01_frame_extraction.py"""
    c_dir = os.path.join(CACHE_FOLDER, v_name)
    if os.path.exists(c_dir):
        files = sorted([f for f in os.listdir(c_dir) if f.endswith('.jpg')])
        target_files = files[:15]
        return [types.Part.from_bytes(data=open(os.path.join(c_dir, f), 'rb').read(), mime_type='image/jpeg') for f in target_files]
    return []

# ==========================================
# EXECUTION
# ==========================================
# Retrieve Cache Name
cache_name = None
if os.path.exists(CACHE_INFO_FILE):
    with open(CACHE_INFO_FILE, "r") as f:
        cache_name = json.load(f).get("cache_name")
else:
    print("⚠️ No active cache found. Ensure 02_pdf_caching.py ran successfully.")

# Load Resume Log
if os.path.exists(JSON_LOG):
    with open(JSON_LOG, 'r') as f:
        all_results = json.load(f)
else:
    all_results = {}

video_files = [f for f in os.listdir(VIDEO_FOLDER) if f.endswith(('.mp4', '.mov', '.avi'))]
total_videos = len(video_files)
start_run_time = time.time()
processed_count = 0

print("-" * 50)
print(f"🚀 Batch Audit Started at {datetime.datetime.now().strftime('%H:%M:%S')}")
print("-" * 50)

for filename in video_files:
    if filename in all_results:
        processed_count += 1
        continue

    frames = get_cached_frames(filename)
    if not frames:
        print(f"⏭️ Skipping {filename}: No extracted frames found.")
        continue

    retries = 0
    wait = 30
    success = False

    while not success and retries < 5:
        try:
            response = client.models.generate_content(
                model=MODEL_ID,
                contents=frames,
                config=types.GenerateContentConfig(
                    cached_content=cache_name if cache_name else None,
                    response_mime_type="application/json"
                )
            )

            all_results[filename] = json.loads(clean_json(response.text))
            processed_count += 1

            elapsed = time.time() - start_run_time
            avg_time = elapsed / processed_count
            eta = str(datetime.timedelta(seconds=int(avg_time * (total_videos - processed_count))))

            with open(JSON_LOG, 'w') as f:
                json.dump(all_results, f, indent=4)
            export_to_csv(all_results, FINAL_CSV)

            print(f"✅ [{processed_count}/{total_videos}] {filename} | ETA: {eta}")
            success = True
            time.sleep(1)

        except Exception as e:
            retries += 1
            print(f"⚠️ {filename} Attempt {retries} Error: {str(e)[:100]}")
            time.sleep(wait)
            wait *= 2

# Cleanup
if cache_name:
    client.caches.delete(name=cache_name)
    os.remove(CACHE_INFO_FILE)
    print("🗑️ Cache cleared to prevent idle storage fees.")

print("✨ Audit Complete.")
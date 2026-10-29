import os
import time
import json
from google import genai
from google.genai import types

# Load API key securely from environment variables
API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    raise ValueError("GEMINI_API_KEY is missing. Please set it in your environment.")

client = genai.Client(api_key=API_KEY)

MODEL_ID = "gemini-3.1-pro-preview"
CODEBOOK_PATH = os.path.join("data", "final_codebook.pdf")
CACHE_INFO_FILE = "cache_info.json"

# Load prompts from external text files
with open(os.path.join("prompts", "system_instruction.txt"), "r") as f:
    SYSTEM_INSTRUCTION = f.read()

with open(os.path.join("prompts", "user_prompt_schema.json"), "r") as f:
    USER_PROMPT_STATIC = f.read()

print("📄 Initializing Forensic Audit Cache (PDF Mode)...")
if not os.path.exists(CODEBOOK_PATH):
    raise FileNotFoundError(f"🛑 ERROR: PDF not found at {CODEBOOK_PATH}.")

# Upload File
codebook_file = client.files.upload(file=CODEBOOK_PATH)
while codebook_file.state.name == "PROCESSING":
    time.sleep(2)
    codebook_file = client.files.get(name=codebook_file.name)

# Create Cache
try:
    cache = client.caches.create(
        model=MODEL_ID,
        config=types.CreateCachedContentConfig(
            display_name="Annotation Codebook",
            contents=[codebook_file, USER_PROMPT_STATIC],
            system_instruction=SYSTEM_INSTRUCTION,
            ttl="14400s",  # 4 hours
        )
    )
    print(f"✅ Cache Active. Instructions and Codebook are stored in model memory.")

    # Save cache reference for the inference script
    with open(CACHE_INFO_FILE, "w") as f:
        json.dump({"cache_name": cache.name}, f)

except Exception as e:
    print(f"⚠️ Cache failed: {e}")
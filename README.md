# TikTok-Ecig-VLM

Code, prompts and analysis scripts for the Doctor of Public Health (DrPH) thesis "Analyzing E-Cigarette Indirect Promotion Strategies in Malaysia's TikTok Videos" (Universiti Sains Malaysia).

The study evaluates whether the vision-language model Gemini 3.1 Pro (`gemini-3.1-pro-preview`) can classify five indirect e-cigarette promotion strategies (product placement, youth appeal, viral marketing, localization and event sponsorship) from sampled video frames, using manual content analysis with a structured codebook as the reference standard. The model is validated on 400 human-annotated videos and then deployed on 787 further videos, with misclassification-adjusted prevalence estimates and targeted human verification of flagged videos.

Manuscript: [add citation / DOI / preprint link when available]

## Data availability

The raw videos are **not** shared. They were collected from TikTok and contain third-party audiovisual content, including potentially identifiable individuals, so redistribution raises privacy, copyright and platform-governance concerns, and conventional de-identification cannot reliably remove the risk of re-identification from audiovisual content.

This repository provides two kinds of data:

| Tier | Location | Content | Purpose |
|---|---|---|---|
| 1. Synthetic pipeline test | `data/`, `output/` | A royalty-free dummy video, synthetic human labels and sample outputs | Check that the Python pipeline (`src/`) runs end to end. Results are **not** the study results. |
| 2. Study label-level data | `analysis/data/` | De-identified labels only (no video, frames or URLs): pseudonymised creator ID, human reference labels and Gemini labels | Reproduce the statistical analysis and manuscript tables in `analysis/`. |

Creator identifiers in Tier 2 are pseudonymised. The pseudonymisation key is not shared.

## Repository structure

```text
TikTok-Ecig-VLM/
├── README.md
├── LICENSE                          # add (see "License")
├── CITATION.cff                     # add
├── requirements.txt                 # Python dependencies
├── .env.example                     # copy to .env and add your API key
├── .gitignore
│
├── codebook/
│   └── final_codebook.pdf           # operational criteria; uploaded to the Gemini cache
│
├── prompt/
│   ├── system_instruction.txt
│   └── user_prompt_schema.json
│
├── src/                             # Stage 1: Python pipeline (run in order)
│   ├── 01_frame_extraction.py
│   ├── 02_pdf_caching.py
│   └── 03_gemini_inference.py
│
├── data/                            # Tier 1: synthetic inputs only
│   ├── test_run/
│   │   └── dummy_video.mp4
│   └── sample_ground_truth.csv
│
├── output/                          # Tier 1: generated sample outputs
│   ├── frame_cache/                 # created by 01_frame_extraction.py
│   ├── sample_gemini_results.csv
│   ├── sample_diagnostic_performance_report.csv
│   └── sample_discrepant_cases_for_analysis.csv
│
└── analysis/                        # Stage 2: R statistical analysis (see analysis/README.md)
    ├── README.md
    ├── R/
    └── data/                        # Tier 2: study label-level CSVs
```

## Setup

**1. Clone the repository**

```bash
git clone https://github.com/drtiong/TikTok-Ecig-VLM.git
cd TikTok-Ecig-VLM
```

**2. Python (Stage 1).** Python 3.9 or later and the Google GenAI SDK.

```bash
pip install -r requirements.txt
```

**3. API credentials.** Copy `.env.example` to `.env` and add your Gemini API key. Never commit `.env`.

```bash
cp .env.example .env
```
```text
GEMINI_API_KEY="your_api_key_here"
```

**4. R (Stage 2).** R and JAGS are needed only for `analysis/`. Versions used: R (version 4.6.1), JAGS (version 4.3.2). See `analysis/README.md`.

## Stage 1: Python pipeline

Run from the repository root, in order.

| Step | Script | Input | Output |
|---|---|---|---|
| 1 | `src/01_frame_extraction.py` | videos (e.g. `data/test_run/`) | `output/frame_cache/` |
| 2 | `src/02_pdf_caching.py` | `codebook/final_codebook.pdf`, `prompt/system_instruction.txt` | Gemini context cache |
| 3 | `src/03_gemini_inference.py` | frames in `output/frame_cache/`, `prompt/user_prompt_schema.json` | `output/sample_gemini_results.csv` |

**01_frame_extraction.py.** Hybrid temporal sampling for short-form video. Frames are always taken at 0.5 s, 1.5 s and 2.5 s to capture rapid visual hooks (for example stitches and quick brand flashes). Remaining frames are spaced at mathematically even intervals across the entire duration of the video. The total is capped at 15 RGB frames per video (720 × 1280) to limit API payload size. Frames are therefore sparser in longer videos.

**02_pdf_caching.py.** Uploads `codebook/final_codebook.pdf` and stores it in the Gemini model cache together with the system instruction. This avoids reprocessing the codebook for every video and keeps the operational definitions fixed during batch inference.

**03_gemini_inference.py.** Reads the extracted frames from `output/frame_cache/`, calls the cached model and requests structured JSON output for the five strategies. [CONFIRM and edit: the manuscript states that the schema allowed an "unsure" option for each strategy; state here the permitted values and how "unsure" is handled downstream, because the R analysis expects yes/no.] Raw decisions are written to a CSV log.


Synthetic data verify that the code runs. They will not reproduce the manuscript numbers.

## Stage 2: Statistical analysis (R)

Validation metrics, majority-class baseline, creator clustering, Bayesian misclassification adjustment and targeted verification are in [`analysis/`](analysis/README.md). The README there lists the scripts, inputs and the manuscript table or figure each script produces.

## Responsible use

This is research code. Model flags are intended to prioritise videos for human review, not to make enforcement decisions on their own. Results apply to Gemini 3.1 Pro with this codebook and prompt, on Malaysian TikTok videos.


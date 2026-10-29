import os
import pandas as pd
import numpy as np
from sklearn.metrics import confusion_matrix
from statsmodels.stats.proportion import proportion_confint

# ==========================================
# 1. FILE CONFIGURATION
# ==========================================
# Use relative paths so it runs out-of-the-box for other researchers
GROUND_TRUTH_CSV = os.path.join("data", "ground_truth.csv")
MODEL_RESULTS_CSV = os.path.join("outputs", "results_v1_codebook.csv")

METRICS_OUTPUT_CSV = os.path.join("outputs", "diagnostic_report_with_CI.csv")
DISCREPANCY_OUTPUT_CSV = os.path.join("outputs", "discrepant_cases_for_analysis.csv")

STRATEGIES = ["product_placement", "youth_appeal", "viral_marketing", "event_sponsorship", "localization"]

# ==========================================
# 2. DATA LOADING & AGGRESSIVE NORMALIZATION
# ==========================================
print("📊 Loading datasets for validation analysis...")
df_gt = pd.read_csv(GROUND_TRUTH_CSV)
df_model = pd.read_csv(MODEL_RESULTS_CSV)

# Force rename first columns to 'filename' safely
if not df_gt.empty:
    df_gt.rename(columns={df_gt.columns[0]: 'filename'}, inplace=True)
if not df_model.empty:
    df_model.rename(columns={df_model.columns[0]: 'filename'}, inplace=True)


def ultra_clean_filename(series):
    """
    Strips extensions, removes trailing decimals (.0), eliminates leading zeros,
    and forces lowercase to guarantee a perfect match across datasets.
    """
    return (series.astype(str)
            .str.replace(r'[\r\n\t]', '', regex=True)
            .str.strip()
            .str.lower()
            .str.replace(r'\.(mp4|mov|avi|jpeg|jpg)$', '', regex=True)
            .str.replace(r'\.0$', '', regex=True)
            .str.lstrip('0'))


df_gt['match_id'] = ultra_clean_filename(df_gt['filename'])
df_model['match_id'] = ultra_clean_filename(df_model['filename'])

# Merge dataframes
merged_df = pd.merge(df_gt, df_model, on='match_id', suffixes=('_gt', '_model'))

# --- PROTECTION & DIAGNOSTIC INTERCEPTOR ---
if merged_df.empty:
    print("\n❌ ERROR: Matched 0 videos! Python cannot join your files on 'filename'.")
    print("=" * 80)
    print("DIAGNOSTIC COMPARISON (First 5 keys after ultra-cleaning):")
    print(f"Ground Truth Clean IDs: {df_gt['match_id'].dropna().head(5).tolist()}")
    print(f"Model Results Clean IDs: {df_model['match_id'].dropna().head(5).tolist()}")
    print("=" * 80)
    print("Please inspect the printed formats above to see where they drift apart.")
    exit()

print(f"✅ Successfully matched {len(merged_df)} videos for verification evaluation.\n")

if 'filename_gt' in merged_df.columns:
    merged_df['filename'] = merged_df['filename_gt']

# ==========================================
# 3. METRICS PROCESSING & DISCREPANCY TRACKING
# ==========================================
performance_report = []
discrepant_rows = []

for strategy in STRATEGIES:
    possible_gt_cols = [strategy, f"{strategy}_gt", f"{strategy}_result_gt", f"{strategy}_result"]
    possible_model_cols = [f"{strategy}_result", f"{strategy}_model"]

    gt_col = next((c for c in possible_gt_cols if c in merged_df.columns and (c.endswith('_gt') or c == strategy)),
                  None)
    if not gt_col:
        gt_col = next((c for c in possible_gt_cols if c in merged_df.columns), None)

    model_col = next((c for c in possible_model_cols if
                      c in merged_df.columns and (c.endswith('_model') or c == f"{strategy}_result")), None)
    if not model_col:
        model_col = next((c for c in possible_model_cols if c in merged_df.columns), None)

    reason_col = f"{strategy}_reasoning"

    if not gt_col or not model_col:
        print(f"⚠️ Skipping strategy '{strategy}': Could not map headers correctly.")
        continue

    # Define the raw data
    y_true_raw = merged_df[gt_col].astype(str).str.strip().str.lower()
    y_pred_raw = merged_df[model_col].astype(str).str.strip().str.lower()

    # Binary mapping
    y_true_binary = y_true_raw.map({'no': 0, 'yes': 1}).fillna(0).astype(int)
    y_pred_binary = y_pred_raw.map({'no': 0, 'yes': 1}).fillna(0).astype(int)

    # Confusion matrix calculation
    try:
        tn, fp, fn, tp = confusion_matrix(y_true_binary, y_pred_binary, labels=[0, 1]).ravel()
    except ValueError:
        tn, fp, fn, tp = 0, 0, 0, 0

    # CI & Metrics calculations
    sensitivity = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    specificity = tn / (tn + fp) if (tn + fp) > 0 else 0.0

    sens_ci = proportion_confint(count=tp, nobs=tp + fn, alpha=0.05, method='wilson') if (tp + fn) > 0 else (0.0, 0.0)
    spec_ci = proportion_confint(count=tn, nobs=tn + fp, alpha=0.05, method='wilson') if (tn + fp) > 0 else (0.0, 0.0)

    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    f1_score = 2 * (precision * sensitivity) / (precision + sensitivity) if (precision + sensitivity) > 0 else 0.0

    performance_report.append({
        "Strategy Domain": strategy.replace("_", " ").title(),
        "TN": tn, "FP": fp, "FN": fn, "TP": tp,
        "Sensitivity": round(sensitivity, 3),
        "Sens_Lower_95CI": round(sens_ci[0], 3),
        "Sens_Upper_95CI": round(sens_ci[1], 3),
        "Specificity": round(specificity, 3),
        "Spec_Lower_95CI": round(spec_ci[0], 3),
        "Spec_Upper_95CI": round(spec_ci[1], 3),
        "Precision": round(precision, 3),
        "F1-Score": round(f1_score, 3),
    })

    # Catch Discrepancies
    mismatches = merged_df[y_true_binary != y_pred_binary]
    for _, row in mismatches.iterrows():
        gt_val = row[gt_col]
        model_val = row[model_col]

        # Determine Error Type
        if str(gt_val).lower() == 'yes' and str(model_val).lower() == 'no':
            error_type = 'False Negative'
        elif str(gt_val).lower() == 'no' and str(model_val).lower() == 'yes':
            error_type = 'False Positive'
        else:
            error_type = 'Mismatch'

        discrepant_rows.append({
            "Filename": row['filename'],
            "Failed Strategy": strategy,
            "Error Type": error_type,
            "Ground Truth (Human)": gt_val,
            "Gemini Model Result": model_val,
            "Model Reasoning": row.get(reason_col, "N/A")
        })

# ==========================================
# 4. EXPORT
# ==========================================
os.makedirs("outputs", exist_ok=True)

df_report = pd.DataFrame(performance_report)
df_report.to_csv(METRICS_OUTPUT_CSV, index=False)

print("=" * 100)
print("🎯 FINAL DIAGNOSTIC VALIDATION REPORT")
print("=" * 100)
print(df_report.to_string(index=False))
print("=" * 100)

if discrepant_rows:
    df_discrepancies = pd.DataFrame(discrepant_rows)
    df_discrepancies.sort_values(by=["Failed Strategy", "Filename"], inplace=True)
    df_discrepancies.to_csv(DISCREPANCY_OUTPUT_CSV, index=False)
    print(f"🚨 Discrepancies logged: {len(df_discrepancies)} rows exported to:\n   {DISCREPANCY_OUTPUT_CSV}\n")
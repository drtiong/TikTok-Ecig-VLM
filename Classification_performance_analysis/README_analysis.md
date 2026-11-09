# Analysis code: Gemini 3.1 Pro classification of indirect e-cigarette promotion

R code for the validation, deployment and sensitivity analyses of the thesis that evaluates Gemini 3.1 Pro against a human reference standard (five strategies, Malaysian TikTok videos; validation n = 400, deployment n = 787).

All commands below are run **from this folder** (`analysis/`):

```bash
cd analysis
```

## Setup

- R (version 4.6.1) and JAGS (version 4.3.2) (JAGS is installed separately from R).
- Install packages: `Rscript R/install_packages.R`
- Seeds are fixed in the scripts: bootstrap 2026 (5,000 resamples; 2,000 for ICC), Bayesian model 42.

## Layout

```text
analysis/
├── R/
│   ├── install_packages.R
│   ├── utils.R                            shared helpers (sourced, not run)
│   ├── validation_metrics.R
│   ├── majority_baseline_comparison.R
│   ├── deployment_clustering.R
│   ├── deployment_bayesian_adjustment.R
│   └──targeted_verification.R
│   
├── data/                                  input CSVs (study label-level data)
│   ├── validation_400.csv
│   ├── deployment_787.csv
│   └── double_coding_400.csv              optional
└── output/                                results (created by the scripts)
```

## Run

```bash
Rscript R/install_packages.R
```

Or one script at a time, for example:

```bash
Rscript R/deployment_bayesian_adjustment.R data/deployment_787.csv data/validation_400.csv output
```

`R/00_pseudonymise_creators.R` replaces creator URLs with pseudonymous IDs. It needs the raw file `data/validation_400_raw.csv`, which is not shared, so skip it: the supplied files are already pseudonymised. The pseudonymisation key is never published.

## Input files

| File | Columns |
|---|---|
| `data/validation_400.csv` | `filename`, `creator_id`, then for each strategy `<strategy>` (human reference) and `<strategy>_gemini`, values yes/no |
| `data/deployment_787.csv` | `filename`, `creator_id`, `<strategy>_gemini` for all five strategies; `<strategy>` human labels only for verified videos (blank otherwise). Rows without Gemini labels are dropped |
| `data/double_coding_400.csv` (optional) | `filename`, `creator_id`, `<strategy>_coder1`, `<strategy>_coder2` (pre-consensus labels) |

Strategies: `product_placement`, `youth_appeal`, `viral_marketing`, `event_sponsorship`, `localization`.

## Scripts and manuscript items

| Script | Produces |
|---|---|
| `validation_metrics.R` | Validation metrics with creator-cluster CIs, 2×2 tables, heatmaps 
| `majority_baseline_comparison.R` | Model versus "label every video with the majority reference label": accuracy, F1, MCC and error counts |
| `deployment_clustering.R` | Creators per video, ICC, design effects, crude proportions with cluster CIs |
| `deployment_bayesian_adjustment.R` | Crude versus Bayesian-adjusted prevalence 
| `targeted_verification.R` | PPV of flagged videos for the less common strategies
| `supplementary_table_S1.R` | Supplementary Table S1: clustering in validation and deployment |



## Methods in brief

- **Validation metrics:** sensitivity, specificity, PPV, NPV, accuracy, F1 (positive class) and MCC. Primary 95% CIs are creator-cluster percentile bootstrap intervals (5,000 resamples, seed 2026). [CONFIRM: Wilson intervals are used only in `deployment_clustering.R` and Supplementary Table S1 as the independent-video comparator. If `validation_metrics.R` still outputs Wilson intervals for validation metrics, say so here or remove them from the script.]
- **Majority-class baseline:** a classifier labelling every video with the majority reference label, used where a strategy is positive in over 90% of validation videos. Accuracy and F1 are compared with paired creator-cluster bootstrap differences; NPV and MCC indicate whether the model adds information beyond the baseline.
- **Clustering:** ICC by one-way ANOVA with a creator-bootstrap CI (unstable when the rarer label sits mostly in creators with a single video). Design effect 1 + (m_w − 1) × ICC, where m_w is the size-weighted mean cluster size. The empirical design effect is (cluster CI width ÷ Wilson CI width)², minimum 1, and effective n = N ÷ empirical design effect.
- **Bayesian misclassification adjustment (primary):** `prevalence::truePrev` (JAGS). Beta(1,1) prior on true prevalence; sensitivity ~ Beta(TP+1, FN+1) and specificity ~ Beta(TN+1, FP+1) from the validation counts; 2 chains, 10,000 burn-in and 10,000 retained iterations; seed 42. The adjustment assumes validation sensitivity and specificity apply in deployment, so adjusted estimates are indicative.
- **Targeted verification:** only Gemini-positive videos were reviewed, so only PPV can be estimated. Sensitivity, specificity, NPV, F1, MCC and accuracy are not estimable at this stage, and the results are not used in the Bayesian adjustment. PPV intervals are creator-cluster bootstrap intervals. [If expected PPV is dropped from the manuscript, delete that check from `targeted_verification.R`.]

## Notes

- Raw videos, frames, creator URLs and the pseudonymisation key are not shared.

# Gemini 3.1 Pro classification of indirect e-cigarette promotion: analysis code

R code for the validation, deployment and sensitivity analyses of a thesis evaluating
Gemini 3.1 Pro against a human reference standard (five strategies, Malaysian TikTok
videos; validation n = 400, deployment n = 787).

## Repository layout

```
R/
  utils.R                              shared helpers (sourced, not run)
  install_packages.R                   installs R packages
  validation_metrics.R                 7 metrics + creator-cluster CIs, ICC, 2x2 tables, heatmaps
  majority_baseline_comparison.R       model vs 'label every video positive' (accuracy, F1, MCC), error counts
  deployment_bayesian_adjustment.R     crude (cluster-bootstrap CI) vs Bayesian-adjusted, figure
  targeted_verification.R              PPV of flagged rare-strategy videos, expected-PPV check
  deployment_clustering.R              standalone creator distribution / ICC for the deployment corpus
data/                                  input CSVs (not tracked)
output/                                results (not tracked)
```

Run every script **from the repository root**.

```bash
Rscript R/install_packages.R          # JAGS must be installed separately
Rscript R/00_pseudonymise_creators.R data/validation_400_raw.csv data/validation_400.csv
Rscript R/run_all.R
```

or one at a time, for example
`Rscript R/deployment_bayesian_adjustment.R data/deployment_787.csv data/validation_400.csv output`.

## Input files

| File | Columns |
|---|---|
| `data/validation_400.csv` | `filename`, `creator_id` , then for each strategy `<strategy>` (human reference) and `<strategy>_gemini`, values yes/no |
| `data/deployment_787.csv` | `filename`, `creator_id`, `<strategy>_gemini` for all five strategies; `<strategy>` human labels only for the verified videos (blank otherwise). Rows without Gemini labels are dropped |
| `data/double_coding_400.csv` (optional) | `filename`, `creator_id`, `<strategy>_coder1`, `<strategy>_coder2` (pre-consensus labels) |

Strategies: `product_placement`, `youth_appeal`, `viral_marketing`, `event_sponsorship`, `localization`.

## Methods in brief

- **Validation metrics:** sensitivity, specificity, PPV, NPV, accuracy,
  F1, MCC. Primary 95% CI: creator-cluster
  percentile bootstrap (5,000 resamples, seed 2026). Wilson intervals (independent videos)
  for the five proportions are shown for comparison.
- **Clustering:** ICC by one-way ANOVA; design effect 1 + (m_w - 1) x ICC with the
  size-weighted mean cluster size m_w; empirical deployment design effect =
  (cluster CI width / Wilson CI width)^2.
- **Bayesian adjustment (primary):** `prevalence::truePrev`; Beta(1,1) prior on true
  prevalence; sensitivity ~ Beta(TP+1, FN+1), specificity ~ Beta(TN+1, FP+1) from the
  validation counts; 2 chains, 10,000 burn-in + 10,000 retained iterations; seed 42.
- **Targeted verification:** only Gemini-positive videos were reviewed, so the metric is
  PPV. Sensitivity, specificity, NPV, F1, MCC and accuracy are not estimable at that stage.





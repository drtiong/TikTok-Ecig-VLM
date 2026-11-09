#!/usr/bin/env Rscript
# ==============================================================================
# Model versus a "label every video positive" classifier (majority-class
# baseline), for each strategy. Shows, for youth appeal in particular, that a
# precise positive label (high precision) can sit beside a weak negative label
# (low NPV, modest MCC) and accuracy / F1 that are BELOW the trivial baseline.
#
# Usage: Rscript R/majority_baseline_comparison.R [validation.csv] [out_dir]
#
# Outputs (out_dir):
#   baseline_comparison_metrics.csv   model and baseline metrics, creator-cluster 95% CIs
#   baseline_comparison_paired.csv    model minus baseline (accuracy, F1), cluster 95% CI
#   youth_appeal_error_counts.csv     false negatives / false positives / correct "no" calls
#
# Baseline definitions (classifier that labels every video with the more common
# reference label; for strategies positive in > 50% of videos this is "all yes"):
#   accuracy = prevalence; F1 = 2p / (1 + p); MCC = 0 (undefined, set to 0);
#   NPV and specificity are undefined / 0 because it never says "no".
# ==============================================================================
.args <- commandArgs(FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.dir  <- if (length(.file) > 0) dirname(normalizePath(.file[1])) else "R"
source(file.path(.dir, "utils.R"))

val_path <- cli_arg(1, "data/validation_400.csv")
out_dir  <- cli_arg(2, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
B <- 5000; SEED <- 2026

val <- read_validation(val_path)
idx <- split(seq_len(nrow(val)), val$cid); K <- length(idx)
cat("Videos:", nrow(val), " Creators:", K, "\n")

safe <- function(a, b) if (b == 0) NA_real_ else a / b

# Model metrics and always-yes baseline metrics from reference (t) and model (p)
both_metrics <- function(t, p) {
  tp <- sum(t == 1 & p == 1); fp <- sum(t == 0 & p == 1)
  fn <- sum(t == 1 & p == 0); tn <- sum(t == 0 & p == 0); n <- length(t)
  den <- sqrt(as.numeric(tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  prev <- mean(t == 1)
  c(model_precision   = safe(tp, tp + fp),
    model_npv         = safe(tn, tn + fn),
    model_accuracy    = (tp + tn) / n,
    model_f1          = safe(2 * tp, 2 * tp + fp + fn),
    model_mcc         = if (den == 0) NA_real_ else (tp * tn - fp * fn) / den,
    base_accuracy     = prev,                      # all-yes accuracy = prevalence
    base_f1           = 2 * prev / (1 + prev),     # all-yes F1
    base_mcc          = 0)                         # no information
}

rows <- list(); pair <- list(); err <- list()
set.seed(SEED)
for (nm in names(STRATEGIES)) {
  s <- STRATEGIES[[nm]]
  t <- val[[s]]; p <- val[[paste0(s, "_gemini")]]
  est <- both_metrics(t, p)

  boot <- t(vapply(seq_len(B), function(b) {
    i <- unlist(idx[sample.int(K, K, replace = TRUE)], use.names = FALSE)
    both_metrics(t[i], p[i])
  }, numeric(length(est))))
  ci <- apply(boot, 2, function(v) quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE))

  rows[[nm]] <- data.frame(Strategy = nm, Metric = names(est), Estimate = unname(est),
                           Cluster_lo = ci[1, ], Cluster_hi = ci[2, ],
                           row.names = NULL)

  d_acc <- boot[, "model_accuracy"] - boot[, "base_accuracy"]
  d_f1  <- boot[, "model_f1"]       - boot[, "base_f1"]
  pair[[nm]] <- data.frame(
    Strategy = nm, Reference_prevalence = mean(t == 1),
    Accuracy_diff = est[["model_accuracy"]] - est[["base_accuracy"]],
    Accuracy_diff_lo = quantile(d_acc, 0.025, names = FALSE),
    Accuracy_diff_hi = quantile(d_acc, 0.975, names = FALSE),
    F1_diff = est[["model_f1"]] - est[["base_f1"]],
    F1_diff_lo = quantile(d_f1, 0.025, na.rm = TRUE, names = FALSE),
    F1_diff_hi = quantile(d_f1, 0.975, na.rm = TRUE, names = FALSE),
    Model_below_baseline_accuracy = est[["model_accuracy"]] < est[["base_accuracy"]],
    Model_below_baseline_F1       = est[["model_f1"]] < est[["base_f1"]],
    row.names = NULL)

  err[[nm]] <- data.frame(
    Strategy = nm,
    Errors = sum(t != p),
    False_negatives = sum(t == 1 & p == 0),
    False_positives = sum(t == 0 & p == 1),
    Model_no_calls = sum(p == 0),
    Model_no_calls_correct = sum(t == 0 & p == 0),
    row.names = NULL)
}

metrics <- do.call(rbind, rows); pair <- do.call(rbind, pair); err <- do.call(rbind, err)
rownames(metrics) <- rownames(pair) <- rownames(err) <- NULL
write.csv(metrics, file.path(out_dir, "baseline_comparison_metrics.csv"), row.names = FALSE)
write.csv(pair,    file.path(out_dir, "baseline_comparison_paired.csv"),  row.names = FALSE)
write.csv(err,     file.path(out_dir, "youth_appeal_error_counts.csv"),   row.names = FALSE)

# ---- Console summary: the youth appeal story ---------------------------------
ya <- metrics[metrics$Strategy == "Youth appeal", ]
cat("\nYOUTH APPEAL (model vs 'label every video positive')\n")
print(ya, row.names = FALSE, digits = 3)
cat("\nPaired differences (model minus baseline), creator-cluster 95% CI:\n")
print(pair, row.names = FALSE, digits = 3)
cat("\nWhere the errors are:\n"); print(err, row.names = FALSE)
cat("\nDone. Outputs in", normalizePath(out_dir), "\n")

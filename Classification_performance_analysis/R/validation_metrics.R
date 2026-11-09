suppressPackageStartupMessages({
  library(ggplot2)
  library(binom)
  library(scales)
})

# ------------------------------------------------------------------------------
# 0. Settings
# ------------------------------------------------------------------------------
args       <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[1] else "data/validation_400.csv"
out_dir    <- if (length(args) >= 2) args[2] else "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

SEED      <- 2026
B_METRICS <- 5000   # bootstrap resamples for metric CIs
B_ICC     <- 2000   # bootstrap resamples for ICC CIs
set.seed(SEED)

strategies <- c(
  "Product placement" = "product_placement",
  "Youth appeal"      = "youth_appeal",
  "Viral marketing"   = "viral_marketing",
  "Event sponsorship" = "event_sponsorship",
  "Localization"      = "localization"
)

metric_names <- c(
  "Sensitivity", "Specificity", "Precision (PPV)", "NPV", "Accuracy",
  "F1", "MCC"
)

# Metrics that are simple proportions (Wilson interval applies)
wilson_metrics <- c("Sensitivity", "Specificity", "Precision (PPV)", "NPV", "Accuracy")

# ------------------------------------------------------------------------------
# 1. Read and validate data
# ------------------------------------------------------------------------------
dat <- read.csv(input_file, fileEncoding = "UTF-8-BOM",
                stringsAsFactors = FALSE, check.names = FALSE)

if ("creator_id" %in% names(dat)) {
  dat$cid <- tolower(trimws(as.character(dat$creator_id)))
} else if ("creator" %in% names(dat)) {
  # keep only the handle: drops "/video/..." suffixes, standardises case
  dat$cid <- tolower(sub(".*tiktok\\.com/@([^/?#]+).*", "\\1",
                         trimws(as.character(dat$creator))))
} else {
  stop("Input needs a 'creator' (URL) or 'creator_id' column.")
}
if (anyNA(dat$cid) || any(dat$cid == "")) stop("Missing creator identifiers.")

to_bin <- function(x, label) {
  v <- tolower(trimws(as.character(x)))
  if (!all(v %in% c("yes", "no"))) {
    stop("Column '", label, "' has values other than yes/no: ",
         paste(unique(v[!v %in% c("yes", "no")]), collapse = ", "))
  }
  as.integer(v == "yes")
}

for (s in strategies) {
  for (col in c(s, paste0(s, "_gemini"))) {
    if (!col %in% names(dat)) stop("Missing column: ", col)
    dat[[col]] <- to_bin(dat[[col]], col)
  }
}

n_videos   <- nrow(dat)
n_creators <- length(unique(dat$cid))
m_bar      <- n_videos / n_creators
m_w        <- sum(table(dat$cid)^2) / sum(table(dat$cid))   # size-weighted mean cluster size
cat("Videos:", n_videos, " Creators:", n_creators,
    " Mean videos/creator:", round(m_bar, 3),
    " Max videos/creator:", max(table(dat$cid)), "\n")

idx_by_creator <- split(seq_len(n_videos), dat$cid)
K <- length(idx_by_creator)

# ------------------------------------------------------------------------------
# 2. Metric functions
# ------------------------------------------------------------------------------
safe_div <- function(a, b) if (b == 0) NA_real_ else a / b

cell_counts <- function(truth, pred) {
  c(tp = sum(truth == 1 & pred == 1), fp = sum(truth == 0 & pred == 1),
    fn = sum(truth == 1 & pred == 0), tn = sum(truth == 0 & pred == 0))
}

calc_metrics <- function(truth, pred) {
  cc <- cell_counts(truth, pred)
  tp <- as.numeric(cc["tp"]); fp <- as.numeric(cc["fp"])
  fn <- as.numeric(cc["fn"]); tn <- as.numeric(cc["tn"])
  n  <- tp + fp + fn + tn

  se <- safe_div(tp, tp + fn)
  sp <- safe_div(tn, tn + fp)

  mcc_den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

  c(
    "Sensitivity"     = se,
    "Specificity"     = sp,
    "Precision (PPV)" = safe_div(tp, tp + fp),
    "NPV"             = safe_div(tn, tn + fn),
    "Accuracy"        = (tp + tn) / n,
    "F1"              = safe_div(2 * tp, 2 * tp + fp + fn),   # positive class
    "MCC"             = if (mcc_den == 0) NA_real_ else (tp * tn - fp * fn) / mcc_den
  )
}

wilson_ci <- function(x, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  r <- binom::binom.confint(x = x, n = n, conf.level = 0.95, methods = "wilson")
  c(r$lower, r$upper)
}

wilson_for <- function(metric, cc) {
  tp <- cc["tp"]; fp <- cc["fp"]; fn <- cc["fn"]; tn <- cc["tn"]
  switch(metric,
    "Sensitivity" = wilson_ci(tp, tp + fn),
    "Specificity" = wilson_ci(tn, tn + fp),
    "Precision (PPV)" = wilson_ci(tp, tp + fp),
    "NPV"         = wilson_ci(tn, tn + fn),
    "Accuracy"    = wilson_ci(tp + tn, tp + fp + fn + tn),
    c(NA_real_, NA_real_)
  )
}

# Creator-cluster bootstrap: resample creators with replacement, keep all of
# each sampled creator's videos, recompute every metric.
cluster_bootstrap <- function(truth, pred, B) {
  boot <- t(vapply(seq_len(B), function(b) {
    s <- sample.int(K, K, replace = TRUE)
    i <- unlist(idx_by_creator[s], use.names = FALSE)
    calc_metrics(truth[i], pred[i])
  }, numeric(length(metric_names))))
  colnames(boot) <- metric_names
  boot
}

# One-way ANOVA estimator of the intracluster correlation (binary outcome)
icc_anova <- function(y, g) {
  g  <- as.integer(factor(g))
  k  <- max(g); N <- length(y)
  ni <- tabulate(g, nbins = k)
  means <- as.numeric(tapply(y, g, mean))
  gm    <- mean(y)
  ssb   <- sum(ni * (means - gm)^2)
  ssw   <- sum((y - means[g])^2)
  msb   <- ssb / (k - 1)
  msw   <- ssw / (N - k)
  n0    <- (N - sum(ni^2) / N) / (k - 1)
  denom <- msb + (n0 - 1) * msw
  if (denom == 0) return(NA_real_)
  (msb - msw) / denom
}

icc_cluster_boot <- function(y, B) {
  vals <- vapply(seq_len(B), function(b) {
    s <- sample.int(K, K, replace = TRUE)
    yy <- unlist(lapply(idx_by_creator[s], function(i) y[i]), use.names = FALSE)
    gg <- rep(seq_along(s), times = lengths(idx_by_creator[s]))
    icc_anova(yy, gg)
  }, numeric(1))
  quantile(vals, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
}

# ------------------------------------------------------------------------------
# 3. Run all strategies
# ------------------------------------------------------------------------------
res_list <- list(); tab_list <- list(); icc_list <- list()

for (sname in names(strategies)) {
  col   <- strategies[[sname]]
  truth <- dat[[col]]
  pred  <- dat[[paste0(col, "_gemini")]]
  cat("Processing:", sname, "\n")

  cc  <- cell_counts(truth, pred)
  est <- calc_metrics(truth, pred)

  tab_list[[sname]] <- data.frame(
    Strategy = sname, TP = cc["tp"], FP = cc["fp"], FN = cc["fn"], TN = cc["tn"],
    Reference_positive = cc["tp"] + cc["fn"], Reference_negative = cc["tn"] + cc["fp"],
    Reference_prevalence = (cc["tp"] + cc["fn"]) / n_videos,
    Majority_class_accuracy = max(cc["tp"] + cc["fn"], cc["tn"] + cc["fp"]) / n_videos,
    Always_yes_F1 = 2 * (cc["tp"] + cc["fn"]) / (n_videos + (cc["tp"] + cc["fn"])),
    row.names = NULL
  )

  boot <- cluster_bootstrap(truth, pred, B_METRICS)
  ci   <- apply(boot, 2, function(v) {
    if (all(is.na(v))) c(NA_real_, NA_real_)
    else quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  })

  rows <- lapply(metric_names, function(m) {
    w <- wilson_for(m, cc)
    data.frame(
      Strategy = sname, Metric = m, Estimate = unname(est[m]),
      Cluster_lo = ci[1, m], Cluster_hi = ci[2, m],
      Wilson_lo = w[1], Wilson_hi = w[2],
      Undefined_replicates = sum(is.na(boot[, m])),
      row.names = NULL
    )
  })
  res_list[[sname]] <- do.call(rbind, rows)

  icc_pt <- icc_anova(truth, dat$cid)
  icc_ci <- icc_cluster_boot(truth, B_ICC)
  err    <- as.integer(truth != pred)
  icc_er <- icc_anova(err, dat$cid)
  icc_list[[sname]] <- data.frame(
    Strategy = sname,
    ICC_reference = icc_pt, ICC_lo = icc_ci[1], ICC_hi = icc_ci[2],
    Design_effect = 1 + (m_w - 1) * max(icc_pt, 0),
    ICC_error_indicator = icc_er,
    Design_effect_error = 1 + (m_w - 1) * max(icc_er, 0),
    row.names = NULL
  )
}

res <- do.call(rbind, res_list); rownames(res) <- NULL
tab <- do.call(rbind, tab_list); rownames(tab) <- NULL
icc <- do.call(rbind, icc_list); rownames(icc) <- NULL

# ------------------------------------------------------------------------------
# 4. Consistency check against the confusion matrices used for the Bayesian
#    misclassification adjustment (edit if your final matrices change)
# ------------------------------------------------------------------------------
expected <- data.frame(
  Strategy = names(strategies),
  TN = c(7, 32, 284, 352, 337), FP = c(2, 6, 12, 1, 6),
  FN = c(1, 55, 33, 4, 24),     TP = c(390, 307, 71, 43, 33)
)
chk <- merge(tab[, c("Strategy", "TN", "FP", "FN", "TP")], expected,
             by = "Strategy", suffixes = c("", "_expected"))
if (any(chk$TN != chk$TN_expected | chk$FP != chk$FP_expected |
        chk$FN != chk$FN_expected | chk$TP != chk$TP_expected)) {
  warning("Confusion matrices differ from those used in the Bayesian adjustment script!")
} else {
  cat("Check passed: 2x2 tables match the Bayesian adjustment inputs.\n")
}

# ------------------------------------------------------------------------------
# 5. Export tables
# ------------------------------------------------------------------------------
write.csv(tab, file.path(out_dir, "validation_2x2_tables.csv"), row.names = FALSE)
write.csv(res, file.path(out_dir, "validation_metrics_long.csv"), row.names = FALSE)
write.csv(icc, file.path(out_dir, "icc_by_strategy.csv"), row.names = FALSE)

fmt <- function(e, lo, hi) ifelse(is.na(e), "NA", sprintf("%.3f (%.3f-%.3f)", e, lo, hi))
res$Primary <- fmt(res$Estimate, res$Cluster_lo, res$Cluster_hi)
wide <- reshape(res[, c("Strategy", "Metric", "Primary")],
                idvar = "Metric", timevar = "Strategy", direction = "wide")
names(wide) <- sub("^Primary\\.", "", names(wide))
wide <- wide[match(metric_names, wide$Metric), c("Metric", names(strategies))]
write.csv(wide, file.path(out_dir, "validation_metrics_primary_table.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 6. Heatmaps (estimates; optional creator-cluster CI in each cell)
# ------------------------------------------------------------------------------
make_heatmap <- function(show_ci) {
  h <- res
  h$Metric   <- factor(h$Metric, levels = metric_names)
  h$Strategy <- factor(h$Strategy, levels = rev(names(strategies)))
  h$label <- if (show_ci) {
    ifelse(is.na(h$Estimate), "NA",
           sprintf("%.2f\n(%.2f-%.2f)", h$Estimate, h$Cluster_lo, h$Cluster_hi))
  } else {
    ifelse(is.na(h$Estimate), "NA", sprintf("%.2f", h$Estimate))
  }
  h$txt_col <- ifelse(!is.na(h$Estimate) & h$Estimate >= 0.70, "white", "#17365D")

  ggplot(h, aes(x = Metric, y = Strategy, fill = Estimate)) +
    geom_tile(colour = "black", linewidth = 0.45) +
    geom_text(aes(label = label, colour = txt_col),
              size = if (show_ci) 2.7 else 3.8, fontface = "bold", lineheight = 0.9) +
    scale_colour_identity() +
    scale_fill_gradientn(
      colours = c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08519C"),
      limits = c(0, 1), oob = scales::squish,
      breaks = seq(0, 1, by = 0.2), labels = scales::percent_format(accuracy = 1),
      name = "Value", na.value = "grey85"
    ) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 11) +
    theme(
      axis.line = element_blank(), axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", colour = "black"),
      axis.text.y = element_text(face = "bold", colour = "black"),
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
}

ggsave(file.path(out_dir, "heatmap_all_metrics_with_CI.png"),
       make_heatmap(TRUE),  width = 10, height = 4.6, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "heatmap_all_metrics_with_CI.pdf"),
       make_heatmap(TRUE),  width = 10, height = 4.6)
ggsave(file.path(out_dir, "heatmap_all_metrics_estimates.png"),
       make_heatmap(FALSE), width = 8, height = 4, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# 7. Console summary and session info
# ------------------------------------------------------------------------------
print(tab, row.names = FALSE)
print(wide, row.names = FALSE)
print(icc, row.names = FALSE)
und <- res[res$Undefined_replicates > 0, c("Strategy", "Metric", "Undefined_replicates")]
if (nrow(und) > 0) { cat("\nUndefined bootstrap replicates (excluded):\n"); print(und, row.names = FALSE) }

writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))
cat("\nDone. Outputs written to:", normalizePath(out_dir), "\n")

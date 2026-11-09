#!/usr/bin/env Rscript
# ==============================================================================
# Deployment corpus (n = 787): crude proportions vs Bayesian misclassification-
# adjusted proportions for all five strategies.
#
#  * Crude proportion: Wilson CI (assumes independent videos) and PRIMARY
#    creator-cluster percentile bootstrap CI (5,000 resamples, seed 2026).
#  * Adjusted proportion: prevalence::truePrev (JAGS); Beta(1,1) prior on the
#    true prevalence; sensitivity ~ Beta(TP+1, FN+1) and specificity ~
#    Beta(TN+1, FP+1) from the validation counts; 2 chains, 10,000 burn-in and
#    10,000 retained iterations; seed 42. Posterior median and equal-tailed 95% CrI.
#  * Figure: crude (cluster-bootstrap CI) versus adjusted (credible interval).
#
# Usage (from the repository root):
#   Rscript R/deployment_bayesian_adjustment.R [deployment.csv] [validation.csv] [out_dir]
# Defaults: data/deployment_787.csv  data/validation_400.csv  output
# Requires: prevalence, rjags (and the JAGS program), coda, binom, ggplot2
# ==============================================================================

.args <- commandArgs(FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.dir  <- if (length(.file) > 0) dirname(normalizePath(.file[1])) else "R"
source(file.path(.dir, "utils.R"))

suppressPackageStartupMessages({ library(prevalence); library(coda); library(ggplot2) })

dep_path <- cli_arg(1, "data/deployment_787.csv")
val_path <- cli_arg(2, "data/validation_400.csv")
out_dir  <- cli_arg(3, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Data
# ------------------------------------------------------------------------------
dep <- read_deployment(dep_path)
val <- read_validation(val_path)
N   <- nrow(dep)
cat("Deployment videos:", N, " creators:", length(unique(dep$cid)), "\n")
if (N != 787) warning("Expected 787 deployment videos, found ", N)

vc <- validation_confusion(val)
check_validation_counts(vc)
stopifnot(all(rowSums(vc[, c("TP", "FP", "FN", "TN")]) == nrow(val)))

# ------------------------------------------------------------------------------
# 2. Crude proportions: Wilson and creator-cluster bootstrap CIs
# ------------------------------------------------------------------------------
crude <- crude_cluster_table(dep, B = 5000, B_icc = 2000, seed = 2026)
write.csv(crude, file.path(out_dir, "deployment_crude_cluster.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. Bayesian misclassification adjustment (one model per strategy)
# ------------------------------------------------------------------------------
rows <- lapply(seq_len(nrow(crude)), function(i) {
  nm <- crude$Strategy[i]; v <- vc[vc$Strategy == nm, ]
  cat("Fitting:", nm, "\n")
  fit <- run_truePrev(x = crude$Positives[i], n = N,
                      tp = v$TP, fn = v$FN, tn = v$TN, fp = v$FP,
                      prior = c(1, 1), nchains = 2L, burnin = 10000L,
                      update = 10000L, seed = 42)
  cri <- quantile(fit$draws, c(0.025, 0.975), names = FALSE)
  data.frame(
    Strategy = nm, Positives = crude$Positives[i], N = N,
    Crude_pct = crude$Crude_pct[i],
    Crude_cluster_lo = crude$Cluster_lo[i], Crude_cluster_hi = crude$Cluster_hi[i],
    Crude_wilson_lo = crude$Wilson_lo[i], Crude_wilson_hi = crude$Wilson_hi[i],
    Adj_median_pct = 100 * median(fit$draws),
    CrI_lo = 100 * cri[1], CrI_hi = 100 * cri[2],
    Adj_minus_crude = 100 * median(fit$draws) - crude$Crude_pct[i],
    Validation_TP = v$TP, Validation_FN = v$FN, Validation_TN = v$TN, Validation_FP = v$FP,
    Youden_J = (v$TP / (v$TP + v$FN)) + (v$TN / (v$TN + v$FP)) - 1,
    ESS = fit$ess, Rhat = fit$rhat, Rhat_upper = fit$rhat_upper,
    stringsAsFactors = FALSE)
})
res <- do.call(rbind, rows)
write.csv(res, file.path(out_dir, "deployment_bayesian_adjustment.csv"), row.names = FALSE)

shown <- res[, c("Strategy", "Crude_pct", "Crude_cluster_lo", "Crude_cluster_hi",
                 "Adj_median_pct", "CrI_lo", "CrI_hi", "ESS", "Rhat")]
print(format(shown, digits = 3), row.names = FALSE)
if (any(res$Rhat_upper > 1.05)) warning("R-hat above 1.05 for at least one strategy: check convergence.")

# ------------------------------------------------------------------------------
# 4. Figure: crude (cluster-bootstrap CI) versus adjusted (credible interval)
# ------------------------------------------------------------------------------
lev <- c("Crude proportion (creator-cluster bootstrap 95% CI)",
         "Bayesian-adjusted proportion (95% credible interval)")
plot_df <- rbind(
  data.frame(Strategy = res$Strategy, Type = lev[1], Est = res$Crude_pct,
             Lo = res$Crude_cluster_lo, Hi = res$Crude_cluster_hi),
  data.frame(Strategy = res$Strategy, Type = lev[2], Est = res$Adj_median_pct,
             Lo = res$CrI_lo, Hi = res$CrI_hi))
plot_df$Strategy <- factor(plot_df$Strategy, levels = names(STRATEGIES))
plot_df$Type     <- factor(plot_df$Type, levels = rev(lev))
plot_df$Label    <- sprintf("%.1f%% (%.1f-%.1f)", plot_df$Est, plot_df$Lo, plot_df$Hi)

cols <- setNames(c("#08519C", "#B2182B"), rev(lev))
crude_df <- plot_df[plot_df$Type == lev[1], ]
adj_df   <- plot_df[plot_df$Type == lev[2], ]

# Crude = horizontal bar (thin whisker = creator-cluster 95% CI);
# adjusted = point with 95% credible-interval line.
fig <- ggplot() +
  geom_col(data = crude_df, aes(y = Type, x = Est, fill = Type),
           width = 0.55, alpha = 0.55) +
  geom_errorbar(data = crude_df, aes(y = Type, xmin = Lo, xmax = Hi),
                width = 0.2, linewidth = 0.6, colour = "black", orientation = "y") +
  geom_pointrange(data = adj_df, aes(y = Type, x = Est, xmin = Lo, xmax = Hi, colour = Type),
                  linewidth = 0.9, size = 0.55) +
  geom_text(data = plot_df, aes(y = Type, x = Hi, label = Label),
            hjust = -0.08, size = 2.8, show.legend = FALSE) +
  facet_wrap(~Strategy, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = cols, breaks = lev, name = NULL, drop = FALSE) +
  scale_colour_manual(values = cols, breaks = lev, name = NULL, drop = FALSE) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.35))) +
  labs(x = "Proportion of deployment videos (%)", y = NULL) +
  theme_thesis() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.direction = "vertical")

save_figure(fig, file.path(out_dir, "figure_crude_vs_adjusted"), width = 9, height = 7)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info_adjustment.txt"))
cat("\nDone. Outputs in", normalizePath(out_dir), "\n")

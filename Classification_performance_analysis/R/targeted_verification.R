#!/usr/bin/env Rscript
# ==============================================================================
# Targeted verification of the rare strategies (viral marketing, event
# sponsorship, localization) in the deployment corpus.
#
# Every deployment video that Gemini flagged for at least one rare strategy was
# annotated by the primary coder. Because ONLY model-positive videos were
# reviewed, the performance metric of this stage is the positive predictive
# value (PPV). Sensitivity, specificity, NPV, F1, MCC and accuracy cannot be
# estimated here.
#
# Outputs:
#   * PPV per strategy with Wilson and creator-cluster bootstrap 95% CIs
#   * confirmed counts as lower bounds on the number of true-positive videos
#   * PPV expected if validation Se/Sp transferred, versus observed PPV
#   * exploratory deployment-implied specificity (assumes validation sensitivity)
#   * video-level confirmation
#   * figure comparing deployment and validation PPV
#
# Usage (from the repository root):
#   Rscript R/targeted_verification.R [deployment.csv] [validation.csv] [out_dir]
# ==============================================================================

.args <- commandArgs(FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.dir  <- if (length(.file) > 0) dirname(normalizePath(.file[1])) else "R"
source(file.path(.dir, "utils.R"))
suppressPackageStartupMessages(library(ggplot2))

dep_path <- cli_arg(1, "data/deployment_787.csv")
val_path <- cli_arg(2, "data/validation_400.csv")
out_dir  <- cli_arg(3, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
B <- 5000

dep <- read_deployment(dep_path)
val <- read_validation(val_path)
N   <- nrow(dep)
vc  <- validation_confusion(val)
check_validation_counts(vc)

# ------------------------------------------------------------------------------
# Verification set
# ------------------------------------------------------------------------------
flag_any <- Reduce(`|`, lapply(RARE, function(s) dep[[paste0(s, "_gemini")]] == 1))
has_label <- !is.na(dep[[RARE[[1]]]])
cat("Flagged for >=1 rare strategy:", sum(flag_any), "\n")
cat("Human-labelled:", sum(has_label), "\n")
cat("Flagged and labelled:", sum(flag_any & has_label), "\n")
if (sum(flag_any & !has_label) > 0)
  warning(sum(flag_any & !has_label), " flagged videos have no human label.")
if (sum(!flag_any & has_label) > 0)
  warning(sum(!flag_any & has_label), " labelled videos were not flagged for a rare strategy.")

ver <- dep[flag_any & has_label, , drop = FALSE]
cat("Verified videos:", nrow(ver), " creators:", length(unique(ver$cid)), "\n")

# Primary adjusted prevalence (if available) for the expected-PPV check
prim_path <- file.path(out_dir, "deployment_bayesian_adjustment.csv")
primary <- if (file.exists(prim_path)) read.csv(prim_path, stringsAsFactors = FALSE) else NULL

# ------------------------------------------------------------------------------
# PPV by strategy
# ------------------------------------------------------------------------------
rows <- lapply(names(RARE), function(nm) {
  s <- RARE[[nm]]
  fl <- dep[[paste0(s, "_gemini")]] == 1
  d  <- dep[fl & has_label, , drop = FALSE]
  d$confirmed <- as.integer(d[[s]] == 1)
  n  <- nrow(d); tp <- sum(d$confirmed); fp <- n - tp
  w  <- wilson(tp, n)

  ct <- cluster_tables(d, "confirmed")
  bs <- boot_ratios(ct, B, seed = 2026)[, 1]
  cl <- q95(bs)

  v  <- vc[vc$Strategy == nm, ]
  se <- v$TP / (v$TP + v$FN); sp <- v$TN / (v$TN + v$FP)
  val_ppv <- v$TP / (v$TP + v$FP); val_w <- wilson(v$TP, v$TP + v$FP)

  # expected PPV if validation Se/Sp transferred, at the adjusted prevalence
  pi_adj <- if (!is.null(primary)) primary$Adj_median_pct[primary$Strategy == nm] / 100
            else { p <- sum(dep[[paste0(s, "_gemini")]]) / N
                   min(max((p + sp - 1) / (se + sp - 1), 0), 1) }
  exp_ppv <- se * pi_adj / (se * pi_adj + (1 - sp) * (1 - pi_adj))

  # deployment-implied specificity: false positives are exact, true negatives
  # need the number of missed positives, taken from validation sensitivity
  fn_hat <- tp * (1 - se) / se
  sp_hat <- 1 - fp / (N - tp - fn_hat)

  data.frame(
    Strategy = nm, Flagged = n, Confirmed_TP = tp, Rejected_FP = fp,
    PPV = tp / n, PPV_cluster_lo = cl[1], PPV_cluster_hi = cl[2],
    PPV_wilson_lo = w[1], PPV_wilson_hi = w[2], Creators = length(unique(d$cid)),
    Validation_PPV = val_ppv, Validation_PPV_wilson_lo = val_w[1], Validation_PPV_wilson_hi = val_w[2],
    Expected_PPV_if_validation_transfers = exp_ppv,
    Lower_bound_prevalence_pct = 100 * tp / N,
    Implied_deployment_specificity = sp_hat, Validation_specificity = sp,
    stringsAsFactors = FALSE)
})
ppv <- do.call(rbind, rows)
write.csv(ppv, file.path(out_dir, "targeted_verification_ppv.csv"), row.names = FALSE)
print(format(ppv[, c("Strategy", "Flagged", "Confirmed_TP", "Rejected_FP", "PPV",
                     "PPV_cluster_lo", "PPV_cluster_hi", "Validation_PPV",
                     "Expected_PPV_if_validation_transfers")], digits = 3), row.names = FALSE)

# Video-level confirmation: at least one flagged rare strategy confirmed
conf_any <- Reduce(`|`, lapply(RARE, function(s)
  ver[[paste0(s, "_gemini")]] == 1 & ver[[s]] == 1))
cat(sprintf("\nVideos with >=1 confirmed flagged rare strategy: %d of %d (%.1f%%)\n",
            sum(conf_any, na.rm = TRUE), nrow(ver), 100 * mean(conf_any, na.rm = TRUE)))

# ------------------------------------------------------------------------------
# Figure: PPV in deployment verification versus validation
# ------------------------------------------------------------------------------
pf <- rbind(
  data.frame(Strategy = ppv$Strategy, Source = "Deployment verification (creator-cluster CI)",
             Est = ppv$PPV, Lo = ppv$PPV_cluster_lo, Hi = ppv$PPV_cluster_hi),
  data.frame(Strategy = ppv$Strategy, Source = "Validation set (Wilson CI)",
             Est = ppv$Validation_PPV, Lo = ppv$Validation_PPV_wilson_lo,
             Hi = ppv$Validation_PPV_wilson_hi))
pf$Strategy <- factor(pf$Strategy, levels = rev(names(RARE)))
exp_df <- data.frame(Strategy = factor(ppv$Strategy, levels = rev(names(RARE))),
                     Exp = ppv$Expected_PPV_if_validation_transfers)

fig <- ggplot(pf, aes(y = Strategy, x = Est, xmin = Lo, xmax = Hi, colour = Source)) +
  geom_pointrange(position = position_dodge(width = 0.55), size = 0.5, linewidth = 0.8) +
  geom_point(data = exp_df, aes(y = Strategy, x = Exp), inherit.aes = FALSE,
             shape = 4, size = 3.5, stroke = 1.3, colour = "black") +
  scale_x_continuous(limits = c(0, 1), labels = function(x) sprintf("%.0f%%", 100 * x)) +
  scale_colour_manual(values = c("#B2182B", "#08519C"), name = NULL) +
  labs(x = "Positive predictive value",
       caption = "Black cross = PPV expected if validation sensitivity and specificity transferred to the deployment corpus.") +
  theme_thesis()
save_figure(fig, file.path(out_dir, "figure_targeted_verification_ppv"), width = 8, height = 4)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info_verification.txt"))
cat("\nDone. Outputs in", normalizePath(out_dir), "\n")

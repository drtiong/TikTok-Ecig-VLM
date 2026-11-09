#!/usr/bin/env Rscript
# Supplementary Table S1: creator clustering in validation (reference labels) and
# deployment (Gemini labels): positives, creators with a positive, ICC (95% creator-bootstrap CI),
# size-weighted cluster size, design effects, Wilson vs creator-cluster CIs, width ratio, effective n.
# Usage: Rscript R/supplementary_table_S1.R data/validation_400.csv data/deployment_787.csv output
# Validation file: creator (or creator_id) and <strategy> reference columns (Yes/No).
# Deployment file: creator_id (or creator) and <strategy>_gemini columns (yes/no).
suppressPackageStartupMessages(library(binom))
args <- commandArgs(trailingOnly = TRUE)
val_f <- if (length(args) >= 1) args[1] else "data/validation_400.csv"
dep_f <- if (length(args) >= 2) args[2] else "data/deployment_787.csv"
out   <- if (length(args) >= 3) args[3] else "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
set.seed(2026); B <- 5000; B_ICC <- 2000
S <- c("product_placement", "youth_appeal", "viral_marketing", "event_sponsorship", "localization")

icc_anova <- function(y, g) {
  g <- as.integer(factor(g)); k <- max(g); n <- length(y); ni <- tabulate(g, k)
  mu <- as.numeric(tapply(y, g, mean)); msb <- sum(ni * (mu - mean(y))^2) / (k - 1)
  msw <- sum((y - mu[g])^2) / (n - k); n0 <- (n - sum(ni^2) / n) / (k - 1)
  den <- msb + (n0 - 1) * msw; if (den == 0) NA_real_ else (msb - msw) / den
}
get_cid <- function(d) {
  cre <- if ("creator_id" %in% names(d)) d$creator_id else d$creator
  tolower(sub(".*tiktok\\.com/@([^/?#]+).*", "\\1", trimws(as.character(cre))))
}
block <- function(d, label_col, panel) {
  d$cid <- get_cid(d); N <- nrow(d); sz <- table(d$cid); K <- length(sz)
  mw <- sum(sz^2) / N; idx <- split(seq_len(N), d$cid)
  do.call(rbind, lapply(S, function(s) {
    y <- as.integer(tolower(trimws(d[[label_col(s)]])) == "yes"); x <- sum(y)
    w <- binom.confint(x, N, methods = "wilson")
    bp <- numeric(B); bi <- numeric(B_ICC)
    for (b in seq_len(B)) bp[b] <- mean(y[unlist(idx[sample.int(K, K, TRUE)], use.names = FALSE)])
    for (b in seq_len(B_ICC)) {
      sm <- sample.int(K, K, TRUE); i <- unlist(idx[sm], use.names = FALSE)
      bi[b] <- icc_anova(y[i], rep(seq_along(sm), lengths(idx[sm])))
    }
    cl <- quantile(bp, c(.025, .975), names = FALSE); il <- quantile(bi, c(.025, .975), na.rm = TRUE, names = FALSE)
    r <- icc_anova(y, d$cid); wr <- (cl[2] - cl[1]) / (w$upper - w$lower); de <- max(1, wr^2)
    data.frame(Panel = panel, Strategy = s, N = N, Creators = K, Positives = x,
      Creators_with_positive = length(unique(d$cid[y == 1])), Pct = 100 * x / N,
      Wilson = sprintf("%.1f-%.1f", 100 * w$lower, 100 * w$upper),
      Cluster = sprintf("%.1f-%.1f", 100 * cl[1], 100 * cl[2]), Width_ratio = round(wr, 2),
      ICC = round(r, 2), ICC_CI = sprintf("%.2f to %.2f", max(il[1], -1), min(il[2], 1)),
      m_w = round(mw, 2), DEFF_icc = round(1 + (mw - 1) * max(r, 0), 2),
      DEFF_empirical = round(de, 2), Effective_n = round(N / de))
  }))
}
rd <- function(f) { d <- read.csv(f, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
  d[!is.na(d[[1]]), ] }
val <- rd(val_f); dep <- rd(dep_f)
dep <- dep[!is.na(dep[[paste0(S[1], "_gemini")]]) & nzchar(trimws(dep[[paste0(S[1], "_gemini")]])), ]
if ("filename" %in% names(dep)) dep <- dep[!duplicated(dep$filename), ]
A <- block(val, function(s) s, "A. Validation (reference labels)")
Bp <- block(dep, function(s) paste0(s, "_gemini"), "B. Deployment (Gemini labels)")
res <- rbind(A, Bp)
write.csv(res, file.path(out, "supplementary_table_S1.csv"), row.names = FALSE)
print(res, row.names = FALSE)
cat("\nNotes: ICC = one-way ANOVA estimator, unstable for rare labels in single-video creators.\n",
    "Effective n = N / empirical design effect (width ratio squared, minimum 1). DEFF_icc uses size-weighted m_w.\n")

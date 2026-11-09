#!/usr/bin/env Rscript
# Clustering of model-assigned labels by creator in the deployment corpus (n = 787):
# creator distribution, ICC (one-way ANOVA), design effects, and crude proportions
# with Wilson (independent-video) and creator-cluster bootstrap 95% CIs.
# Usage: Rscript R/deployment_clustering.R data/deployment_787.csv output
# Input: creator_id (or creator URL) and <strategy>_gemini columns (yes/no). Rows with
# missing Gemini labels (e.g. stray filename rows) are dropped.
suppressPackageStartupMessages(library(binom))
args <- commandArgs(trailingOnly = TRUE)
inp <- if (length(args) >= 1) args[1] else "data/deployment_787.csv"
out <- if (length(args) >= 2) args[2] else "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
set.seed(2026); B <- 5000; B_ICC <- 2000

d <- read.csv(inp, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
S <- c("product_placement", "youth_appeal", "viral_marketing", "event_sponsorship", "localization")
d <- d[!is.na(d[[paste0(S[1], "_gemini")]]) & nzchar(trimws(d[[paste0(S[1], "_gemini")]])), ]
cre <- if ("creator_id" %in% names(d)) d$creator_id else d$creator
d$cid <- tolower(sub(".*tiktok\\.com/@([^/?#]+).*", "\\1", trimws(as.character(cre))))
stopifnot(!anyNA(d$cid), nrow(d) > 0)
for (s in S) d[[s]] <- as.integer(tolower(trimws(d[[paste0(s, "_gemini")]])) == "yes")

N <- nrow(d); sz <- table(d$cid); K <- length(sz)
m <- N / K; mw <- sum(sz^2) / N            # mean and size-weighted cluster size
cat("Videos:", N, " Creators:", K, " mean:", round(m, 2), " weighted mean:", round(mw, 2),
    " max:", max(sz), "\n")
write.csv(data.frame(creator = names(sz), n_videos = as.integer(sz))[order(-as.integer(sz)), ],
          file.path(out, "deployment_videos_per_creator.csv"), row.names = FALSE)

idx <- split(seq_len(N), d$cid)
icc_anova <- function(y, g) {
  g <- as.integer(factor(g)); k <- max(g); n <- length(y); ni <- tabulate(g, k)
  mu <- as.numeric(tapply(y, g, mean)); msb <- sum(ni * (mu - mean(y))^2) / (k - 1)
  msw <- sum((y - mu[g])^2) / (n - k); n0 <- (n - sum(ni^2) / n) / (k - 1)
  den <- msb + (n0 - 1) * msw; if (den == 0) NA_real_ else (msb - msw) / den
}
res <- lapply(S, function(s) {
  y <- d[[s]]; x <- sum(y)
  w <- binom.confint(x, N, methods = "wilson")
  bp <- numeric(B); bi <- numeric(B_ICC)
  for (b in seq_len(B)) bp[b] <- mean(y[unlist(idx[sample.int(K, K, TRUE)], use.names = FALSE)])
  for (b in seq_len(B_ICC)) {
    sm <- sample.int(K, K, TRUE); i <- unlist(idx[sm], use.names = FALSE)
    bi[b] <- icc_anova(y[i], rep(seq_along(sm), lengths(idx[sm])))
  }
  r <- icc_anova(y, d$cid); cl <- quantile(bp, c(.025, .975), names = FALSE)
  data.frame(Strategy = s, Positives = x, Crude_pct = 100 * x / N,
    Wilson_lo = 100 * w$lower, Wilson_hi = 100 * w$upper,
    Cluster_lo = 100 * cl[1], Cluster_hi = 100 * cl[2],
    Width_ratio = (cl[2] - cl[1]) / (w$upper - w$lower),
    ICC = r, ICC_lo = quantile(bi, .025, na.rm = TRUE, names = FALSE),
    ICC_hi = quantile(bi, .975, na.rm = TRUE, names = FALSE),
    DEFF_icc = 1 + (mw - 1) * max(r, 0), DEFF_empirical = ((cl[2] - cl[1]) / (w$upper - w$lower))^2,
    Effective_n = N / (1 + (mw - 1) * max(r, 0)))
})
res <- do.call(rbind, res)
write.csv(res, file.path(out, "deployment_clustering.csv"), row.names = FALSE)
print(res, digits = 3, row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out, "session_info_deployment.txt"))

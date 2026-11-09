# ==============================================================================
# Shared helpers for the deployment, sensitivity, verification and reliability
# scripts. Sourced by those scripts; not run directly.
# ==============================================================================

suppressPackageStartupMessages(library(binom))

STRATEGIES <- c(
  "Product placement" = "product_placement",
  "Youth appeal"      = "youth_appeal",
  "Viral marketing"   = "viral_marketing",
  "Event sponsorship" = "event_sponsorship",
  "Localization"      = "localization"
)
RARE <- STRATEGIES[c("Viral marketing", "Event sponsorship", "Localization")]

# Validation confusion matrices used in the original Bayesian script. They are
# recomputed from the validation file and compared with these as a safeguard.
EXPECTED_VALIDATION <- data.frame(
  Strategy = names(STRATEGIES),
  TN = c(7, 32, 284, 352, 337), FP = c(2, 6, 12, 1, 6),
  FN = c(1, 55, 33, 4, 24),     TP = c(390, 307, 71, 43, 33),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Command line and paths
# ------------------------------------------------------------------------------
script_dir <- function() {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[1])) else "R"
}

cli_arg <- function(i, default) {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= i && nzchar(a[i])) a[i] else default
}

# ------------------------------------------------------------------------------
# Reading and cleaning data
# ------------------------------------------------------------------------------
creator_handle <- function(x) {
  tolower(sub(".*tiktok\\.com/@([^/?#]+).*", "\\1", trimws(as.character(x))))
}

get_cid <- function(d) {
  cre <- if ("creator_id" %in% names(d)) d$creator_id
         else if ("creator" %in% names(d)) d$creator
         else stop("Input needs a 'creator_id' or 'creator' column.")
  cid <- creator_handle(cre)
  if (anyNA(cid) || any(!nzchar(cid))) stop("Missing creator identifiers.")
  cid
}

to_binary <- function(x, label, allow_na = FALSE) {
  v <- tolower(trimws(as.character(x)))
  v[v %in% c("", "na", "nan")] <- NA
  bad <- !is.na(v) & !(v %in% c("yes", "no"))
  if (any(bad)) {
    stop("Column '", label, "' has values other than yes/no: ",
         paste(unique(v[bad]), collapse = ", "))
  }
  if (!allow_na && anyNA(v)) stop("Column '", label, "' has missing values.")
  as.integer(v == "yes")
}

read_validation <- function(path) {
  d <- read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                check.names = FALSE)
  d$cid <- get_cid(d)
  for (s in STRATEGIES) {
    for (col in c(s, paste0(s, "_gemini"))) {
      if (!col %in% names(d)) stop("Missing column: ", col)
      d[[col]] <- to_binary(d[[col]], col)
    }
  }
  d
}

# Deployment file: rows without Gemini labels (e.g. stray filename rows) are
# dropped. Human labels (verification) may be missing for most videos.
read_deployment <- function(path) {
  d <- read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                check.names = FALSE)
  g1 <- paste0(STRATEGIES[[1]], "_gemini")
  if (!g1 %in% names(d)) stop("Missing column: ", g1)
  keep <- !is.na(d[[g1]]) & nzchar(trimws(as.character(d[[g1]])))
  d <- d[keep, , drop = FALSE]
  d$cid <- get_cid(d)
  for (s in STRATEGIES) {
    gc <- paste0(s, "_gemini")
    if (!gc %in% names(d)) stop("Missing column: ", gc)
    d[[gc]] <- to_binary(d[[gc]], gc)
    d[[s]] <- if (s %in% names(d)) to_binary(d[[s]], s, allow_na = TRUE) else NA_integer_
  }
  d
}

# ------------------------------------------------------------------------------
# Confusion matrix and metrics
# ------------------------------------------------------------------------------
confusion <- function(truth, pred) {
  c(tp = sum(truth == 1 & pred == 1), fp = sum(truth == 0 & pred == 1),
    fn = sum(truth == 1 & pred == 0), tn = sum(truth == 0 & pred == 0))
}

validation_confusion <- function(val) {
  do.call(rbind, lapply(names(STRATEGIES), function(nm) {
    s  <- STRATEGIES[[nm]]
    cc <- confusion(val[[s]], val[[paste0(s, "_gemini")]])
    data.frame(Strategy = nm, TP = cc[["tp"]], FP = cc[["fp"]],
               FN = cc[["fn"]], TN = cc[["tn"]], stringsAsFactors = FALSE)
  }))
}

check_validation_counts <- function(vc) {
  m <- merge(vc, EXPECTED_VALIDATION, by = "Strategy", suffixes = c("", "_exp"))
  if (any(m$TP != m$TP_exp | m$FP != m$FP_exp | m$FN != m$FN_exp | m$TN != m$TN_exp)) {
    warning("Validation confusion matrices differ from those used in the original ",
            "Bayesian adjustment script. Check the input file.")
  } else {
    cat("Check passed: validation 2x2 tables match the original Bayesian inputs.\n")
  }
}

safe_div <- function(a, b) if (b == 0) NA_real_ else a / b

metric_names <- c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy",
                  "Balanced accuracy", "F1 (yes)", "F1 (no)", "MCC",
                  "Cohen kappa", "Gwet AC1", "PABAK")

calc_metrics <- function(truth, pred) {
  cc <- confusion(truth, pred)
  tp <- as.numeric(cc["tp"]); fp <- as.numeric(cc["fp"])
  fn <- as.numeric(cc["fn"]); tn <- as.numeric(cc["tn"])
  n  <- tp + fp + fn + tn
  se <- safe_div(tp, tp + fn); sp <- safe_div(tn, tn + fp)
  mcc_den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  po <- (tp + tn) / n
  pe <- ((tp + fp) * (tp + fn) + (tn + fn) * (tn + fp)) / n^2
  pi_yes <- ((tp + fp) + (tp + fn)) / (2 * n)
  pe_g <- 2 * pi_yes * (1 - pi_yes)
  c("Sensitivity" = se, "Specificity" = sp,
    "PPV" = safe_div(tp, tp + fp), "NPV" = safe_div(tn, tn + fn),
    "Accuracy" = po, "Balanced accuracy" = (se + sp) / 2,
    "F1 (yes)" = safe_div(2 * tp, 2 * tp + fp + fn),
    "F1 (no)"  = safe_div(2 * tn, 2 * tn + fn + fp),
    "MCC" = if (mcc_den == 0) NA_real_ else (tp * tn - fp * fn) / mcc_den,
    "Cohen kappa" = if (pe == 1) NA_real_ else (po - pe) / (1 - pe),
    "Gwet AC1" = if (pe_g == 1) NA_real_ else (po - pe_g) / (1 - pe_g),
    "PABAK" = 2 * po - 1)
}

wilson <- function(x, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  r <- binom::binom.confint(x = x, n = n, conf.level = 0.95, methods = "wilson")
  c(r$lower, r$upper)
}

# ------------------------------------------------------------------------------
# Clustering: ICC, design effects, creator-cluster bootstrap
# ------------------------------------------------------------------------------
icc_anova <- function(y, g) {
  g  <- as.integer(factor(g)); k <- max(g); N <- length(y)
  if (k < 2) return(NA_real_)
  ni <- tabulate(g, nbins = k)
  mu <- as.numeric(tapply(y, g, mean)); gm <- mean(y)
  msb <- sum(ni * (mu - gm)^2) / (k - 1)
  msw <- sum((y - mu[g])^2) / (N - k)
  n0  <- (N - sum(ni^2) / N) / (k - 1)
  den <- msb + (n0 - 1) * msw
  if (den == 0) NA_real_ else (msb - msw) / den
}

# Size-weighted mean cluster size (appropriate when cluster sizes vary widely)
weighted_cluster_size <- function(cid) { sz <- table(cid); sum(sz^2) / sum(sz) }

design_effect <- function(icc, m) 1 + (m - 1) * max(icc, 0, na.rm = TRUE)

# Per-creator numerators and denominators for ratio estimators
# `data` is a data frame with a `cid` column; xcols / ncols are COLUMN NAMES in it.
cluster_tables <- function(data, xcols, ncols = NULL) {
  g <- factor(data$cid)
  agg <- function(col) as.numeric(tapply(as.numeric(data[[col]]), g, sum))
  x <- vapply(xcols, agg, numeric(nlevels(g)))
  if (is.null(dim(x))) x <- matrix(x, ncol = length(xcols))
  n <- if (is.null(ncols)) matrix(as.numeric(table(g)), nrow = nlevels(g), ncol = ncol(x))
       else { m <- vapply(ncols, agg, numeric(nlevels(g)))
              if (is.null(dim(m))) matrix(m, ncol = length(ncols)) else m }
  list(x = x, n = n, K = nlevels(g))
}

# Creator-cluster bootstrap of ratio estimators sum(x)/sum(n), same resample
# applied to every column. Returns a B x S matrix (NaN when a denominator is 0).
boot_ratios <- function(ct, B, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  out <- matrix(NA_real_, nrow = B, ncol = ncol(ct$x))
  for (b in seq_len(B)) {
    s <- sample.int(ct$K, ct$K, replace = TRUE)
    num <- colSums(ct$x[s, , drop = FALSE]); den <- colSums(ct$n[s, , drop = FALSE])
    out[b, ] <- ifelse(den == 0, NA_real_, num / den)
  }
  out
}

q95 <- function(v) quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE)

icc_cluster_boot <- function(y, cid, B, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  idx <- split(seq_along(y), cid); K <- length(idx)
  vals <- vapply(seq_len(B), function(b) {
    s <- sample.int(K, K, replace = TRUE)
    i <- unlist(idx[s], use.names = FALSE)
    icc_anova(y[i], rep(seq_along(s), times = lengths(idx[s])))
  }, numeric(1))
  q95(vals)
}

# Crude proportions with Wilson and creator-cluster bootstrap CIs, plus ICC and
# design effects, for the deployment corpus.
crude_cluster_table <- function(dep, B = 5000, B_icc = 2000, seed = 2026) {
  N <- nrow(dep); cols <- paste0(STRATEGIES, "_gemini")
  ct <- cluster_tables(dep, cols)
  bp <- boot_ratios(ct, B, seed = seed)
  mw <- weighted_cluster_size(dep$cid)
  do.call(rbind, lapply(seq_along(STRATEGIES), function(j) {
    nm <- names(STRATEGIES)[j]; y <- dep[[cols[j]]]; x <- sum(y)
    w <- wilson(x, N); cl <- q95(bp[, j])
    r <- icc_anova(y, dep$cid); ci <- icc_cluster_boot(y, dep$cid, B_icc, seed = seed + j)
    ratio <- (cl[2] - cl[1]) / (w[2] - w[1])
    data.frame(Strategy = nm, Positives = x, N = N, Crude_pct = 100 * x / N,
               Wilson_lo = 100 * w[1], Wilson_hi = 100 * w[2],
               Cluster_lo = 100 * cl[1], Cluster_hi = 100 * cl[2],
               Width_ratio = ratio, ICC = r, ICC_lo = ci[1], ICC_hi = ci[2],
               DEFF_icc = design_effect(r, mw), DEFF_empirical = max(1, ratio^2),
               Effective_n = N / design_effect(r, mw), stringsAsFactors = FALSE)
  }))
}

# ------------------------------------------------------------------------------
# Bayesian misclassification adjustment
# ------------------------------------------------------------------------------
# Primary model: prevalence::truePrev (JAGS), exactly as in the thesis methods.
run_truePrev <- function(x, n, tp, fn, tn, fp, prior = c(1, 1),
                         nchains = 2L, burnin = 10000L, update = 10000L, seed = 42) {
  set.seed(seed)
  fit <- prevalence::truePrev(
    x = x, n = n,
    SE = list(dist = "beta", alpha = tp + 1, beta = fn + 1),
    SP = list(dist = "beta", alpha = tn + 1, beta = fp + 1),
    prior = prior, nchains = nchains, burnin = burnin, update = update,
    verbose = FALSE
  )
  if (!("TP" %in% names(fit@mcmc))) stop("TP posterior component missing.")
  chains <- fit@mcmc[["TP"]]
  draws  <- as.numeric(as.matrix(chains))
  if (length(draws) == 0L || any(!is.finite(draws))) stop("Invalid posterior draws.")
  rh <- coda::gelman.diag(chains, autoburnin = FALSE, multivariate = FALSE)
  list(draws = draws, ess = as.numeric(coda::effectiveSize(chains))[1],
       rhat = unname(rh$psrf[1, "Point est."]), rhat_upper = unname(rh$psrf[1, "Upper C.I."]))
}

wquantile <- function(x, w, probs) {
  o <- order(x); xs <- x[o]; cw <- cumsum(w[o]); cw <- cw / cw[length(cw)]
  xs[pmin(findInterval(probs, cw) + 1L, length(xs))]
}

# Importance-sampling version of the same model. Used for sensitivity analyses
# that cannot be run through truePrev (non-integer effective counts, alternative
# likelihoods). Prior draws for pi are weighted by the likelihood.
#   d_val: design effect dividing the validation counts (1 = as analysed)
#   d_dep: design effect dividing the deployment counts (1 = as analysed)
#   verification: NULL, or c(confirmed, rejected) among ALL flagged videos, which
#     switches to the partial-verification (multinomial) deployment likelihood.
is_posterior <- function(x, n, tp, fn, tn, fp, d_val = 1, d_dep = 1,
                         prior = c(1, 1), verification = NULL, M = 1.5e6, seed = 42) {
  set.seed(seed)
  pi <- rbeta(M, prior[1], prior[2])
  se <- rbeta(M, tp / d_val + 1, fn / d_val + 1)
  sp <- rbeta(M, tn / d_val + 1, fp / d_val + 1)
  if (is.null(verification)) {
    ap <- pmin(pmax(se * pi + (1 - sp) * (1 - pi), 1e-300), 1 - 1e-16)
    xe <- x / d_dep; ne <- n / d_dep
    ll <- xe * log(ap) + (ne - xe) * log1p(-ap)
  } else {
    conf <- verification[1]; rej <- verification[2]; rest <- n - conf - rej
    p1 <- pmax(se * pi, 1e-300); p2 <- pmax((1 - sp) * (1 - pi), 1e-300)
    p3 <- pmax(1 - p1 - p2, 1e-300)
    ll <- (conf * log(p1) + rej * log(p2) + rest * log(p3)) / d_dep
  }
  w <- exp(ll - max(ll))
  qs <- function(v, scale = 1) scale * wquantile(v, w, c(0.5, 0.025, 0.975))
  list(pi = qs(pi, 100), se = qs(se), sp = qs(sp), ess = sum(w)^2 / sum(w^2))
}

# ------------------------------------------------------------------------------
# Plot theme
# ------------------------------------------------------------------------------
theme_thesis <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(colour = "black"),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}

save_figure <- function(p, path_no_ext, width, height) {
  ggplot2::ggsave(paste0(path_no_ext, ".png"), p, width = width, height = height,
                  dpi = 300, bg = "white")
  ggplot2::ggsave(paste0(path_no_ext, ".pdf"), p, width = width, height = height)
}

#!/usr/bin/env Rscript
# ==============================================================================
# One-panel figure: crude flagged proportion versus Bayesian-
# adjusted prevalence, each a point with a 95% interval line (different colours), for all five strategies,
# on a single x axis (proportion of deployment videos, %). The y axis lists the
# strategies, grouped into tiers: primary anchors, then supplementary strategies.
#
# Reads the table written by R/deployment_bayesian_adjustment.R, so JAGS does not
# have to be re-run.
#
# Usage: Rscript R/figure_bayesian_combined.R [adjustment_csv] [out_dir]
# Output: out_dir/figure_bayesian_combined.png and .pdf
# ==============================================================================
suppressPackageStartupMessages(library(ggplot2))

args    <- commandArgs(trailingOnly = TRUE)
in_csv  <- if (length(args) >= 1) args[1] else "output/deployment_bayesian_adjustment.csv"
out_dir <- if (length(args) >= 2) args[2] else "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

res <- read.csv(in_csv, stringsAsFactors = FALSE, check.names = FALSE)
need <- c("Strategy", "Crude_pct", "Crude_cluster_lo", "Crude_cluster_hi",
          "Adj_median_pct", "CrI_lo", "CrI_hi")
if (!all(need %in% names(res))) stop("Missing columns: ", paste(setdiff(need, names(res)), collapse = ", "))

# Tiers: primary anchors first, then supplementary strategies (top to bottom)
ord  <- c("Product placement", "Event sponsorship", "Youth appeal", "Viral marketing", "Localization")
ypos <- c(5, 4, 3, 2, 1)                       # gap between the two tiers
res  <- res[match(ord, res$Strategy), ]
if (anyNA(res$Strategy)) stop("Strategy names in the CSV do not match: ", paste(ord, collapse = ", "))

lev <- c("Crude proportion flagged by Gemini (point; creator-cluster bootstrap 95% CI)",
         "Bayesian-adjusted prevalence (point; median and 95% credible interval)")
off <- 0.2                                      # vertical offset of the two estimates

lab <- function(e, lo, hi) sprintf("%.2f%% (%.2f-%.2f)", e, lo, hi)
crude <- data.frame(Strategy = ord, y = ypos + off, x = res$Crude_pct,
                    lo = res$Crude_cluster_lo, hi = res$Crude_cluster_hi)
crude$label <- lab(crude$x, crude$lo, crude$hi); crude$Type <- lev[1]
adj <- data.frame(Strategy = ord, y = ypos - off, x = res$Adj_median_pct,
                  lo = res$CrI_lo, hi = res$CrI_hi)
adj$label <- lab(adj$x, adj$lo, adj$hi); adj$Type <- lev[2]
both <- rbind(crude, adj)

XMAX <- 135
tiers <- data.frame(tier = c("Primary anchors", "Supplementary strategies"),
                    y = c(4.3, 2.3))
band  <- data.frame(ymin = 3.4, ymax = 5.6)     # light shading behind the anchor tier

fig <- ggplot() +
  geom_rect(data = band, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey95", colour = NA) +
  geom_pointrange(data = both, aes(x = x, y = y, xmin = lo, xmax = hi, colour = Type),
                  linewidth = 0.9, size = 0.55) +
  geom_text(data = both, aes(x = hi, y = y, label = label),
            hjust = -0.08, size = 3.9) +
  geom_text(data = tiers, aes(x = XMAX, y = y, label = tier),
            hjust = 1, vjust = 1, fontface = "italic", size = 4.2, colour = "grey35") +
  scale_colour_manual(values = setNames(c("#08519C", "#B2182B"), lev), breaks = lev, name = NULL) +
  scale_y_continuous(breaks = ypos, labels = ord, limits = c(0.4, 6.6), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, XMAX), expand = c(0, 0)) +
  labs(x = "Proportion of deployment videos (%)", y = NULL) +
  theme_classic(base_size = 13) +
  theme(axis.text.y = element_text(face = "bold", colour = "black"),
        panel.grid.major.x = element_line(colour = "grey88", linetype = "dashed"),
        legend.position = "bottom", legend.direction = "vertical",
        legend.box.just = "left", plot.margin = margin(8, 12, 8, 8))

ggsave(file.path(out_dir, "figure_bayesian_combined.png"), fig,
       width = 9.5, height = 5.4, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "figure_bayesian_combined.pdf"), fig, width = 9.5, height = 5.4)
cat("Saved figure_bayesian_combined.png / .pdf in", normalizePath(out_dir), "\n")

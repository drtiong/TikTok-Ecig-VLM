#!/usr/bin/env Rscript
# Installs the R packages used by the scripts. JAGS (the program) must be
# installed separately from https://mcmc-jags.sourceforge.io/ before rjags/prevalence
# can fit the primary Bayesian model.
pkgs <- c("binom", "ggplot2", "scales", "coda", "rjags", "prevalence")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need) > 0) install.packages(need, repos = "https://cloud.r-project.org")
for (p in pkgs) cat(sprintf("%-12s %s\n", p, as.character(packageVersion(p))))

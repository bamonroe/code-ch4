if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

do_hng_clean    <- T
do_hng_estimate <- T
do_hng_format   <- T
do_hng_common   <- T
do_hng_wel      <- T
do_hng_plots    <- T

# Take thr original HNG data and clean it up for use in R and my MSL library
if (do_hng_clean) {
	cat("Clean real HNG data\n")
	source("clean.R")
}

# Estimate the HNG data, both with the HNG transformations of outcomes and without
if (do_hng_estimate) {
	cat("Estimate real HNG data\n")
	source("estimate.R")
}

# Rename the HNG dataset in a way that matches my format
if (do_hng_format) {
	cat("Format real HNG data\n")
	source("format.R")
}

# Find the common stuff in our estimates
if (do_hng_common) {
	cat("Get common stuff for real HNG data\n")
	source("common.R")
}

# Do the welfare calculations on HNG
if (do_hng_wel) {
	cat("Get welfare for real HNG data\n")
	source("welfare.R")
}

# Regenerate a few plots from HNG
if (do_hng_plots) {
	cat("Get plots for real HNG data\n")
	source("plots.R")
}

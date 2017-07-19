if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

multiples <- c(3, 5, 7, 9, 11)

new_rawdat_dir <- paste0(raw_dir, "new_rawdat")
new_est_dir    <- paste0(raw_dir, "new_estimates")
new_df_dir     <- paste0(raw_dir, "new_estdf")
new_weldat_dir <- paste0(raw_dir, "new_weldat")
new_merged_dir <- paste0(raw_dir, "new_merged")
new_full_dir   <- paste0(raw_dir, "new_full")

c.export(ls())

# These two only ever need to be done once
do_keepHNG   <- F
do_mkweldat  <- F
# Estimation take a really, really long time.
do_estimate  <- F
# From below are the only ones needed post estimation
do_instsplit <- F
do_welcalc   <- F
do_fullmerge <- T

if (do_keepHNG) {
	source("keepHNG.R")
}
if (do_mkweldat) {
	source("mkweldat.R")
}
if (do_estimate) {
	source("estimate.R")
}

if (do_instsplit) {
	source("instsplit.R")
}

if (do_welcalc) {
	source("welcalc.R")
}

if (do_fullmerge) {
	source("fullmerge.R")
}


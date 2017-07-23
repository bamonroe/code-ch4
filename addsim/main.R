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

for (dir in c(new_rawdat_dir, new_est_dir, new_df_dir, new_weldat_dir, new_merged_dir, new_full_dir)) {
	if (! dir.exists(dir)) dir.create(dir)
}

c.export(ls())

# These two only ever need to be done once
do_popgen    <- F # Generate the choice data
do_keepHNG   <- F # drop everything that isn't HNG
do_mkweldat  <- F # Separate out the insurance task data
# Estimation takes a really, really long time.
do_estimate  <- F # estimate on the HNG datasets
# From below are the only ones needed post estimation
do_instsplit <- F # Split the files out per Instrument and pick the winners for HNG
do_welcalc   <- F # Calculate welfare
do_fullmerge <- T # merge everything into one file per instrument

if (do_popgen) {
	source("popgen.R")
}

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

here <- getwd()
setwd("../")
source("setup.R")
setwd(here)

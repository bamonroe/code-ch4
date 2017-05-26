if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

do_merge_inst <- F
do_est2inst   <- F
do_merge_est  <- T

if (do_merge_inst) {
	c.source("full.R")
	c.lapply(insts, merge_inst)
}

if (do_est2inst) {
	source("mergeest.R")
	c.lapply(insts, est_2_inst)
}

if (do_merge_est) {
	source("mergeest.R")
	lapply(insts, merge_est)
}

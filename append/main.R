if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

do_merge_inst <- T
do_est2inst   <- T
do_merge_est  <- F
do_est_hess   <- T

if (do_merge_inst) {
	c.source("full.R")
	lapply(insts, merge_inst)
}

if (do_est2inst) {
	source("mergeest.R")
	lapply(insts, est_2_inst)
}

if (do_merge_est) {
	source("mergeest.R")
	lapply(insts, merge_est)
}

if (do_est_hess) {
	source("mergeest.R")
	lapply(insts, est_hess)
}

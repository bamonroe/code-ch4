if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

# plot config
dev.type <- "pdf"
width   <- 10
height  <- 4.0
units   <- "in"

# Full data plot
do_win_all   <- T
do_win_ind   <- T
do_wel_all   <- T
do_wel_ind   <- T
do_exwel_ind <- T

do_exwel_diff_ind <- F

do_all_insts <- F

do_correct_all <- T

theme_set(theme_grey())

c.source("newplot.R")

if (do_win_all | do_win_ind | do_wel_all | do_wel_ind | do_exwel_ind | do_exwel_diff_ind) {
	lapply(insts, per_inst)
}

if (do_all_insts) {
	all_exwel(insts)
}

if (do_correct_all) {
	all_correct_prob(insts)
}

print(warnings())

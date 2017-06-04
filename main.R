source("setup.R")
from_main <- T

# What stuff are we doing?
do_real_HNG <- F

do_append <- F
do_clean  <- F
do_fit    <- T
do_plot   <- T

if (do_real_HNG) {
	setwd("HNG")
	cat("Do Real HNG Analysis\n")
	source("main.R")
	setwd("../")
}

if (do_append) {
	setwd("append")
	cat("Do Appending\n")
	source("main.R")
	setwd("../")
}

if (do_clean) {
	setwd("clean")
	cat("Do Clean\n")
	source("main.R")
	setwd("../")
}

#load(paste0(data_dir, "stat_names.Rda"))
#win_vars <- c(win_vars, sname)

if (do_fit) {
	setwd("fitline")
	cat("Do Fit\n")
	source("main.R")
	setwd("../")
}

if (do_plot) {
	setwd("plot")
	cat("Do Plot\n")
	source("main.R")
	setwd("../")
}


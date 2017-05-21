source("setup.R")
from_main <- T

# What stuff are we doing?
do_clean <- T
do_fit   <- T
do_plot  <- T

if (do_clean) {
	setwd("clean")
	cat("Do Clean\n")
	source("main.R")
	setwd("../")
}

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


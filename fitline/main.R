if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

do_fit   <- T # Fit the Gam models
do_fpred <- T # Make predictions based on the fitted models
do_ppred <- T # Make predictions based on the fitted models for a specific population
do_csv   <- T # make simulated population formatted in a way to be useful in latex


if (do_fit) {
	source("fit.R")
}

if (do_fpred) {
	source("full_predict.R")
}

if (do_ppred) {
	source("pop.R")
}

if (do_csv) {
	source("tocsv2.R")
}


if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

do_fit   <- T # Fit the Gam models
do_fpred <- T # Make predictions based on the fitted models

do_ppred <- F # Make predictions based on the fitted models
do_pred_plot  <- F # Plot the predicted lines
do_csv   <- F # make simulated population formatted in a way to be useful in latex

if (do_fit) {
	source("fit.R")
}

if (do_fpred) {
	source("full_predict.R")
}

if (do_ppred) {
	source("pop_predict.R")
}

if (do_pred_plot) {
	source("plot_fits.R")
}

if (do_csv) {
	source("tocsv.R")
}


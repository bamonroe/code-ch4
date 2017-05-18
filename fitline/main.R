library(ctools)
c.library("dplyr", "mgcv", "halton", "ggplot2")

# General Configs
insts <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
insts <- c("HNG_1")
data_dir <- "../data/classify/full/"
fit_dir <- "../data/lo_fits/"

wel_vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel_var  <- wel_vars[1]

win_vars <- c("win_05", "default")

mods <- c("EUT", "PRE")

do_fit   <- T # Fit the Gam models
do_fpred <- T # Make predictions based on the fitted models
do_ppred <- F # Make predictions based on the fitted models
do_plot  <- F # Plot the predicted lines
do_csv   <- F # make simulated population formatted in a way to be useful in latex

if (do_fit) {
	source("fit.R")
}

if (do_fpred) {
	source("full_predict.R")
}

if (do_ppred) {
	source("pop_predict.R", echo = T)
}

if (do_plot) {
	source("plot_fits.R", echo = T)
}

if (do_csv) {
	source("tocsv.R", echo = T)
}

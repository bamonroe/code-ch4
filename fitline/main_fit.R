
do.mini    <- F # Make mini version of the datasets with extraneous variables removed
do.fit     <- T # Fit the Gam models
do.predict <- T # Make predictions based on the fitted models
do.plot    <- T # Plot the predicted lines
do.csv     <- T # make simulated population formatted in a way to be useful in latex

if (do.mini){
	source("mini.R", echo = T)
}

if (do.fit) {
	source("fit.R", echo = T)
}

if (do.fit) {
	source("fit.R", echo = T)
}

if (do.predict) {
	source("predict.R", echo = T)
}

if (do.plot) {
	source("plot_fits.R", echo = T)
}

if (do.csv) {
	source("tocsv.R", echo = T)
}

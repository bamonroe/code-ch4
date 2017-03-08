library(dplyr)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
data_dir <- "../data/classify/full/"
fit_dir <- "../data/lo_fits/"
suffix <- "-mini.Rda"

fit_lo <- function(dat, mod, win) {

	dat <- dat %>%
		filter(model == mod)

	dat$win_05 <- ifelse(dat$win_05 == win, 1, 0)

	print(paste(mod, "as", win, ":", sum(dat$win_05)))

	if (mod == "EUT") {
		hfit <- loess(win_05 ~ r + mu, dat )
	} else if ((mod == "pow") | (mod == "invs")) {
		hfit <- loess(win_05 ~ r + mu + alpha, dat )
	} else if (mod == "prelec") {
		hfit <- loess(win_05 ~ r + mu + alpha + beta, dat )
	} else {
		stop("Not a correct model")
	}

	return(hfit)
}

for (inst in instruments) {

	load(paste0(data_dir, inst, suffix))

	cat(paste("\nFitting for", inst),"\n")

	# Load the instrument into a known var
	idat <- get(inst)

	loess_fits <- list()

	loess_fits$EUT <- list(EUT = fit_lo(idat, "EUT", "EUT"),
	                       POW = fit_lo(idat, "EUT", "POW"),
	                       INV = fit_lo(idat, "EUT", "INV"),
	                       PRE = fit_lo(idat, "EUT", "PRE"))
	loess_fits$POW <- list(EUT = fit_lo(idat, "pow", "EUT"),
	                       POW = fit_lo(idat, "pow", "POW"),
	                       INV = fit_lo(idat, "pow", "INV"),
	                       PRE = fit_lo(idat, "pow", "PRE"))
	loess_fits$INV <- list(EUT = fit_lo(idat, "invs", "EUT"),
	                       POW = fit_lo(idat, "invs", "POW"),
	                       INV = fit_lo(idat, "invs", "INV"),
	                       PRE = fit_lo(idat, "invs", "PRE"))
	loess_fits$PRE <- list(EUT = fit_lo(idat, "prelec", "EUT"),
	                       POW = fit_lo(idat, "prelec", "POW"),
	                       INV = fit_lo(idat, "prelec", "INV"),
	                       PRE = fit_lo(idat, "prelec", "PRE"))

	fit_name <- paste0(inst, "_loess")

	assign(fit_name, loess_fits)

	print(ls())

	save(list=fit_name, file=paste0(fit_dir, fit_name, suffix))
	cat("\n")

}




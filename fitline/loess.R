library(dplyr)
library(mgcv)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
data_dir <- "../data/classify/full/"
fit_dir <- "../data/lo_fits/"
load_suffix <- "-mini.Rda"
save_suffix <- "-gam.Rda"

fit_mod <- function(dat, mod, win) {
	dat <- dat %>%
		filter(model == mod)

	dat$win_05 <- ifelse(dat$win_05 == win, 1, 0)

	print(paste(mod, "as", win, ":", sum(dat$win_05)))

	if (mod == "EUT") {
		hfit <- gam(win_05 ~ s(r) + s(mu), data = dat)
	} else if ((mod == "pow") | (mod == "invs")) {
		hfit <- gam(win_05 ~ s(r) + s(mu) +s(alpha) + s(r, alpha), data = dat)
	} else if (mod == "prelec") {
		hfit <- gam(win_05 ~ s(r) + s(mu) + s(alpha) + s(beta) + s(alpha, beta)+ s(r, alpha, beta), data = dat)
	} else {
		stop("Not a correct model")
	}

	return(hfit)
}

for (inst in instruments) {

	load(paste0(data_dir, inst, load_suffix))

	cat(paste("\nFitting for", inst),"\n")

	# Load the instrument into a known var
	idat <- get(inst)

	loess_fits <- list()

	loess_fits$EUT <- list(EUT = fit_mod(idat, "EUT", "EUT"),
	                       POW = fit_mod(idat, "EUT", "POW"),
	                       INV = fit_mod(idat, "EUT", "INV"),
	                       PRE = fit_mod(idat, "EUT", "PRE"))
	loess_fits$POW <- list(EUT = fit_mod(idat, "pow", "EUT"),
	                       POW = fit_mod(idat, "pow", "POW"),
	                       INV = fit_mod(idat, "pow", "INV"),
	                       PRE = fit_mod(idat, "pow", "PRE"))
	loess_fits$INV <- list(EUT = fit_mod(idat, "invs", "EUT"),
	                       POW = fit_mod(idat, "invs", "POW"),
	                       INV = fit_mod(idat, "invs", "INV"),
	                       PRE = fit_mod(idat, "invs", "PRE"))
	loess_fits$PRE <- list(EUT = fit_mod(idat, "prelec", "EUT"),
	                       POW = fit_mod(idat, "prelec", "POW"),
	                       INV = fit_mod(idat, "prelec", "INV"),
	                       PRE = fit_mod(idat, "prelec", "PRE"))

	fit_name <- paste0(inst, "_loess")

	assign(fit_name, loess_fits)

	save(list=fit_name, file=paste0(fit_dir, fit_name, save_suffix))
	cat("\n")

}




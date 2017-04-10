library(dplyr)
library(mgcv)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
data_dir <- "../data/classify/full/"
fit_dir <- "../data/lo_fits/"
load_suffix <- "-bak.Rda"
load_suffix <- "-mini.Rda"
save_suffix <- "-gam.Rda"

wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel.var  <- wel.vars[1]

fit_gam <- function(dat, mod, win, wel) {

	dat <- dat %>%
		filter(model == mod)

	dat$win_05 <- ifelse(dat$win_05 == win, 1, 0)

	dat <- dat %>%
		filter(!is.na(win_05))

	cat("\n")
	cat(paste(mod, "as", win, ":", sum(dat$win_05), "; Not NA:", nrow(dat), "\n"))

	cat("\tFitting Probability\n")
	if (mod == "EUT") {
		pfit <- gam(win_05 ~ s(r) + s(mu), data = dat)
	} else if ((mod == "pow") | (mod == "invs")) {
		pfit <- gam(win_05 ~ s(r) + s(alpha) + s(r, alpha) + s(mu), data = dat)
	} else if (mod == "prelec") {
		pfit <- gam(win_05 ~ s(r) + s(alpha) + s(beta) + s(alpha, beta)+ s(r, alpha, beta) + s(mu), data = dat)
	} else {
		stop("Not a correct model")
	}

	dat <- dat %>%
		filter(win_05 == 1)

	dat$wel <- dat[[paste0(win, "_", wel)]] - dat[[paste0("real_", wel)]]

	cat("\tFitting Welfare\n")
	if (mod == "EUT") {
		wfit <- gam(wel ~ s(r) + s(mu), data = dat)
	} else if ((mod == "pow") | (mod == "invs")) {
		wfit <- gam(wel ~ s(r) + s(alpha) + s(r, alpha) + s(mu), data = dat)
	} else if (mod == "prelec") {
		wfit <- gam(wel ~ s(r) + s(alpha) + s(beta) + s(alpha, beta)+ s(r, alpha, beta) + s(mu), data = dat)
	} else {
		stop("Not a correct model")
	}

	return(list(prob = pfit, wel = wfit))
}

for (inst in instruments) {

	load(paste0(data_dir, inst, load_suffix))

	cat(paste("\nFitting for", inst),"\n")

	# Load the instrument into a known var
	idat <- get(inst)

	gam_fits <- list()

	gam_fits$EUT <- list(EUT = fit_gam(idat, "EUT",    "EUT", wel.var),
	                     POW = fit_gam(idat, "EUT",    "POW", wel.var),
	                     INV = fit_gam(idat, "EUT",    "INV", wel.var),
	                     PRE = fit_gam(idat, "EUT",    "PRE", wel.var))
	gam_fits$POW <- list(EUT = fit_gam(idat, "pow",    "EUT", wel.var),
	                     POW = fit_gam(idat, "pow",    "POW", wel.var),
	                     INV = fit_gam(idat, "pow",    "INV", wel.var),
	                     PRE = fit_gam(idat, "pow",    "PRE", wel.var))
	gam_fits$INV <- list(EUT = fit_gam(idat, "invs",   "EUT", wel.var),
	                     POW = fit_gam(idat, "invs",   "POW", wel.var),
	                     INV = fit_gam(idat, "invs",   "INV", wel.var),
	                     PRE = fit_gam(idat, "invs",   "PRE", wel.var))
	gam_fits$PRE <- list(EUT = fit_gam(idat, "prelec", "EUT", wel.var),
	                     POW = fit_gam(idat, "prelec", "POW", wel.var),
	                     INV = fit_gam(idat, "prelec", "INV", wel.var),
	                     PRE = fit_gam(idat, "prelec", "PRE", wel.var))

	fit_name <- paste0(inst, "_gam")

	assign(fit_name, gam_fits)

	save(list=fit_name, file=paste0(fit_dir, fit_name, save_suffix))
	cat("\n")

}




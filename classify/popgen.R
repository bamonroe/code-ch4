library(ctools)
c.library("cgen", "dplyr")

genpop <- function(run) {

	file_name <- paste0("subdat-", run, ".Rda")

	in_raw <- list.files(path="rawdat", pattern = paste0(run, ".Rda"))

	if (file.exists(file_name) | (length(in_raw) != 0)) {
		print(paste("Rawdat", run, "already exists"))
		return(NA)
	}

	print(paste("Generating rawdat", run))

	rmin <- -1
	rmax <- 0.95
	amin <- 0.1
	amax <- 2.0
	bmin <- 0.1
	bmax <- 2.0
	umax <- .30
	umin <- .01

	EUT.pop <- c(rmax = rmax, rmin = rmin, umax = umax, umin = umin, ru = 0)
	INV.pop <- c(rmax = rmax, rmin = rmin, amax = amax, amin = amin, umax = umax, us = umin, ra = 0, ru = 0, au = 0)
	POW.pop <- c(rmax = rmax, rmin = rmin, amax = amax, amin = amin, umax = umax, us = umin, ra = 0, ru = 0, au = 0)
	PRE.pop <- c(rmax = rmax, rmin = rmin, amax = amax, amin = amin, bm = bmax, bs = bmin, um = umax, us = umin, ra = 0, rb =0, ru = 0, au = 0, bu = 0, ab = 0)

	n <- 2000

	# How big are the populations for each model - in parts per parts per thousand
	EUT.N <- 1
	POW.N <- 1
	INV.N <- 1
	PRE.N <- 1

	prop <- EUT.N + POW.N + INV.N + PRE.N
	EUT.N <- round((EUT.N / prop) * n)
	POW.N <- round((POW.N / prop) * n)
	INV.N <- round((INV.N / prop) * n)
	PRE.N <- round((PRE.N / prop) * n)

	TOT.N <- 0

	# Which instrument
	inst <- c("SH", "HO", "LMS20", "LMS30", rep("HNG",13), "HNG.ins")

	# Which population(s)
	u.pop <- c("EUT", "POW", "INV", "PRE")

	# Generate our datasets
	if ("EUT" %in% u.pop & EUT.N > 0) {
		#print("In EUT")
		persub <- genChoice(EUT.pop, N = EUT.N, inst = inst, pfunc = "EUT", ufunc = "CRRA", unif = T)
		persub$alpha <- NA
		persub$beta  <- NA
		TOT.N <- TOT.N + EUT.N
	}
	if ("POW" %in% u.pop & POW.N > 0) {
		#print("In POW")
		if (!exists("persub")) {
			persub <- genChoice(POW.pop, N = POW.N, inst = inst, pfunc = "pow", ufunc = "CRRA", unif = T) %>%
				cbind(beta = NA)
		} else {
			persub <- genChoice(POW.pop, N = POW.N, inst = inst, pfunc = "pow", ufunc = "CRRA", unif = T) %>%
				cbind(beta = NA) %>%
				rbind(persub)
		}
		TOT.N <- TOT.N + POW.N
	}
	if ("INV" %in% u.pop & INV.N > 0) {
		#print("In INV")
		if (!exists("persub")) {
			persub <- genChoice(INV.pop, N = INV.N, inst = inst, pfunc = "invs", ufunc = "CRRA", unif = T) %>%
				cbind(beta = NA)
		} else {
			persub <- genChoice(INV.pop, N = INV.N, inst = inst, pfunc = "invs", ufunc = "CRRA", unif = T) %>%
				cbind(beta = NA) %>%
				rbind(persub)
		}
		TOT.N <- TOT.N + INV.N
	}
	if ("PRE" %in% u.pop & PRE.N > 0) {
		#print("In PRE")
		if (!exists("persub")) {
			persub <- genChoice(PRE.pop, N = PRE.N, inst = inst, pfunc = "prelec", ufunc = "CRRA", unif = T)
		} else {
			persub <- genChoice(PRE.pop, N = PRE.N, inst = inst, pfunc = "prelec", ufunc = "CRRA", unif = T) %>%
				rbind(persub)
		}
		TOT.N <- TOT.N + PRE.N
	}

	# Need to renumber IDs when binding
	# First grab number of tasks
	TT <- nrow(persub) / TOT.N
	# Replace every TT rows of ID with a counted ID number
	ID <- c()
	for (i in 1:TOT.N) {
		start <- ((i-1)*TT)+1
		end   <- (i*TT)
		ID <- c(ID, rep(i, TT))
	}

	persub$ID <- ID

	# list with each element a subject's data
	#print("Splitting")
	persub <- split(persub, persub$ID)

	#print("Saving")
	save(persub, file = paste0("subdat-", run, ".Rda"))

	rm(persub)
	gc()

	return(0)

}

c.export("genpop")

runs <- 25

#for (run in 1:runs) {
cap <- c.lapply(1:runs, function(run) {
	genpop(run)
})
#}

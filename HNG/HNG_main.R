library(ctools)
c.library("MSL", "welfare")

load("../data/HNG/HNG.Rda")

perSID <- split(x=DD, f=DD$ID)

indCalc <- function(dd, K) {

	HNG <- dd %>%
		filter(qid == "HNG")
	HNG.ins <- dd %>%
		filter(qid == "HNG.ins")

	# The format of the results of this function
	results <- list(failed = 0, sim = NA, EUT = 0, pow = 0, invs = 0, prelec = 0)

	optimizer <- "BHHH"

	eut.init    <- c(0.5, 0.1)
	pow.init    <- c(0.5, 1, 0.1)
	invs.init   <- c(0.5, 1, 0.1)
	prelec.init <- c(0.5, 1, 1, 0.1)

	# Be generous, start the optimizer at the real parameters
	EUT <- tryCatch(ML.optim(pars = eut.init, inst = HNG, pfunc = "EUT", optimizer = optimizer, try = F, report = F, clean = F),
					error = function(x) return(NA))

	rdu.real <- c(s.real[1], 1, s.real[2])
	pow <- tryCatch(ML.optim(pars = pow.init, inst = HNG, pfunc = "pow", optimizer = optimizer, try = F, report = F, clean = F),
					error = function(x) return(NA))

	invs <- tryCatch(ML.optim(pars = invs.init, inst = HNG, pfunc = "invs", optimizer = optimizer, try = F, report = F, clean = F),
						error = function(x) return(NA))

	rdu.real <- c(s.real[1], 1, 1, s.real[2])
	prelec <- tryCatch(ML.optim(pars = prelec.init, inst = HNG, pfunc = "prelec", optimizer = optimizer, try = F, report = F, clean = F),
							error = function(x) return(NA))

	POW.test <- F
	INVS.test <- F
	PRELEC.test <- F

	# Test if the RDU models show a difference from EUT
	if (!is.na(pow[1])) {
		est <- pow$estimates
		sd <- sqrt(diag(solve(-pow$hessian)))
		POW.test <- abs((est[2] - 1)/sd[2]) > 2
	}

	if (!is.na(invs[1])) {
		est <- invs$estimates
		sd <- sqrt(diag(solve(-invs$hessian)))
		INVS.test <- abs((est[2] - 1)/sd[2]) > 2
	}

	if (!is.na(prelec[1])) {
		est <- prelec$estimates
		sd <- sqrt(diag(solve(-prelec$hessian)))
		PRELEC0.test <- abs((est[2] - 1)/sd[2]) > 2
		PRELEC1.test <- abs((est[3] - 1)/sd[3]) > 2
		PRELEC.test <- PRELEC0.test | PRELEC1.test
	}

	# If the RDU model doesn't have converging SE, then the test is false
	if (is.na(POW.test))    RDU.test <- F
	if (is.na(INVS.test))   INVS.test <- F
	if (is.na(PRELEC.test)) PRELEC.test <- F

	# If EUT Exists, or if RDU is different from EUT, get the likelihoods
	likes <- c()
	likes[1] <- ifelse(!is.na(EUT[1]), EUT$likelihood, NA)
	likes[2] <- ifelse(POW.test, pow$likelihood, NA)
	likes[3] <- ifelse(INVS.test, invs$likelihood, NA)
	likes[4] <- ifelse(PRELEC.test, prelec$likelihood, NA)

	# If everything is NA return a bunch of NAs

	if(all(is.na(likes))) {
		results$failed <- 1
		return(NA)
	}

	# Get the EUT model and the RDU models that are different from EUT ranked
	mods  <- c("EUT", "pow", "invs", "prelec")
	mods  <- mods[!is.na(likes)]
	likes <- likes[!is.na(likes)]
 
 	mods  <- mods[order(likes, decreasing = T)]
 	likes <- likes[order(likes, decreasing = T)]

	# After the sorting above, the first element of mods contains the winning model
	# This is either EUT or the RDU model that is different from EUT with the highest likelihood
	mod <- mods[1]

	# Use get to get the object of the same name
	m <- get(mod)

	# Will need to transform the real parameters into what the likelihood function would see
	r.par <- s.real
	r.par[2] <- log(r.par[2]^3)

	# Point Estimates of Individual Estimation
	indw <- welCalc(inst = HNG.ins, pars = m$estimates, pfunc = mod, boot = F)
	# Bootstrap Estimates of Individual Estimation
	bindw <- tryCatch(welCalc(inst = HNG.ins, pars = m$estimates, hessian = m$hessian, pfunc = mod, boot = T),
										error = function(x) return(NA))

	# If we were able to bootstrap, use the bootstrap
	if (!is.na(bindw[1])) indw <- bindw

	# Solve the hessian to get the covariance matrix of the estimators
	# Point Estimates
	simw <- tryCatch(welCalc.sim(inst = HNG.ins, pars = K$estimates, pfunc = "EUT", boot = F),
					 error = function(x) return(NA))
	# Boot Estimates
	#welCalc.sim(inst = d, pars = K$estimates, hessian = K$hessian, pfunc = "EUT", boot = T)

	RES <- list(Ind.wel = indw, Sim.wel = simw, ind.mod = mod, ind.est = m)

	return(RES)


}


# Some configurations
hh   <- 500    # Halton Draws
opt  <- "NR" # optimizer Method
inum <- 150    # Iteration limit

MSL.res <- list()

MSL.eut.init    <- c(rm = 0.5, rs = 0.5, um = 0.3, us = 0.1, ru = 0.2)
MSL.pow.init    <- c(rm = 0.5, rs = 0.5, am = 1.5, as = 0.1, um = 0.3, us = 0.1, ra = 0.5, ru = 0, au = 0)
MSL.invs.init   <- c(rm = 0.5, rs = 0.5, am = 1.5, as = 0.1, um = 0.3, us = 0.1, ra = 0.5, ru = 0, au = 0)
MSL.prelec.init <- c(rm = 0.5, rs = 0.5, am = 1.5, as = 0.1, bm = 0.5, bs = 0.1, um = 0.3, us = 0.1, ra = 0.5, rb = 0.2, ab = 0, ru = 0, au = 0, bu = 0)

MSL.res[["EUT"]]    <- MSL.optim(pars = MSL.eut.init,    inst = DD, pfunc = "EUT", iterations = inum, HH = hh, optimizer = opt)
#MSL.res[["pow"]]    <- MSL.optim(pars = MSL.pow.init,    inst = DD, pfunc = "EUT", iterations = inum, HH = hh, optimizer = opt)
#MSL.res[["invs"]]   <- MSL.optim(pars = MSL.invs.init,   inst = DD, pfunc = "EUT", iterations = inum, HH = hh, optimizer = opt)
#MSL.res[["prelec"]] <- MSL.optim(pars = MSL.prelec.init, inst = DD, pfunc = "EUT", iterations = inum, HH = hh, optimizer = opt)

c.lapply(indCalc, perSID, K = MSL.res[["EUT"]])

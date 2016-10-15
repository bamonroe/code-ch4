
exp3      <- function(x) {exp(x)^(1/3)}
exp3.inv  <- function(y) {log(y^3)}
m1to1     <- function(x) {(exp(x) / (1+exp(x))) * 2 - 1}
m1to1.inv <- function(y) {log(-(y+1) / (y-1))}

ot <- function(pars, transforms) {
	ifelse(transforms == 1, exp3(pars),
	ifelse(transforms == 2, m1to1(pars),
				 pars))
}

it <- function(pars, transforms) {
	ifelse(transforms == 1, exp3.inv(pars),
	ifelse(transforms == 2, m1to1.inv(pars),
				 pars))
}

runInd <- function(d, K) {

#	dbug <- 1
#	print(paste("Here:", dbug)) ; dbug <- dbug + 1

	# The format of the results of this function
	results <- list(failed = 0, sim = NA, EUT = 0, pow = 0, invs = 0, prelec = 0)

	optimizer <- "NR"

	# What are the real EUT parameters
	s.real <- c(d$r[1], d$mu[1])

	# Be generous, start the optimizer at the real parameters
	EUT <- tryCatch(ML.optim(pars = s.real, inst = d, pfunc = "EUT", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	rdu.real <- c(s.real[1], 1, s.real[2])
	pow <- tryCatch(ML.optim(pars = rdu.real, inst = d, pfunc = "pow", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	invs <- tryCatch(ML.optim(pars = rdu.real, inst = d, pfunc = "invs", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	rdu.real <- c(s.real[1], 1, 1, s.real[2])
	prelec <- tryCatch(ML.optim(pars = rdu.real, inst = d, pfunc = "prelec", optimizer = optimizer, try = F, report = F, clean = F), 
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
	if (is.na(POW.test)) RDU.test <- F
	if (is.na(INVS.test)) INVE.test <- F
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
		return(unlist(results))
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

	# Real Welfare of the individual
	rwel <- welCalc(inst = d, pars = r.par, pfunc = "EUT", boot = F)

	# Point Estimates of Individual Estimation
	indw <- welCalc(inst = d, pars = m$estimates, pfunc = mod, boot = F)
	# Bootstrap Estimates of Individual Estimation
	bindw <- tryCatch(welCalc(inst = d, pars = m$estimates, hessian = m$hessian, pfunc = mod, boot = T),
										error = function(x) return(NA))

	# If we were able to bootstrap, use the bootstrap
	if (!is.na(bindw[1])) indw <- bindw

	# Solve the hessian to get the covariance matrix of the estimators
	# Point Estimates
	simw <- tryCatch(welCalc.sim(inst = d, pars = K$estimates, pfunc = "EUT", boot = F),
					 error = function(x) return(NA))
	# Boot Estimates
	#welCalc.sim(inst = d, pars = K$estimates, hessian = K$hessian, pfunc = "EUT", boot = T)

	idiff <- abs(rwel - indw)
	sdiff <- abs(rwel - simw)

	if (is.na(idiff[1] > sdiff[1])) {
		results$sim <- NA
	} else if (idiff[1] > sdiff[1]) {
		print(paste("Simulated is closer to Real:", mod))
		results$sim <- 0
	} else {
		print(paste("Individual is closer to Real:", mod))
		results$sim <- 1
	}

	if (mod == "EUT")
		results$EUT <- 1 
	else if (mod == "pow")
		results$pow <- 1 
	else if (mod == "invs")
		results$invs <- 1 
	else if (mod == "prelec")
		results$prelec <- 1 

	return(unlist(results))

}



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

	# What are the real EUT parameters
	s.real <- c(d$r[1], d$mu[1])

	# Be generous, start the optimizer at the real parameters
	m <- tryCatch(ML.optim(pars = s.real, inst = d, pfunc = "EUT", optimizer = "BFGS", try = F, report = F), 
					 error = function (x) NA)

	if (is.na(m[1])) return(NA)

	# Will need to transform the real parameters into what the likelihood function would see
	r.par <- s.real
	r.par[2] <- log(r.par[2]^3)

	# Real Welfare of the individual
	rwel <- welCalc(inst = d, pars = r.par, pfunc = "EUT", boot = F)

	# Point Estimates of Individual Estimation
	indw <- welCalc(inst = d, pars = m$estimates, pfunc = "EUT", boot = F)
	# Bootstrap Estimates of Individual Estimation
	bindw <- tryCatch(welCalc(inst = d, pars = m$estimates, hessian = m$hessian, pfunc = "EUT", boot = T),
										error = function (x) NA )

	# If we were able to bootstrap, lets bootstrap
	if (!is.na(bindw[1])) indw <- bindw

#	# Get the transformed populaiton level parameters
#	transforms.eut <- c(0,1,1,1,2)
#	sim.eut <- it(sim.eut, transforms.eut)
#
#	# The welfare using the "real" population parameters
#	welCalc.sim(inst = d, pars = sim.eut, pfunc = "EUT", boot = F)


	# Solve the hessian to get the covariance matrix of the estimators
	# Point Estimates
	simw <- welCalc.sim(inst = d, pars = K$estimates, pfunc = "EUT", boot = F)
	# Boot Estimates
	welCalc.sim(inst = d, pars = K$estimates, hessian = K$hessian, pfunc = "EUT", boot = T)

	if (is.na(indw[1]) | is.na(simw[1])) return(NA)

	idiff <- abs(rwel - indw)
	sdiff <- abs(rwel - simw)

	if (idiff[1] > sdiff[1]) {
		print("Simulated is closer to Real")
		return(1)
	} else {
		print("Individual is closer to Real")
		return(0)
	}

}

runSimEUT <- function(sim.eut) {

	N <- 200
	H <- 250
	iter <- 1000
	control <- list()

	names(sim.eut) <- c("rm", "rs", "um", "us", "ru")

	D <- genChoice(sim.eut, N, inst="HNG", pfunc = "EUT")
	K <- MSL.optim(pars = sim.eut, inst = D, pfunc = "EUT",  HH = H, optimizer = "BFGS", iterations = iter, try = F, control = control)
	#save(D, K, file="optimres.Rda")
	#load("optimres.Rda")

	perSID <- split(x=D, f=D$ID)

	sim_ind <- lapply(perSID, runInd, K = K)

	avg <- mean(unlist(sim_ind), na.rm = T)

	return(avg)

}

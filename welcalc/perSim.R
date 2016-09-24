
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
	n <- tryCatch(ML.optim(pars = s.real, inst = d, pfunc = "EUT", optimizer = "BFGS", try = F, report = F), 
					 error = function(x) return(NA))

	rdu.real <- c(s.real[1], 1, s.real[2])
	#o <- tryCatch(ML.optim(pars = rdu.real, inst = d, pfunc = "pow", optimizer = "BFGS", try = F, report = F), 
	#				 error = function(x) return(NA))
	o <- NA

	if (is.na(n[1]) & is.na(o[1])) {
		return(NA)
	} else if (is.na(n[1])) {
		m <- o
		mod <- "pow"
	} else if (is.na(o[1])) {
		m <- n
		mod <- "EUT"
	} else if (n$likelihood > o$likelihood) {
		m <- n
		mod <- "EUT"
	} else if (o$likelihood > n$likelihood) {
		m <- o
		mod <- "pow"
	}

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
	simw <- tryCatch(welCalc.sim(inst = d, pars = K$estimates, pfunc = "EUT", boot = F),
					 error = function(x) return(NA))
	# Boot Estimates
	#welCalc.sim(inst = d, pars = K$estimates, hessian = K$hessian, pfunc = "EUT", boot = T)

	if (is.na(indw[1]) & is.na(simw[1])) {
		return(NA)
	} else if (is.na(indw[1])) {
		return(1)
	} else if (is.na(simw[1])) {
		return(0)
	}

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
	H <- 350
	iter <- 100
	control <- list()

	names(sim.eut) <- c("rm", "rs", "um", "us", "ru")

	D <- genChoice(sim.eut, N, inst="HNG", pfunc = "EUT")

	K <- tryCatch(MSL.optim(pars = sim.eut, inst = D, pfunc = "EUT",  HH = H, optimizer = "BFGS", iterations = iter, try = F, control = control),
				  error = function(x) return(NA))

	if (is.na(K[1])) return(NULL)
	#save(D, K, file="optimres.Rda")
	#load("optimres.Rda")

	perSID <- split(x=D, f=D$ID)

	sim_ind <- lapply(perSID, runInd, K = K)

	avg <- mean(unlist(sim_ind), na.rm = T)

	print(avg)

	return(avg)

}

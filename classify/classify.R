
runInd <- function(d) {

#	dbug <- 1
#	print(paste("Here:", dbug)) ; dbug <- dbug + 1

	# The format of the results of this function
	results <- list(failed = 0, correct = 0, EUT = 0, pow = 0, invs = 0, prelec = 0)

	optimizer <- "NR"

	# What are the real EUT parameters
	if ( d$mod[1] == "EUT" ) {
		eut.init    <- c(d$r[1], d$mu[1])
		pow.init    <- c(d$r[1], 1, d$mu[1])
		invs.init   <- c(d$r[1], 1, d$mu[1])
		prelec.init <- c(d$r[1], 1, 1, d$mu[1])
	} else if (d$mod[1] == "pow") {
		eut.init    <- c(d$r[1], d$mu[1])
		pow.init    <- c(d$r[1], d$alpha[1], d$mu[1])
		invs.init   <- c(d$r[1], d$alpha[1], d$mu[1])
		prelec.init <- c(d$r[1], d$alpha[1], 1, d$mu[1])
	} else if (d$mod[1] == "invs") {
		eut.init    <- c(d$r[1], d$mu[1])
		pow.init    <- c(d$r[1], d$alpha[1], d$mu[1])
		invs.init   <- c(d$r[1], d$alpha[1], d$mu[1])
		prelec.init <- c(d$r[1], 1, d$alpha[1], d$mu[1])
	} else if (d$mod[1]== "prelec") {
		eut.init    <- c(d$r[1], d$mu[1])
		pow.init    <- c(d$r[1], d$alpha[1], d$mu[1])
		invs.init   <- c(d$r[1], d$beta[1], d$mu[1])
		prelec.init <- c(d$r[1], d$alpha[1], d$beta[1], d$mu[1])
	}


	# Be generous, start the optimizer at the real parameters
	EUT <- tryCatch(ML.optim(pars = eut.init, inst = d, pfunc = "EUT", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	pow <- tryCatch(ML.optim(pars = pow.init, inst = d, pfunc = "pow", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	invs <- tryCatch(ML.optim(pars = invs.init, inst = d, pfunc = "invs", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	prelec <- tryCatch(ML.optim(pars = prelec.init, inst = d, pfunc = "prelec", optimizer = optimizer, try = F, report = F, clean = F), 
					 error = function(x) return(NA))

	POW.test    <- F
	INVS.test   <- F
	PRELEC.test <- F

	conf.num <- 2
	# Test if the RDU models show a difference from EUT
	if (!is.na(pow[1])) {
		est <- pow$estimates
		sd <- sqrt(diag(solve(-pow$hessian)))
		if (any(is.na(sd) | is.nan(sd))) {
			POW.test <- F
		} else{
			POW.test <- abs((est[2] - 1)/sd[2]) > conf.num
		}
	}

	if (!is.na(invs[1])) {
		est <- invs$estimates
		sd <- sqrt(diag(solve(-invs$hessian)))
		if (any(is.na(sd) | is.nan(sd))) {
			INVS.test <- F
		} else{
			INVS.test <- abs((est[2] - 1)/sd[2]) > conf.num
		}
	}

	if (!is.na(prelec[1])) {
		est <- prelec$estimates
		sd <- sqrt(diag(solve(-prelec$hessian)))
		if (any(is.na(sd) | is.nan(sd))) {
			PRELEC.test <- F
		} else{
			PRELEC0.test <- abs((est[2] - 1)/sd[2]) > conf.num
			PRELEC1.test <- abs((est[3] - 1)/sd[3]) > conf.num
			PRELEC.test <- PRELEC0.test | PRELEC1.test
		}
	}

	# If the RDU model doesn't have converging SE, then the test is false
	if (is.na(POW.test) | is.nan(POW.test)) RDU.test <- F
	if (is.na(INVS.test) | is.nan(INVS.test)) INVE.test <- F
	if (is.na(PRELEC.test) | is.nan(PRELEC.test)) PRELEC.test <- F

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

	# What is the real mod?
	rmod <- d$mod[1]

	if (mod == rmod) {
		results$correct <- 1
		print(paste0("Correct:   ", rmod, " ", mod))
	} else {
		results$correct <- 0
		print(paste0("Incorrect: ", rmod, " ", mod))
	}

	# Use get to get the object of the same name
	m <- get(mod)

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


do.optimx <- function(par, N, HR, HU, A, B, pA, pB, Max, Min, choice, 
					 config = list(method="Nelder-Mead", UH = "Unknown", HH = ncol(HR)), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T)){

	m <- tryCatch(optimx(par = par, fn = MSL_EUT, 
						 h1 = HR, h2 = HU, 
						 A = A, B = B, pA = pA, pB = pB, 
						 Max = Max, Min = Min, choice = choice, 
						 method=config$method, hessian = T, control=control),
				error = function(x){NULL})

	hess <- attr(m, "details")[config$method,"nhatend"][[1]]
	fisher <- tryCatch(solve(hess), 
				error = function(x){NULL})


	# Split out the hessian to return it piece by piece
	hrm <- hess[,1]
	hrs <- hess[,2]
	hum <- hess[,3]
	hus <- hess[,4]

	# Get the square root of it
	se <- sqrt(diag(fisher))

	# Retireve the solve parameters
	spars <- do.call(c,m[1,1:length(par)])

	# Get the 95% confidence interval
	up <- spars + 1.96*se
	low <- spars - 1.96*se

	# Get the t stat
	t <- spars / se

	# Get the p-values
	pval<-2*(1-pt(abs(t),nrow(A)-length(par)))

	# Adjust for the logged parameters
	tt <-  spars
	tt[2:4] <- exp(tt[2:4])^(1/3)
	#tt[2:4] <- abs(tt[2:4])

	ts <- se
	#ts[2:4] <- abs(se[2:4])
	ts[2:4] <- exp(ts[2:4])^(1/3)

	tu <- up
	#tu[2:4] <- abs(tu[2:4])
	tu[2:4] <- exp(tu[2:4])^(1/3)

	tl <- low
	tl[2:4] <- exp(tl[2:4])^(1/3)

	start <- par
	start[2:4] <- exp(par[2:4])^(1/3)

	# Save everything in a convienient place
	if( is.null(fisher) ){
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, llike = m$value, 
						Subjects = config$N, Obs = config$Obs, H = config$HH)
	}else{
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, par = tt,
						se = ts, lower = tl, upper = tu, pvalue = pval, llike = m$value, 
						Subjects = config$N, Obs = config$Obs, H = config$HH)
	}

	# Add in the hessian matrix
	mm$hrm <- hrm
	mm$hrs <- hrs
	mm$hum <- hum
	mm$hus <- hus

	rownames(mm) <- c("rm","rs","um","us")
	# Print these things out
	print(mm)
	print("")
	return(mm)

}

do.optim <- function(par, N, HR, HU, A, B, pA, pB, Max, Min, choice, 
					 config = list(method="Nelder-Mead", UH = "Unknown", HH = ncol(HR)), 
					 control= list(trace=2, REPORT = 1)){

	m <- tryCatch(optim(par = par, fn = MSL_EUT, 
						 h1 = HR, h2 = HU, 
						 A = A, B = B, pA = pA, pB = pB, 
						 Max = Max, Min = Min, choice = choice, 
						 method=config$method, hessian = F, control=control), 
				  error = function(x){NULL})

	hes <- numDeriv::hessian(func = MSL_EUT, x = par, h1 = HR, h2 = HU, A = A, B = B, pA = pA, pB = pB, Max = Max, Min = Min, choice = choice)

	fisher <- tryCatch(solve(hes), 
				error = function(x){NULL})

	# Get the square root of it
	se <- sqrt(diag(fisher))

	# Retireve the solve parameters
	spars <- m$par

	# Get the 95% confidence interval
	up <- spars + 1.96*se
	low <- spars - 1.96*se

	# Get the t stat
	t <- spars / se

	# Get the p-values
	pval<-2*(1-pt(abs(t),nrow(A)-length(par)))

	# Adjust for the logged parameters
	tt <-  spars
	tt[2:4] <- exp(tt[2:4])^(1/3)
	#tt[2:4] <- abs(tt[2:4])

	ts <- se
	#ts[2:4] <- abs(se[2:4])
	ts[2:4] <- exp(ts[2:4])^(1/3)

	tu <- up
	#tu[2:4] <- abs(tu[2:4])
	tu[2:4] <- exp(tu[2:4])^(1/3)

	tl <- low
	tl[2:4] <- exp(tl[2:4])^(1/3)

	start <- par
	start[2:4] <- exp(par[2:4])^(1/3)

	# Save everything in a convienient place
	if( is.null(fisher) ){
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, llike = m$value, 
						Subjects = config$N, Obs = config$Obs, H = config$HH)
	}else{
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, par = tt,
						se = ts, lower = tl, upper = tu, pvalue = pval, llike = m$value, 
						Subjects = config$N, Obs = config$Obs, H = config$HH)
	}

	# Print these things out
	print(mm)
	print("")
	return(mm)

}


do.optimx2 <- function(par, HR, HU, inst,
					 config = list(method="Nelder-Mead", UH = "Unknown", HH = ncol(HR)), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T)){

	perSID <- split(x=inst, f=inst$SID)

	INST <- lapply(perSID,function(x){
							A <- x %>%
									select(starts_with("A")) %>%
									as.matrix()
							B <- x %>%
									select(starts_with("B")) %>%
									as.matrix()
							pA <- x %>%
									select(starts_with("pA")) %>%
									as.matrix()
							pB <- x %>%
									select(starts_with("pB")) %>%
									as.matrix()
							Min    <- as.vector(x$Min)
							Max    <- as.vector(x$Max)
							choice <- as.vector(x$choice)
							SID    <- as.vector(x$SID)

							list(A=A, B=B, pA=pA, pB=pB, Min=Min, Max=Max, choice=choice, SID=SID)
					 
					 })

	#m <- tryCatch(optimx(par = par, fn = MSL_EUT, 
	m <- optimx(par = par, fn = MSL_EUT, 
						 h1 = HR, h2 = HU, 
						 Inst = INST,
						 method=config$method, hessian = T, control=control)
#				error = function(x){NULL})

	hess <- attr(m, "details")[config$method,"nhatend"][[1]]
	fisher <- tryCatch(solve(hess), 
				error = function(x){NULL})


	# Split out the hessian to return it piece by piece
	hrm <- hess[,1]
	hrs <- hess[,2]
	hum <- hess[,3]
	hus <- hess[,4]

	# Get the square root of it
	se <- sqrt(diag(fisher))

	# Retireve the solve parameters
	spars <- do.call(c,m[1,1:length(par)])

	# Get the 95% confidence interval
	up <- spars + 1.96*se
	low <- spars - 1.96*se

	# Get the t stat
	t <- spars / se

	# Get the p-values
	pval<-2*(1-pt(abs(t),nrow(inst)-length(par)))

	# Adjust for the logged parameters
	tt <-  spars
	tt[2:4] <- exp(tt[2:4])^(1/3)
	#tt[2:4] <- abs(tt[2:4])

	ts <- se
	#ts[2:4] <- abs(se[2:4])
	ts[2:4] <- exp(ts[2:4])^(1/3)

	tu <- up
	#tu[2:4] <- abs(tu[2:4])
	tu[2:4] <- exp(tu[2:4])^(1/3)

	tl <- low
	tl[2:4] <- exp(tl[2:4])^(1/3)

	start <- par
	start[2:4] <- exp(par[2:4])^(1/3)

	# Save everything in a convienient place
	if( is.null(fisher) ){
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, llike = m$value, 
						Subjects = config$N, Obs = config$Obs, H = config$HH)
	}else{
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, par = tt,
						se = ts, lower = tl, upper = tu, pvalue = pval, llike = m$value, 
						Subjects = length(perSID), Obs = nrow(inst), H = config$HH)
	}

	# Add in the hessian matrix
	mm$hrm <- hrm
	mm$hrs <- hrs
	mm$hum <- hum
	mm$hus <- hus

	rownames(mm) <- c("rm","rs","um","us")
	# Print these things out
	print(mm)
	print("")
	return(mm)

}

do.optimx3 <- function(par, HR, HU, inst,
					 config = list(method="Nelder-Mead", UH = "Unknown", HH = ncol(HR)), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T)){

	perSID <- split(x=inst, f=inst$SID)

	INST <- lapply(perSID,function(x){
							A <- x %>%
									select(starts_with("A")) %>%
									as.matrix()
							B <- x %>%
									select(starts_with("B")) %>%
									as.matrix()
							pA <- x %>%
									select(starts_with("pA")) %>%
									as.matrix()
							pB <- x %>%
									select(starts_with("pB")) %>%
									as.matrix()
							Min    <- as.vector(x$Min)
							Max    <- as.vector(x$Max)
							choice <- as.vector(x$choice)
							SID    <- as.vector(x$SID)

							list(A=A, B=B, pA=pA, pB=pB, Min=Min, Max=Max, choice=choice, SID=SID)
					 
					 })

	m <- optimx(par = par, fn = MSL_EUT, 
#	m <- tryCatch(optimx(par = par, fn = MSL_EUT, 
						 h1 = HR, h2 = HU, 
						 Inst = INST,
						 method=config$method, hessian = T, control=control)
	#			error = function(x){NULL})

	hess <- attr(m, "details")[config$method,"nhatend"][[1]]
	fisher <- tryCatch(solve(hess), 
				error = function(x){NULL})


	if (is.na(hess) | is.null(hess)){
		return(NULL)
	}


	# Split out the hessian to return it piece by piece
	hrm <- hess[,1]
	hrs <- hess[,2]
	hum <- hess[,3]
	hus <- hess[,4]
	hrh <- hess[,5]

	# Get the square root of it
	se <- sqrt(diag(fisher))

	# Retireve the solve parameters
	spars <- do.call(c,m[1,1:length(par)])

	# Get the 95% confidence interval
	up <- spars + 1.96*se
	low <- spars - 1.96*se

	# Get the t stat
	t <- spars / se

	# Get the p-values
	pval<-2*(1-pt(abs(t),nrow(inst)-length(par)))

	# Adjust for the logged parameters
	tt <-  spars
	tt[2:4] <- exp(tt[2:4])^(1/3)
	tt[5] <- (exp(tt[5]) / ( 1 + exp(tt[5]))) * 2 - 1

	ts <- se
	#ts[2:4] <- abs(se[2:4])
	ts[2:4] <- exp(ts[2:4])^(1/3)
	ts[5] <- (exp(ts[5]) / ( 1 + exp(ts[5]))) * 2 - 1

	tu <- up
	#tu[2:4] <- abs(tu[2:4])
	tu[2:4] <- exp(tu[2:4])^(1/3)
	tu[5] <- (exp(tu[5]) / ( 1 + exp(tu[5]))) * 2 - 1

	tl <- low
	tl[2:4] <- exp(tl[2:4])^(1/3)
	tl[5] <- (exp(tl[5]) / ( 1 + exp(tl[5]))) * 2 - 1

	start <- par
	start[2:4] <- exp(par[2:4])^(1/3)
	start[5] <- (exp(par[5]) / ( 1 + exp(par[5]))) * 2 - 1

	# Save everything in a convienient place
	if( is.null(fisher) ){
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, llike = m$value, 
						Subjects = config$N, Obs = config$Obs, H = config$HH)
	}else{
		mm <- data.frame(poppar = config$poppar, sampar = config$sampar, init = start, est = spars, par = tt,
						se = ts, lower = tl, upper = tu, pvalue = pval, llike = m$value, 
						Subjects = length(perSID), Obs = nrow(inst), H = config$HH)
	}

	# Add in the hessian matrix
	mm$hrm <- hrm
	mm$hrs <- hrs
	mm$hum <- hum
	mm$hus <- hus
	mm$hrh <- hrh

	rownames(mm) <- c("rm","rs","um","us","rh")
	# Print these things out
	print(mm)
	print("")
	return(mm)

}


try.optimx.MSL <- function(par, HH, inst,
					 config = list(method="Nelder-Mead", is.SIM=FALSE), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	tryCatch(do.optimx.MSL(par, HH, inst, config, control),
			 error= function(x){paste0("Error with parameters ",par)}
			 )

}

do.optimx.MSL <- function(par, HH, inst,
					 config = list(method="Nelder-Mead", is.SIM=FALSE), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	if(exists("is.SIM", where=config)){
		if(config$is.SIM){
			# Keep track of what initial par-set we're on
			initnum <- par[6]
		}
	}

	par <- par[1:5]

	# Split the passed instrument into a list by subject ID
	perSID <- split(x=inst, f=inst$SID)

	# Split each element of the list into a list of the important elements
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

	# Number of Subjects
	N <- length(perSID)

	# Generate Halton Sequences
	HR <- matrix(hunif(HH*N ,prime = 13, burn=45 ), nrow = N, ncol = HH, byrow=F)
	HU <- matrix(hunif(HH*N ,prime = 17, burn=45 ), nrow = N, ncol = HH, byrow=F)

	# Run the optimization
	m <- optimx(par = par, fn = MSL_EUT, 
						 h1 = HR, h2 = HU, 
						 Inst = INST,
						 method=config$method, hessian = T, control=control)

	# Retrieve and solve the hessian matrix
	hess <-attr(m, "details")[config$method,"nhatend"][[1]]
	fisher <- solve(hess)

	# If the hessian can't be solved (isn't positive semi-definite) ignore these results, they're not a real optimum
	tryCatch(
	if (is.na(hess) | is.null(hess) | is.na(fisher) | is.null(fisher)){
		return(NULL)
	} , warning = function(x){})

	# Split out the hessian to return it piece by piece
	hrm <- hess[,1]
	hrs <- hess[,2]
	hum <- hess[,3]
	hus <- hess[,4]
	hrh <- hess[,5]

	# Get the square root of it
	se <- sqrt(diag(fisher))

	# Retrieve the solve parameters
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

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = m$value, 
					Subjects = N, Obs = nrow(inst), H = HH)

	# Add in the hessian matrix
	mm$hrm <- hrm
	mm$hrs <- hrs
	mm$hum <- hum
	mm$hus <- hus
	mm$hrh <- hrh

	# Cbind stuff if it exists
	if(exists("reals",where=config)){
		mm <- cbind(config$reals, mm)
	}

	rownames(mm) <- c("rm","rs","um","us","rh")
	# Print these things out

	if(exists("is.SIM", where=config)){
		if(config$is.SIM){
			# Keep track of what initial par-set we're on
			mesg <- paste0("Simnum: ",config$simnum," Init#: ",initnum)
			cat("\n")
			cat(mesg)
		}
	}

	cat("\n")
	print(mm)
	return(mm)

}

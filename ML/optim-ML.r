
try.optimx.ML <- function(par,inst,
					 config = list(method="Nelder-Mead"),  
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	tryCatch(do.optimx.ML(par, inst, config, control),
			 error= function(x){paste0("Error with parameters ",par)}
			 )

}

do.optimx.ML <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	# Run the optimization
	m <- optimx(par = par, fn = ML_EUT, 
						 Inst = inst,
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
	hr <- hess[,1]
	hu <- hess[,2]

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
	pval<-2*(1-pt(abs(t),nrow(inst$A)-length(par)))

	# Adjust for the logged parameters
	tt <-  spars
	tt[2] <- exp(tt[2])^(1/3)

	ts <- se
	ts[2] <- exp(ts[2])^(1/3)

	tu <- up
	tu[2] <- exp(tu[2])^(1/3)

	tl <- low
	tl[2] <- exp(tl[2])^(1/3)

	start <- par
	start[2] <- exp(par[2])^(1/3)



	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = m$value, 
					Subjects = (nrow(inst$A)/80), Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r <- hr
	mm$hess.mu <- hu

	rownames(mm) <- c("r","mu")
	# Print these things out
	print(mm)
	return(mm)

}

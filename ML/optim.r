try.optimx.ML <- function(par, inst, model = "EUT",
					 config = list(method="Nelder-Mead" ),  
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	if( model == "EUT"){
		tryCatch(do.optimx.ML.EUT(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})

	}else if( model == "rEUT" ){

		tryCatch(do.optimx.ML.rEUT(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})
	
	}else if( model == "POW" ){

		tryCatch(do.optimx.ML.POW(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})

	}else if( model == "PRE" ){

		tryCatch(do.optimx.ML.POW(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})
	
	}

}

do.optimx.ML.EUT <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ML_EUT

	m <- optimx(par = par, fn = model, Inst = inst,
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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Subjects = (nrow(inst$A)/80), Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r <- hr
	mm$hess.mu <- hu

	rownames(mm) <- c("r","mu")

	# Print these things out
	print(mm)
	return(mm)

}

do.optimx.ML.rEUT <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ml_eut

	m <- optimx(par = par, fn = model, Inst = inst,
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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Subjects = (nrow(inst$A)/80), Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r <- hr
	mm$hess.mu <- hu

	rownames(mm) <- c("r","mu")

	# Print these things out
	print(mm)
	return(mm)

}

do.optimx.ML.POW <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ML_RDU_POW

	m <- optimx(par = par, fn = model, Inst = inst,
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
	ha <- hess[,3]

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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r     <- hr
	mm$hess.mu    <- hu
	mm$hess.alpha <- ha

	rownames(mm) <- c("r","mu","alpha")

	# Print these things out
	print(mm)
	return(mm)

}

do.optimx.ML.POW <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ML_RDU_PRE

	m <- optimx(par = par, fn = model, Inst = inst,
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
	ha <- hess[,3]
	hb <- hess[,3]

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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r     <- hr
	mm$hess.mu    <- hu
	mm$hess.alpha <- ha
	mm$hess.beta  <- hb

	rownames(mm) <- c("r","mu","alpha","beta")

	# Print these things out
	print(mm)
	return(mm)

}







try.optimx.MLsim <- function(par, inst, model = "EUT",
					 config = list(method="Nelder-Mead" ),  
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	if( model == "EUT"){
		tryCatch(do.optimx.ML.EUTsim(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})

	}else if( model == "rEUT" ){

		tryCatch(do.optimx.ML.rEUTsim(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})
	
	}else if( model == "POW" ){

		tryCatch(do.optimx.ML.POWsim(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})

	}else if( model == "PRE" ){

		tryCatch(do.optimx.ML.POWsim(par, inst, config, control),
				error= function(x){paste0("Error with parameters ",par)})
	
	}

}

do.optimx.ML.EUTsim <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ML_EUT

	m <- optimx(par = par, fn = model, Inst = inst,
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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	real <- c(mean(inst$r),mean(inst$mu))

	# Save everything in a convenient place
	mm <- data.frame(real=real, init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Subjects = (nrow(inst$A)/80), Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r <- hr
	mm$hess.mu <- hu

	rownames(mm) <- c("r","mu")

	# Print these things out
	print(mm)
	return(mm)

}

do.optimx.ML.rEUTsim <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ml_eut

	m <- optimx(par = par, fn = model, Inst = inst,
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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	real <- c(mean(inst$r),mean(inst$mu))

	# Save everything in a convenient place
	mm <- data.frame(real=real, init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Subjects = (nrow(inst$A)/80), Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r <- hr
	mm$hess.mu <- hu

	rownames(mm) <- c("r","mu")

	# Print these things out
	print(mm)
	return(mm)

}


do.optimx.ML.POWsim <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ML_RDU_POW

	m <- optimx(par = par, fn = model, Inst = inst,
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
	ha <- hess[,3]

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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r     <- hr
	mm$hess.mu    <- hu
	mm$hess.alpha <- ha

	rownames(mm) <- c("r","mu","alpha")

	# Print these things out
	print(mm)
	return(mm)

}

do.optimx.ML.POWsim <- function(par, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)){

	model <- ML_RDU_PRE

	m <- optimx(par = par, fn = model, Inst = inst,
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
	ha <- hess[,3]
	hb <- hess[,3]

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
	tt[2:length(par)] <- exp(tt[2:length(par)])^(1/3)

	ts <- se
	ts[2:length(par)] <- exp(ts[2:length(par)])^(1/3)

	tu <- up
	tu[2:length(par)] <- exp(tu[2:length(par)])^(1/3)

	tl <- low
	tl[2:length(par)] <- exp(tl[2:length(par)])^(1/3)

	start <- par
	start[2:length(par)] <- exp(par[2:length(par)])^(1/3)

	# Save everything in a convenient place
	mm <- data.frame(init = start, est = spars, par = tt,
					se = ts, lower = tl, upper = tu, pvalue = pval, llike = -m$value, 
					Obs = nrow(inst$A))

	# Add in the hessian matrix
	mm$hess.r     <- hr
	mm$hess.mu    <- hu
	mm$hess.alpha <- ha
	mm$hess.beta  <- hb

	rownames(mm) <- c("r","mu","alpha","beta")

	# Print these things out
	print(mm)
	return(mm)

}

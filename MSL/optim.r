library(dplyr)
library(halton)
library(optimx)
## Useful Functions

# Function to split traditional "FULL-Long" datasets to per subject "Long" list
# of datsets.
opt.getInst <- function(inst){

	# Split the passed instrument into a list by subject ID
	perSID <- split(x=inst, f=inst$ID)

	# Split each element of the list into a list of the important elements
	INST <- lapply(perSID,function(x){

					out <- list()

					y <- x %>%
						select(matches("[AB][0-9]$"))

					out$A <- y %>%
							select(starts_with("A")) %>%
							as.matrix()
					out$B <- y %>%
							select(starts_with("B")) %>%
							as.matrix()
					out$pA <- y %>%
							select(starts_with("pA")) %>%
							as.matrix()
					out$pB <- y %>%
							select(starts_with("pB")) %>%
							as.matrix()
					out$Min    <- as.vector(x$Min)
					out$Max    <- as.vector(x$Max)
					out$choice <- as.vector(x$choice)
					out$SID     <- as.vector(x$ID)
					out$QID    <- as.vector(x$qid)

					out
	})

	return(INST)

}

opt.getDemPars <- function(dempars, pars){
		lapply(1:length(pars),function(i){
						tryCatch({dempars[[i]]},
							error=function(x){NULL})
		})
}

opt.getDemPass <- function(dempars, inst){
	# Make a list of the covariates that will be needed so we can isolate them
	demused <- dempars %>%
						 lapply(names) %>%
						 unlist %>%
						 unique

	if( ! is.null(demused)){
		covars <- inst %>%
			select(one_of(demused),ID) %>%
			tbl_df() 
	}else{
		covars <- inst %>%
			select(ID) %>%
			tbl_df() 
	}

}

opt.splitDem <- function(dempass){
	dempass <- split(x=dempass, f=factor(dempass$ID))
	dempass <- lapply(dempass,function(x){lapply(x,function(y){y})})
}

opt.getDemIndex <- function(dempars, dpass){
	dnames <- lapply(dempars, names)

	lapply(dnames,function(x,dpass){
		out <-lapply(x, match, table=names(dpass))
		do.call(c,out) - 1
	}, dpass=dpass)
}

opt.getFullPars <- function(dempars, pars){
	fullpars <- lapply(1:length(pars), function(i){
		if(i > length(dempars)){
			pars[i]
		}else{
			c(pars[i], dempars[[i]])
		}
	})
	unlist(fullpars)
}

opt.getSolvedPars <- function(dempars, pars){
	fullpars <- lapply(1:length(pars), function(i){
		if(i > length(dempars)){
			c(i)
		}else{
			c(i,i)
		}
	})
	unlist(fullpars)
}

# Parameter transformation functions
opt.exp3 <- function(x){
	exp(x)^(1/3)
}

opt.m1to1 <- function(x){
	(exp(x) / ( 1 + exp(x))) * 2 - 1
}

# Function to format results and do some tansformations
opt.transformPars <- function(fun, param){

	if( fun == 0 ){
		# No Transformation
		tfun <- function(x){x}
	}else if( fun == 1 ){
		# Cube Root Exponential Transformation
		tfun <- opt.exp3
	}else if( fun == 2 ){
		# Exponential transform for -1 to 1
		tfun <- opt.m1to1
	}

	out <- tfun(param)

	return(out)

}

opt.getResIndex <- function(dempars, pars){
	parindex <- list()
	j <- 0
	for(i in 1:length(pars)){
		if(i > length(dempars)){
			j <- j + 1
			parindex[[i]] <- j
		}else{
			k <- j + 1
			j <- k + length(dempars[[i]])
			parindex[[i]] <- k:j
		}
	}
	parindex
}

opt.getResults <- function(m, config, dempars, pars, transforms){

	hess   <- attr(m, "details")[1, "nhatend"][[1]]
	fisher <- solve(hess)

	# If the hessian can't be solved (isn't positive semi-definite) ignore these results, they're not a real optimum
	tryCatch(
	if (is.na(hess) | is.null(hess) | is.na(fisher) | is.null(fisher)){
		return(NULL)
	} , warning = function(x){})

	# get the index of the dems in the fullvars vector
	pindex <- opt.getResIndex(dempars, pars)

	# Names
	demused <- dempars %>%
						 lapply(names) 
	parused <- names(pars)

	# Get the starting values 
	start <- opt.getFullPars(dempars, pars)

	# make a vector for the names of all parameters and dems
	fullnames <- c()
	fullestimates <- c()
	fulltransformed <- c()
	fullse <- sqrt(diag(fisher))	# Standard Errors
	fullup95 <- c()
	fulllo95 <- c()

	for(i in 1:length(pindex)){

		pind  <-  pindex[[i]]
		pname <- parused[[i]]
		dname <- demused[[i]]

		# Add to names
		fullnames <- c(fullnames, pname)

		# base parameters
		base <- m[1,pind[1]]
		fullestimates <- c(fullestimates, base)

		# transformed base parameters
		tbase <- opt.transformPars(transforms[i], base)
		fulltransformed <- c(fulltransformed, tbase)

		# 95% Confidence Interval
		bconf <- 1.96 * fullse[pind[1]]
		u95 <- opt.transformPars(transforms[i], base + bconf)
		l95 <- opt.transformPars(transforms[i], base - bconf)

		u95 <- u95 - tbase
		l95 <- l95 - tbase

		fullup95 <- c(fullup95, u95)
		fulllo95 <- c(fulllo95, l95)

		if(! is.null(dname)){

			range <- 2:(length(dname)+1)
			# Prepend the main parameter name to these other parameters
			for( j in 1:length(dname)){
				dname[j] <- paste0(pname, ": ", dname[j])
			}
			fullnames <- c(fullnames, dname)

			dems <- m[1,pind[range]]
			fullestimates <- c(fullestimates, dems)

			change <-  base + dems
			tchange <- opt.transformPars(transforms[i], change)

			tdems <- tchange - tbase

			fulltransformed <- c(fulltransformed, tdems)

			# Confidence Intervals
			dconf <- 1.96*fullse[pind[range]]
			tup <- opt.transformPars(transforms[i], change + dconf)
			tlo <- opt.transformPars(transforms[i], change - dconf)

			u95 <- tup - tbase
			l95 <- tlo - tbase

			fullup95 <- c(fullup95, u95)
			fulllo95 <- c(fulllo95, l95)

		}
	
	}

	fullestimates <- unlist(fullestimates)
	fulltransformed <- unlist(fulltransformed)
	fullup95 <- unlist(fullup95)
	fulllo95 <- unlist(fulllo95)
	start <- unlist(start)

	# Create the matrix that is going to return our results.
	mm <- data.frame(init = start, est = fullestimates, par = fulltransformed,
									 se = fullse, lower = fulllo95, upper = fullup95, llike = m$value
									 )

	colnames(hess) <- fullnames

	mm <- cbind(mm,hess)

	rownames(mm) <- fullnames

	mm
	
}

# Optim Functions
try.optimx.MSL <- function(pars, inst, dempars = list(), HH = 100, model = "EUT",
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	if (model == "EUT"){
		tryCatch(
					do.optimx.MSL.EUT(par = sim, inst = D, dempars = dempars, HH = HH,
					config = config, control= control),
					error= function(x){paste0("Error with parameters ",par)}
				)
	}
}

do.optimx.MSL.EUT <- function(pars, inst, dempars = list(), HH = 100, 
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	# Split full-long dataset to list of per-subject long datsets
	INST     <- opt.getInst(inst)

	# Fill in the elements of covariates not specified with NULL
	dempars  <- opt.getDemPars(dempars, pars)

	# Make a per-subject list containing covariates for each subject
	dempass  <- opt.getDemPass(dempars, inst)

	# Get the column index of the covnames in covpass
	demindex <- opt.getDemIndex(dempars, dempass)

	# have all the initial values of the covariates go after the parameters
	fullpars <- opt.getFullPars(dempars, pars)

	# Split the covariates by ID and then turn them into a list
	dempass <- opt.splitDem(dempass)

	# Number of Subjects
	N <- length(INST)

	# Generate Halton Sequences
	HR <- matrix(hunif(HH*N ,prime = 3, burn=45 ), nrow = N, ncol = HH, byrow=F)
	HU <- matrix(hunif(HH*N ,prime = 7, burn=45 ), nrow = N, ncol = HH, byrow=F)

	# Run the optimization
	m <- optimx(par = fullpars, fn = MSL_EUT_cov, 
						 covars = dempass, covindex = demindex,
						 h1 = HR, h2 = HU, Inst = INST,
						 method = config$method, hessian = T, control = control, itnmax = config$itnmax)

	transforms <- c(0, 1, 1, 1, 2)
	results <- opt.getResults(m, config, dempars, pars, transforms)
	return(results)

}

do.MSL.EUT <- function(par, dempars, HH, inst,
					 config = list(method="Nelder-Mead"), 
					 control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)){

	# Split full-long dataset to list of per-subject long datsets
	INST     <- opt.getInst(inst)

	# Fill in the elements of covariates not specified with NULL
	dempars  <- opt.getDemPars(dempars, par)

	# Make a per-subject list containing covariates for each subject
	dempass  <- opt.getDemPass(dempars, inst)

	# Get the column index of the covnames in covpass
	demindex <- opt.getDemIndex(dempars, dempass)

	# have all the initial values of the covariates go after the parameters
	fullpars <- opt.getFullPars(dempars, par)

	# Split the covariates by ID and then turn them into a list
	dempass <- opt.splitDem(dempass)

	# Number of Subjects
	N <- length(INST)

	# Generate Halton Sequences
	HR <- matrix(hunif(HH*N ,prime = 13, burn=45 ), nrow = N, ncol = HH, byrow=F)
	HU <- matrix(hunif(HH*N ,prime = 17, burn=45 ), nrow = N, ncol = HH, byrow=F)

	# Run the optimization
	m <- optimx(par = fullpars, fn = MSL_EUT_cov, 
						 covars = dempass, covindex = demindex,
						 h1 = HR, h2 = HU, Inst = INST,
						 method = config$method, hessian = T, control = control)

	return(m)

	# Retrieve and solve the hessian matrix
	hess   <- attr(m, "details")[config$method, "nhatend"][[1]]
	fisher <- solve(hess)

	# If the hessian can't be solved (isn't positive semi-definite) ignore these results, they're not a real optimum
	tryCatch(
	if (is.na(hess) | is.null(hess) | is.na(fisher) | is.null(fisher)){
		return(NULL)
	} , warning = function(x){})

	# Sort out _cons pars from dempars



	# Split out the hessian to return it piece by piece
	hrm <- hess[,1]
	hrs <- hess[,2]
	hum <- hess[,3]
	hus <- hess[,4]
	hrh <- hess[,5]

	# Get the square root of it
	se <- sqrt(diag(fisher))

	# Retrieve the solve parameters
	spars <- do.call(c, m[1,1:length(par)])

	# Get the 95% confidence interval
	up  <- spars + 1.96*se
	low <- spars - 1.96*se

	# Get the t stat
	t <- spars / se

	# Get the p-values
	pval<-2*(1-pt(abs(t), nrow(inst)-length(par)))

	# Adjust for the logged parameters
	tt <-  spars
	tt[2:4] <- opt.exp3(tt[2:4]) 
	tt[5]   <- opt.m1to1(tt[5]) 

	ts <- se
	ts[2:4] <- opt.exp3(ts[2:4])
	ts[5]   <- opt.m1to1(ts[5]) 

	tu <- up
	tu[2:4] <- opt.exp3(tu[2:4])
	tu[5]   <- opt.m1to1(tu[5]) 

	tl <- low
	tl[2:4] <- opt.exp3(tl[2:4])
	tl[5]   <- opt.m1to1(tl[5]) 

	start <- par
	start[2:4] <- opt.exp3(par[2:4])
	start[5]   <- opt.m1to1(par[5]) 

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

	rownames(mm) <- c("rm", "rs", "um", "us", "rh")

	cat("\n")
	print(mm)
	return(mm)

}


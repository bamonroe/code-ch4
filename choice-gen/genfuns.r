CRRA <- function(x,r){
	ifelse( r == 1, log(x),	(x^(1-r)) / (1-r) )
}

context <- function(x, Max){
	# All Options need to have equal number of outcomes, and we're only handling binary lottery pairs here
	# So the first half of the x vector will be probabilities and the second half outcomes.
	onum <- length(x) / 2
	base <- ifelse(Max, -9999999999, 999999999)

	y <- ifelse(x[1:onum]>0, x[(onum+1):length(x)] , base)

	ifelse(Max, max(y), min(y))
}

# Supply the standard deviations and the correlation coefficients and get back
# the covariance matrix
mkcov <- function(sd,rho){

	z <- .5 * ((8*length(rho) + 1)^.5 +1)
	RHO <- diag(x=1, nrow=z, ncol=z)
	count <- 0
	for(row in 1:(z-1)){
		for(col in (row+1):z){
			count <- count + 1
			RHO[row,col] <- rho[count]
		}
	}

	RHO[lower.tri(RHO)] <- RHO[upper.tri(RHO)]

	sigma <- diag(x=1, nrow=z, ncol=z)
	# Fill out the covariance matrix
	for(row in 1:z){
		for(col in 1:z){
			sigma[row,col] = sd[row] * sd[col] * RHO[row,col];
		}
	}

	sigma

}

genEUT <- function(par, N, A, B, pA, pB, Min, Max){

	rm <- par[1] # CRRA Mean
	rs <- par[2] # CRRA Standard Deviation
	um <- par[3] # Fechner Mean
	us <- par[4] # Fechner Standard Deviation
	rh <- par[5] # Rho Mean

	# Fechner will use a gamma distribution, so need to back out shape and
	# scale parameters

	means <- c(rm,um)
	sds   <- c(rs,us)

	sigma <- mkcov(sds,rh)

	dists <- MASS::mvrnorm(n=N, mu=means, Sigma=sigma)

	p1 <- pnorm(dists[,1], mean=means[1], sd=sds[1])
	p2 <- pnorm(dists[,2], mean=means[2], sd=sds[2])

	pvals <- cbind(p1,p2)

	k <- (um^2)/(us^2)
	t <- (us^2)/um

	r  <- qnorm(pvals[,1], mean=rm, sd=rs)
	mu <- qgamma(pvals[,2], shape=k, scale=t)

	d <- data.frame(c=rep(0, nrow(A)))

	for(i in 0:(ncol(A)-1)){
		d[[paste0("A",i)]]  <- A[,i+1]
	}
	for(i in 0:(ncol(A)-1)){
		d[[paste0("pA",i)]] <- pA[,i+1]
	}
	for(i in 0:(ncol(B)-1)){
		d[[paste0("B",i)]]  <- B[,i+1]
	}
	for(i in 0:(ncol(B)-1)){
		d[[paste0("pB",i)]] <- pB[,i+1]
	}

	d$Min <- Min
	d$Max <- Max

	e <- d
	e$ID <- 1

	rval <- r[1]
	uval <- mu[1] 

	while( is.na(rval) | is.infinite(rval)){
		rval <- rnorm(1,mean = rm, sd = rs) 
	}
	while( is.na(uval) | is.infinite(uval)){
		uval <- rgamma(1,shape = k, scale = t) 
	}

	e$r  <- rval
	e$mu <- uval

	D <- e

	for( i in 2:N){
		e <- d
		e$ID <- i
		# grab one value from the distribution

		rval <- r[i]
		uval <- mu[i] 

		while( is.na(rval) | is.infinite(rval)){
			rval <- rnorm(1,mean = rm, sd = rs) 
		}
		while( is.na(uval) | is.infinite(uval)){
			uval <- rgamma(1,shape = k, scale = t) 
		}

		e$r  <- rval
		e$mu <- uval
	
		D <- rbind(D,e)
	}

	ctx <- CRRA(D$Max,D$r) - CRRA(D$Min,D$r)

	UA <- 0
	UB <- 0

	for(i in 1:ncol(A)){
		UA <- UA + (pA[,i] * CRRA(A[,i], D$r))
		UB <- UB + (pB[,i] * CRRA(B[,i], D$r))
	}

	UB.1 <- (UB/ctx/D$mu) - (UA/ctx/D$mu)
	UA.1 <- 0

	# Have things gone haywire because of the computer's inability to handle
	# numbers bigger than ~3e310 ?
	c.N <- is.nan(UB.1) | is.infinite(UB.1)

	Aprob <- ifelse( c.N ,		# Are we dealing with an insane number?
		# yes:
		ifelse( UB > UA , 0 , 1 ) ,
		# no, but are we making an insane number via exp?
		ifelse( UB.1 > 709 , 0 , 
			ifelse( UB.1 < -709, 1 , exp(UA.1) / (exp(UA.1) + exp(UB.1)) )
		)
	)

	Bprob <- 1 - Aprob

	# Random uniform number
	rand <- runif(nrow(D))

	# This is a great R function, ifelse collapses would would otherwise
	# potentially be several lines of code into a very readable one line
	# statement.
	D$choice <- ifelse(Aprob > rand, 0, 1)

	return(D)

}

genEUT <- compiler::cmpfun(genEUT)


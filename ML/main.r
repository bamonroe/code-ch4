library(ctools)
c.library("dplyr","optimx","halton")

# Clean the Raw data from Glenn into my format
source("../HNG/clean.r")

c.source("optim-ML.r","../MSL/optim.r")

c.sourceCpp(c("../Rcpp/ML.cpp","../Rcpp/MSL.cpp"), on.main=F)
#c.sourceCpp("../Rcpp/ML.cpp")

getInst <- function(x){

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
					
					}

getDist <- function(subject){

	if(!is.data.frame(subject)) return(c(NA,NA))

	r <-  subject$est[1]
	mu <- subject$est[2]
	mu <- exp(mu)^(1/3)

	if(r > 5) return(c(NA,NA))
	if(r < -10) return(c(NA,NA))

	cbind(r,mu)

}

# Split each element of the list into a list of the important elements

# Whole Dataset
INST <- getInst(D)
# Per subject
perID <- lapply(split(x=D, f=D$ID), getInst)

doEUT      <- T
doRDU.pow  <- F
doRDU.prel <- F

# Parameter Set for EUT
if( doEUT ){
	par <- c(r = 0.5, mu = 0.15)
	par[2] <- log(par[2]^3)
	# Just run the function, see if we get a number
	#print(	ML_EUT(par, Inst=INST, pweight = 0) )
	print(	ML_EUT(par, Inst=perID[[1]], pweight = 0) )
	print(	ML_EUT(par, Inst=perID[[2]], pweight = 0) )

	config  <- list(method="BFGS", is.SIM=F, model="EUT", weight=1)
	config  <- list(method="Nelder-Mead", is.SIM=F, model="EUT", weight=1)
	control <- list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)
	print(	try.optimx.ML(par, inst=perID[[1]], config=config, control=control) )
	print(	try.optimx.ML(par, inst=perID[[2]], config=config, control=control) )

	stop()


	# Do the standard "pooled" data estimation for EUT
	control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)
	pooled <- do.optimx.ML(par, INST, config = config, control = control)

	per.EUT <- c.lapply(perID, try.optimx.ML, par=par, config=config)

	dist <- lapply(per.EUT,getDist)
	dist <- do.call(rbind,dist)
	dist <- dist[!is.na(dist[,1]),]
	distEUT <- dist

	#The number of subjects able to be estimated, with estimates in reasonable range
	print(nrow(dist))
	plot(density(dist[,1]))
}

#Now RDU
if( doRDU.pow ){
	# Use Power
	# Parameter Set for RDU
	par <- c(r = 0.8, mu = 0.15, alpha = 1.3)
	par[2:3] <- log(par[2:3]^3)
	# Just run the function, see if we get a number
	ML_RDU(par, Inst=INST, pweight = 0)

	# Do the standard "pooled" data estimation for RDU
	config  <- list(method="Nelder-Mead", is.SIM=F, model="RDU", weight=0)
	pooled <- do.optimx.ML(par, INST, config)

	per.Power <- c.lapply(perID, try.optimx.ML, par=par, config=config)

	dist <- lapply(per.Power,getDist)
	dist <- do.call(rbind,dist)
	dist <- dist[!is.na(dist[,1]),]
	distRDU <- dist

	#The number of subjects able to be estimated, with estimates in reasonable range
	print(nrow(dist))
	plot(density(dist[,1]))
}

# Use Prelec
if ( doRDU.prel ){
	# Parameter Set for RDU
	par <- c(r = 0.8, mu = 0.15, alpha = 1.3, beta = 1.4)
	par[2:4] <- log(par[2:4]^3)
	# Just run the function, see if we get a number
	print(ML_RDU(par, Inst=INST, pweight = 1))

	# Do the standard "pooled" data estimation for RDU
	config  <- list(method="Nelder-Mead", is.SIM=F, model="RDU", weight=1 )
	pooled <- do.optimx.ML(par, INST, config)

	per.Prelec <- c.lapply(perID, try.optimx.ML, par=par, config=config)

	dist <- lapply(per.Prelec,getDist)
	dist <- do.call(rbind,dist)
	dist <- dist[!is.na(dist[,1]),]
	distRDU2 <- dist

	#The number of subjects able to be estimated, with estimates in reasonable range
	print(nrow(dist))
	plot(density(dist[,1]))

}


 Gather the heighest likelihood results for each subject
all.per <- list()

for ( i in 1:length(per.EUT)){
	all.per[[i]] <- list( EUT=per.EUT[[i]], POWER=per.Power[[i]], PRELEC=per.Prelec[[i]])
}

all.heighest <- lapply(all.per,function(x){

		ll <- lapply(x,function(x){

			if(is.list(x)){
				x[["llike"]][1]
			}else{
			 -99999999999999999999999999
			}

		})

		ll <- do.call(c,ll)

		for( i in 1:3){
			if (ll[i] == max(ll)){
				return(x[[i]])
			}
		}
		print(ll)

})

dist <- lapply(all.heighest,getDist)
dist <- do.call(rbind,dist)
dist <- dist[!is.na(dist[,1]),]
plot(density(dist[,1]))

print(distEUT)



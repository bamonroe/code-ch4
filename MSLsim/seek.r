#Get rid of the bits
rm(list=ls())
gc()

#Libraries
library(ctools)
c.library("halton","dplyr","Rcpp","optimx")

# Compile the functions that are passed through to optim
c.sourceCpp("../Rcpp/MSL.cpp", on.main=F)

# Source 'do.optim', the function which calls optim and parses the output
# and load in/generate the dataset we want to estimate over
# genHL function takes pars and subject number as arguments
# Always better to use a single c.source function call so that the workers in the cluster
# are only called up once.
sourcefiles <- c("../MSL/optim.r", "../choice-gen/geninst.r")
c.source(sourcefiles, on.main=F)

mapper <- function(genpars, N = 100, HH = 50, itype = "HL"){

	simnum <- genpars[6]
	genpars <- genpars[1:5]

	if( itype == "HL"){
		D <- tbl_df(genHL(genpars, N))
	}else if( itype == "HNG"){
		D <- tbl_df(genHNG(genpars, N))
	}

	subjects <- max(D$ID)

	sampars <- c(mean(D$r),sd(D$r),mean(D$mu),sd(D$mu), cor(D$r,D$mu))

	# How many random places to initialize the optimization
	init.num <- 1

	rmr <- hunif(init.num, min = -.5, max = 1, prime = 3)
	rsr <- hunif(init.num, min = 0.1, max = .5, prime = 11)
	umr <- hunif(init.num, min = 0.1, max = .5, prime = 17)
	usr <- hunif(init.num, min = 0.1, max = .5, prime = 7)
	rhr <- hunif(init.num, min = -.5, max = .5, prime = 13)

	# The grid of parameters for our initial values
	rm <- c(rmr, sampars[1])
	rs <- c(rsr, sampars[2])
	um <- c(umr, sampars[3])
	us <- c(usr, sampars[4])
	rh <- c(rhr, sampars[5])

	rm <- c(rmr)
	rs <- c(rsr)
	um <- c(umr)
	us <- c(usr)
	rh <- c(rhr)

	# For Testing
	#rm <- c(genpars[1], sampars[1])
	#rs <- c(genpars[2], sampars[2])
	#um <- c(genpars[3], sampars[3])
	#us <- c(genpars[4], sampars[4])
	#rh <- c(genpars[5], sampars[5])

	sim <- data.frame(matrix(c(rm,rs,um,us,rh), nrow = 5, byrow = T))

	# We do exp(x)^(1/3) to bound the parameter to > 0, so let's transform these
	# intitial  parameters to this bounding
	sim[2:4,] <- log(sim[2:4,]^3)	
	sim[5,] <- log((-sim[5,] -1) / (sim[5,] -1))

	# Set the real values in a matrix
	reals <- matrix(c(genpars,sampars), ncol=2, byrow=F )
	colnames(reals) <- c("poppars","sampars")

	config  <- list(method = "BFGS", itnmax = 1000)
	config  <- list(method = "Nelder-Mead", itnmax = 1000)
	control <- list(trace = 0, REPORT = 2, kkt = T, usenumDeriv = T, dowarn = F)

	# Pass the actual Instrument (D here) to do.optimx.MSL
	res <- lapply(sim, do.optimx.MSL.EUT, inst = D, dempars = list(), HH = HH, 
				config = config, control= control)

	res <- lapply(res, function(x, real){cbind(real,x)}, real = reals)

	#res <- do.optimx.MSL.EUT(pars = sim[,1], inst = D, dempars = list(), HH = HH, 
	#				config = config, control= control)

	#res <- lapply(sim, try.optimx.MSL, inst = D, dempars = dempars, HH = HH, model = "EUT",
	#				config = config, control= control)

	return(res)

}

# How fine do we want the generation grid to be?
S <- 3

RM <- hunif(S, min = -.5,  max = 1,    prime = 3)     # Mean of the CRRA distribution
RS <- hunif(S, min = 0.10, max = 0.70, prime = 13)    # Standard Deviation of the CRRA distribution
UM <- hunif(S, min = 0.05, max = 0.20, prime = 7)     # Mean of the mu distribution
US <- hunif(S, min = 0.10, max = 0.60, prime = 11)    # Standard Deviation of the mu distribution
RH <- hunif(S, min = -.65, max = 0.65, prime = 19)    # Correlation coefficient between CRRA and mu distrubutions

SIM <- data.frame(matrix(c(RM, RS, UM, US, RH, 1:S), nrow = 6, byrow = T))
rownames(SIM) <- c("rm", "rs", "um", "us", "rh", "snum")

NN <- c( 100, 200, 300, 100, 200, 300)    # Sample size of generated dataset
HN <- c( 150, 150, 150, 500, 500, 500)    # Number of H draws to be used in estimation

NN <- c( 100 )    # Sample size of generated dataset
HN <- c( 200 )    # Number of H draws to be used in estimation

grid <- rbind(NN,HN)

# How many times to resample the SIM grid. SIM doesn't change, because of hunif, but the generation
# of choice data draws a new sample using the same population parameters.
sample.start <- 1
sample.end <- 1

instruments <- c("HNG")
instruments <- c("HL")
instruments <- c("HL", "HNG")

for(s in sample.start:sample.end){
	for( g in 1:ncol(grid)){
		for(i in instruments){

			n <- grid[1,g]
			hh <- grid[2,g]

			datdir <- "../data/MSL-Sim/"

			RES <- c.lapplyLB(SIM, mapper, N = n, HH = hh, itype = i)
			#RES <- lapply(SIM, mapper, N = n, HH = hh, itype = i)
#			save(RES, file = paste0(datdir,"Inst-",i,"-N",n,"-H",hh,"-Sam",s,".Rda"))
			print(i)
			print(RES)

		}
	}
}


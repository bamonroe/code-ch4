#Get rid of the bits
rm(list=ls())
gc()

#Libraries
library(ctools)
c.config(2)
c.library("halton","dplyr","Rcpp","optimx")


dcount <<- 0
dbug <- function(){
	dcount <<- dcount + 1
	message(paste("here",dcount))
}
c.export("dcount","dbug",push=F)

# Compile the functions that are passed through to optim
c.sourceCpp("../Rcpp/sim.cpp")

# Source 'do.optim', the function which calls optim and parses the output
# and load in/generate the dataset we want to estimate over
# genHL function takes pars and subject number as arguments
# Always better to use a single c.source function call so that the workers in the cluster
# are only called up once.
sourcefiles <- c("optim.r", 
				 "../choice-gen/geninst.r")
c.source(sourcefiles)

UH <- function(HH, N, TT, type = "per.subject"){

	if (type == "per.subject"){
	
		HR <- matrix(hunif(HH*N , prime = 3 ), nrow = N, ncol = HH, byrow=F)
		HU <- matrix(hunif(HH*N , prime = 7 ), nrow = N, ncol = HH, byrow=F)

		#HR <- matrix(runif(HH*N ), nrow = N, ncol = HH, byrow=F)
		#HU <- matrix(runif(HH*N ), nrow = N, ncol = HH, byrow=F)

		return(list(HR=HR,HU=HU))

		HR <- cbind(1:nrow(HR), HR)
		HU <- cbind(1:nrow(HU), HU)

		HR <- do.call(rbind,replicate(TT, HR, simplify=F))
		HU <- do.call(rbind,replicate(TT, HU, simplify=F))

		HR <- HR[order(HR[,1]),2:ncol(HR)]
		HU <- HU[order(HU[,1]),2:ncol(HU)]

	}else if(type == "per.H"){
	
		HR <- matrix(halton(HH , prime = 3), nrow = N*TT, ncol = HH, byrow = T)
		HU <- matrix(halton(HH , prime = 7), nrow = N*TT, ncol = HH, byrow = T)
	
	}else if(type == "per.obs"){
	
		HR <- matrix(halton(HH*N*TT , prime = 3), nrow = N*TT, ncol = HH, byrow = F)
		HU <- matrix(halton(HH*N*TT , prime = 7), nrow = N*TT, ncol = HH, byrow = F)
	
	}

	return(list(HR=HR,HU=HU))

}

# Will need the UH function inside the mapper function
c.export("UH")

mapper <- function(genpars, N = 100, HH = 50, itype = "HL"){

	if( itype == "HL"){
		D <- tbl_df(genHL(genpars, N))
	}else if( itype == "HNG"){
		D <- tbl_df(genHNG(genpars, N))
	}

	A <- D %>%
			select(starts_with("A")) %>%
			as.matrix()
			
	B <- D %>%
			select(starts_with("B")) %>%
			as.matrix()

	pA <- D %>%
			select(starts_with("pA")) %>%
			as.matrix()
			
	pB <- D %>%
			select(starts_with("pB")) %>%
			as.matrix()

	Min <- as.vector(D$Min)
	Max <- as.vector(D$Max)
	choice <- as.vector(D$c)

	SID <- as.vector(D$ID)

	sampars <- c(mean(D$r),sd(D$r),mean(D$mu),sd(D$mu), cor(D$r,D$mu))

	rmr <- hunif(5, min = -.5, max = 1, prime = 3)
	rsr <- hunif(5, min = 0.1, max = .5, prime = 11)
	umr <- hunif(5, min = 0.1, max = .5, prime = 17)
	usr <- hunif(5, min = 0.1, max = .5, prime = 7)
	rhr <- hunif(5, min = -.05, max = .05, prime = 7)

	# The grid of parameters for our initial values
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

	htype <- "per.obs"		# Different value for each observation
	htype <- "per.H"		# Different value for each H draw
	htype <- "per.subject"  # Different value for each subject
	subjects <- max(D$ID)

	Hseq <- UH(HH, subjects, nrow(D)/subjects, type = htype)

	HR <- Hseq$HR
	HU <- Hseq$HU

	config  <- list(method="BFGS", HH = HH, poppars=genpars, sampars=sampars)
	config  <- list(method="Nelder-Mead", HH = HH, poppars=genpars, sampars=sampars)

	control <- list(trace=0, REPORT = 1, kkt = T, usenumDeriv = T)

	inst <- data.frame(cbind(A,pA,B,pB,Max,Min,choice,SID))

	res <- lapply(sim, do.optimx, HR=HR, HU=HU, inst=inst, config=config, control=control)

	return(res)

}

S <- 500

RM <- hunif(S, min = -.5, max = 1, prime = 3)
RS <- hunif(S, min = 0.10, max = 0.70, prime = 13)
UM <- hunif(S, min = 0.10, max = 0.60, prime = 7)
US <- hunif(S, min = 0.10, max = 0.60, prime = 11)
RH <- rep(0,S)

SIM <- data.frame(matrix(c(RM,RS,UM,US,RH), nrow = 5, byrow = T))
rownames(SIM) <- c("rm", "rs", "um", "us", "rh")

NN <- c( 300, 400, 400 )
HN <- c( 350, 200, 350 )

NN <- c( 100, 200, 300)
HN <- c( 150, 150, 150)

grid <- rbind(NN,HN)

# How many times to resample the SIM grid. SIM doesn't change, because of hunif, but the generation
# of choice data draws a new sample using the same population parameters.
sample.start <- 2
sample.end <- 2

for(s in sample.start:sample.end){
	for(i in c("HL","HNG")){
		for( g in 1:ncol(grid)){

			n <- grid[1,g]
			hh <- grid[2,g]

			datdir <- "../data/MSL-Sim/"

			RES <- c.lapplyLB(SIM, mapper, N = n, HH = hh, itype = i)
			#RES <- lapply(SIM, mapper, N = n, HH = hh)
			save(RES, file = paste0(datdir,"Inst-",i,"-N",n,"-H",hh,"-Sam",s,".Rda"))
		}
	}
}

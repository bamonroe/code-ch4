#Get rid of the bits
rm(list=ls())
gc()

#Libraries
library(ctools)
c.library("halton","dplyr","Rcpp","optimx")

# Compile the functions that are passed through to optim
c.call(Rcpp::sourceCpp,"../Rcpp/sim.cpp")

# Source 'do.optim', the function which calls optim and parses the output
source("optim.r")

# Load in/generate the dataset we want to estimate over
source("../choice-gen/geninst.r")		# genHL function takes pars and subject number as arguments

UH <- function(HH, N, TT, type = "per.subject"){

	if (type == "per.subject"){
	
		HR <- matrix(halton(HH*N , prime = 3 ), nrow = N, ncol = HH, byrow=F)
		HU <- matrix(halton(HH*N , prime = 7 ), nrow = N, ncol = HH, byrow=F)

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

c.export("UH","genHL","genEUT","CRRA","do.optim","do.optimx")

mapper <- function(genpars, N = 100, HH = 50){

	#D <- tbl_df(genHO(genpars, N))
	D <- tbl_df(genHL(genpars, N))

	A <- D %>%
			select(starts_with("A"))
	A <- as.matrix(A)
			
	B <- D %>%
			select(starts_with("B"))
	B <- as.matrix(B)

	pA <- D %>%
			select(starts_with("pA"))
	pA <- as.matrix(pA)
			
	pB <- D %>%
			select(starts_with("pB"))
	pB <- as.matrix(pB)

	Min <- as.vector(D$Min)
	Max <- as.vector(D$Max)
	choice <- as.vector(D$c)

	SID <- as.vector(D$ID)

	sampars <- c(mean(D$r),sd(D$r),mean(D$mu),sd(D$mu))

	rmr <- hunif(5, min = -1.5, max = 1, prime = 3)
	rsr <- hunif(5, min = 0.1, max = .7, prime = 11)
	umr <- hunif(5, min = 0.1, max = .7, prime = 17)
	usr <- hunif(5, min = 0.1, max = .7, prime = 7)

	# The grid of parameters for our initial values
	rm <- c(rmr)
	rs <- c(rsr)
	um <- c(umr)
	us <- c(usr)

	# For Testing
	#rm <- c(genpars[1], sampars[1])
	#rs <- c(genpars[2], sampars[2])
	#um <- c(genpars[3], sampars[3])
	#us <- c(genpars[4], sampars[4])

	sim <- data.frame(matrix(c(rm,rs,um,us), nrow = 4, byrow = T))

	# We do exp(x)^(1/3) to bound the parameter to > 0, so let's transform these
	# intitial  parameters to this bounding
	sim[2:4,] <- log(sim[2:4,]^3)	

	htype <- "per.subject"  # Different value for each subject
	htype <- "per.obs"		# Different value for each observation
	htype <- "per.H"		# Different value for each H draw
	subjects <- max(D$ID)

	Hseq <- UH(HH, subjects, nrow(D)/subjects, type = htype)

	HR <- Hseq$HR
	HU <- Hseq$HU

	config  <- list(method="BFGS", HH = HH, poppars=genpars, sampars=sampars, N = subjects, Obs = nrow(D))
	config  <- list(method="Nelder-Mead", HH = HH, poppars=genpars, sampars=sampars, N = subjects, Obs = nrow(D))

	control <- list(trace=0, REPORT = 10, kkt = T, usenumDeriv = T)
	#res <- lapply(sim, do.optimx, HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice, config=config, control=control)


	inst <- data.frame(cbind(A,pA,B,pB,Max,Min,choice,SID))

	res <- lapply(sim, do.optimx2, HR=HR, HU=HU, inst=inst, config=config, control=control)

	#control <- list(trace=1, REPORT = 10)
	#res <- lapply(sim, do.optim, HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice, config=config, control=control)

	print(res)

	return(res)
}

S <- 400

RM <- hunif(S, min = -1.9, max = 1.55, prime = 3)
RS <- hunif(S, min = 0.10, max = 0.70, prime = 13)
UM <- hunif(S, min = 0.10, max = 0.70, prime = 7)
US <- hunif(S, min = 0.10, max = 0.70, prime = 11)

#RM <- hunif(S, min = -.1 , max = 0.55, prime = 3)
#RS <- hunif(S, min = 0.10, max = 0.40, prime = 13)
#UM <- hunif(S, min = 0.10, max = 0.40, prime = 7)
#US <- hunif(S, min = 0.10, max = 0.40, prime = 11)

SIM <- data.frame(matrix(c(RM,RS,UM,US), nrow = 4, byrow = T))
rownames(SIM) <- c("rm", "rs", "um", "us")

NN <- c( 300, 400, 400 )
HN <- c( 350, 200, 350 )

#NN <- c( 300)
#HN <- c( 150)

grid <- rbind(NN,HN)

# How many times to resample the SIM grid. SIM doesn't change, because of hunif, but the generation
# of choice data draws a new sample using the same population parameters.
sample.start <- 2
sample.end <- 10

for(s in sample.start:sample.end){
	for( i in 1:ncol(grid)){

		n <- grid[1,i]
		hh <- grid[2,i]

		RES <- c.lapplyLB(SIM, mapper, N = n, HH = hh)
		#RES <- lapply(SIM, mapper, N = n, HH = hh)
		save(RES, file = paste0("Grid-N",n,"-H",hh,"-Sam",s,".Rda"))
	}

}

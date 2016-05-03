#Get rid of the bits
rm(list=ls())
gc()

#Libraries
library(ctools)
c.library("halton","dplyr","Rcpp")

# Compile the functions that are passed through to optim
c.call(Rcpp::sourceCpp,"../Rcpp/sim.cpp")

# Source 'do.optim', the function which calls optim and parses the output
source("optim.r")

# Load in/generate the dataset we want to estimate over
source("../choice-gen/geninst.r")		# genHL function takes pars and subject number as arguments

genpars <- c(rm = 0.65, rs = 0.3, um = 0.35, us = 0.3)

N <- 200

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
choice   <- as.vector(D$c)

# The parameter space we will loop over 
S <- c.cores() - 1

rm <- hunif(S, min = -.2, max = .7, prime = 3)
rs <- hunif(S, min = 0.1, max = .5, prime = 13)
um <- hunif(S, min = 0.1, max = .5, prime = 7)
us <- hunif(S, min = 0.1, max = .5, prime = 11)

SIM <- data.frame(matrix(c(rm,rs,um,us), nrow = 4, byrow = T))
SIM <- cbind(SIM,genpars)
#SIM[2:4,] <- log(SIM[2:4,])
#SIM[2:4,] <- log(SIM[2:4,]^3)
rownames(SIM) <- c("rm", "rs", "um", "us")

htype <- "per.H"		# Different value for each H draw
htype <- "per.subject"  # Different value for each subject
htype <- "per.obs"		# Different value for each observation
subjects <- max(D$ID)

HH <- 200
HH <- max(ceiling(sqrt(nrow(D))), HH)	# The rate at which the MSL estimator approaches unbiasedness, I think, check out Train (2002)

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

Hseq <- UH(HH, subjects, nrow(D)/subjects, type = htype)

HR <- Hseq$HR
HU <- Hseq$HU

config <- list(method="BFGS", HH = HH, real=genpars )
config <- list(method="Nelder-Mead", HH = HH, real=genpars )

res <- c.lapply(SIM,do.optim , HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice, config=config)
#do.optim(SIM[,1], HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice, config=config)
#res <- lapply(SIM,do.optim , HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice, config=config)

print(res)

save(res, file = "OptimRes.Rda")


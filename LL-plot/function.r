#Get rid of the bits
rm(list=ls())
gc()

#Libraries
library(ctools)
c.library("halton","dplyr","Rcpp","ggplot2")

#c.config(2)

# Compile the functions that are passed through to optim
c.call(Rcpp::sourceCpp,"../Rcpp/sim.cpp")

# Load in/generate the dataset we want to estimate over
source("../choice-gen/geninst.r")		# genHL function takes pars and subject number as arguments

genpars <- c(rm = 0.28, rs = 0.4, um = 0.35, us = 0.15)

N <- 200

#D <- tbl_df(genHO(genpars, N))
#D <- tbl_df(genHL(genpars, N))
D <- tbl_df(genHNG(genpars, N))

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

sampars <- c(rm = mean(D$rm), rs = mean(D$rs), um = mean(D$um), us = mean(D$us))

# The parameter space we will loop over 
S <- 100

rm <- hunif(S, min = -0.3, max = .8, prime = 3)
rs <- hunif(S, min = 0.1, max = .5, prime = 13)
um <- hunif(S, min = 0.1, max = .5, prime = 7)
us <- hunif(S, min = 0.1, max = .5, prime = 11)

SIM <- data.frame(matrix(c(rm,rs,um,us), nrow = 4, byrow = T))
SIM <- cbind(SIM,genpars)
#SIM[2:4,] <- log(SIM[2:4,])
SIM[2:4,] <- log(SIM[2:4,]^3)
rownames(SIM) <- c("rm", "rs", "um", "us")

htype <- "per.subject"  # Different value for each subject
htype <- "per.obs"		# Different value for each observation
htype <- "per.H"		# Different value for each H draw
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
	
	}else if(type == "unif.seq"){

		unif <- seq(from=0, to=1, length.out=(HH+2))
		unif <- unif[2:(length(unif)-1)]
		HR <- matrix(unif, nrow = N*TT, ncol = HH, byrow = F)
		HU <- matrix(unif, nrow = N*TT, ncol = HH, byrow = F)
	
	}

	return(list(HR=HR,HU=HU))

}

	Hseq <- UH(HH, subjects, nrow(D)/subjects, type = htype)

	HR <- Hseq$HR
	HU <- Hseq$HU

	config  <- list(method="Nelder-Mead", HH = HH, poppars=genpars, sampars=sampars)
	config  <- list(method="BFGS", HH = HH, poppars=genpars, sampars=sampars)

	control <- list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T)

	inst <- data.frame(cbind(A,pA,B,pB,Max,Min,choice,SID))

	res <- do.optimx2(sim[[1]], HR=HR, HU=HU, inst=inst, config=config, control=control)

	print(res)

	return(res)

	perSID <- split(x=inst, f=inst$SID)

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

do.MSL <- function(par , HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice){
	MSL_EUT(par, h1=HR, h2=HU, Inst=INST )
}

#res <- c.lapply(SIM, MSL_EUT, HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice)
res <- c.lapply(SIM, do.MSL, HR=HR, HU=HU, A=A, B=B, pA=pA, pB=pB, Max=Max, Min=Min, choice=choice)

print(res)

allres <- do.call(rbind,res)
SIM[2:4,] <- exp(SIM[2:4,])^(1/3)
allsim <- do.call(rbind,SIM)

LL <- data.frame(cbind(allres, allsim))
LL <- tbl_df(LL)
colnames(LL) <- c("LL","rm","rs","um","us")
LL$LL <- -(LL$LL)

save(LL,genpars,sampars, file="LL.Rda")
#save(res, file = "OptimRes.Rda")


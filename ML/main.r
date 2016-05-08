library(ctools)
c.library("dplyr","optimx","halton")

load("../data/HNG/choices_all.Rda")

# Clean the Raw data from Glenn into my format
source("clean.r")

c.source("optim-ML.r","../MSL/optim.r")

c.sourceCpp(c("../Rcpp/ML.cpp","../Rcpp/MSL.cpp"), on.main=F)
#c.sourceCpp("../Rcpp/ML.cpp")

getInst <- function(x){
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
						ID    <- as.vector(x$ID)

						list(A=A, B=B, pA=pA, pB=pB, Min=Min, Max=Max, choice=choice, ID=ID)
					
					}

# Split each element of the list into a list of the important elements

# Whole Dataset
INST <- getInst(D)
# Per subject
perID <- lapply(split(x=D, f=D$ID), getInst)


par <- c(r = 0.5, mu = 0.15)
par[2] <- log(par[2]^3)

pooled <- do.optimx.ML(par, INST)

per <- c.lapply(perID, try.optimx.ML, par=par)

getDist <- function(subject){

	if(!is.data.frame(subject)) return(c(NA,NA))

	r <-  subject$est[1]
	mu <- subject$est[2]
	mu <- exp(mu)^(1/3)

	if(r > 5) return(c(NA,NA))
	if(r < -10) return(c(NA,NA))

	cbind(r,mu)

}

dist <- lapply(per,getDist)
dist <- do.call(rbind,dist)
dist <- dist[!is.na(dist[,1]),]

#The number of subjects able to be estimated, with estimates in reasonable range
print(nrow(dist))

plot(density(dist[,1]))

rm <- mean(dist[,1])
rs <-   sd(dist[,1])
um <- mean(dist[,2])
us <-   sd(dist[,2])
rh <- cor(dist[,1],dist[,2])

sim <- c(rm=rm, rs=rs, um=um, us=us, rh=rh)
sim[2:4] <- log(sim[2:4]^3)	
sim[5] <- log((-sim[5] -1) / (sim[5] -1))

config  <- list(method="Nelder-Mead", is.SIM=F)
config  <- list(method="BFGS", is.SIM=F)

control <- list(trace=1, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)

sim <- data.frame(cbind(sim,sim,sim,sim,sim,sim,sim,sim))
jitter <- hunif(5*8,min=-1,max=1)
sim[,2:8] <- sim[,2:8] + jitter

res <- c.lapply(sim, try.optimx.MSL, HH=1500, inst=D, config=config, control=control)

print(sim)
print(res)

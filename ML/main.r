library(ctools)
c.library("dplyr","optimx")

load("../data/HNG/choices_all.Rda")
# Clean the Raw data from Glenn into my format
source("clean.r")

c.source("optim-ML.r")
c.sourceCpp("../Rcpp/ML.cpp", on.main=F)
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

	r  <- rnorm(1000, subject$est[1], subject$se[1])
	mu <- rnorm(1000, subject$est[2], subject$se[2])
	mu <- exp(mu)^(1/3)

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

plot(density(dist[,1]))

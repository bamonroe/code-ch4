library(ctools)

c.library("dplyr","optimx","halton")

c.source("../ML/optim.r", "../choice-gen/geninst.r", "ml.r", clear=T)

c.sourceCpp(c("../Rcpp/ML.cpp","../Rcpp/MSL.cpp"), on.main=F, clear=T)

genpars <- c(rm = 0.67, rs = .4, um = .25, us = .06, rh = .3 )

N <- 111

D <- genHNG(genpars, N)

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
						out$choice <- as.vector(x$c)
						out$ID     <- as.vector(x$ID)
						out$QID    <- as.vector(x$qid)
						out$r      <- as.vector(x$r)
						out$mu     <- as.vector(x$mu)

						out
					
					}

INST <- getInst(D)
perID <- lapply(split(x=D, f=D$ID), getInst)

par <- c(r = genpars[1], mu = genpars[3])
par[2] <- log(par[2]^3)

config  <- list(method="BFGS")
config  <- list(method="Nelder-Mead")
control <- list(trace=2, REPORT = 1, kkt = T, usenumDeriv = TRUE, dowarn = F)

# Get estimates on the pooled Data
agg.1 <- try.optimx.MLsim(par, inst=INST, model = "EUT", config=config, control=control) 
agg.2 <- try.optimx.MLsim(par, inst=INST, model = "rEUT", config=config, control=control) 

agg.1
agg.2


# Get estimates on the individual Data

per.EUT <- c.lapply(perID, try.optimx.MLsim, par=par, model = "EUT", config=config)

# How far are the estimates from the real numbers?
getDiff <- function(result){

	if(is.list(result)){
		diff <- result$real - result$par
		return(diff)
		if( diff[1] < 2 & diff[1] > -2){
			diff
		}else{
			c(NA,NA)
		}
	}else{
		c(NA,NA)
	}

}

diff <- lapply(per.EUT, getDiff)
den <- do.call(rbind,diff)

plot(den[,1])
plot(density(den[,1],na.rm=T))

lapply(list(agg.1,agg.2), getDiff)


# How big is the welfare consequence of the estimation

welDiff <- function(sub){

	A  <- sub$A
	B  <- sub$B
	pA <- sub$pA
	pB <- sub$pB
	c  <- sub$choice
	r  <- mean(sub$r)
	result <- sub$result

	if(is.list(result)){

		e <- result$par[1]

		UA.r <- pA * A^(1-r) / (1-r)
		UB.r <- pB * B^(1-r) / (1-r)

		UA.r <- rowSums(UA.r)
		UB.r <- rowSums(UB.r)

		CEA.r <- ((1-r)*UA.r)^(1/(1-r))
		CEB.r <- ((1-r)*UB.r)^(1/(1-r))

		CEM.r <- ifelse(CEA.r > CEB.r, CEA.r, CEB.r)
		CE0.r <- ifelse(c == 0, CEA.r, CEB.r)
		CE1.r <- ifelse(c == 0, CEB.r, CEA.r)

		WS.r <- sum(CE0.r - CE1.r)
		WE.r <- sum(CE0.r) / sum(CEM.r)


		UA.e <- pA * A^(1-e) / (1-e)
		UB.e <- pB * B^(1-e) / (1-e)

		UA.e <- rowSums(UA.e)
		UB.e <- rowSums(UB.e)

		CEA.e <- ((1-e)*UA.e)^(1/(1-e))
		CEB.e <- ((1-e)*UB.e)^(1/(1-e))

		CEM.e <- ifelse(CEA.e > CEB.e, CEA.e, CEB.e)
		CE0.e <- ifelse(c == 0, CEA.e, CEB.e)
		CE1.e <- ifelse(c == 0, CEB.e, CEA.e)

		WS.e <- sum(CE0.e - CE1.e)
		WE.e <- sum(CE0.e) / sum(CEM.e)

		return(r - e)
		return(abs(WS.r-WS.e)/WS.r)
		return(abs(WE.r-WE.e)/WE.r)
		return((WS.r-WS.e)/WS.r)
		return((WE.r-WE.e)/WE.r)

	}else{
		NA
	}


}

for(i in 1:length(perID)){
	perID[[i]]$result <- per.EUT[[i]]
}

exclude <- 8

wdiff <- lapply(perID, welDiff)
wdiff <- do.call(rbind, wdiff)
wdiff <- ifelse(wdiff < exclude & wdiff > -exclude, wdiff, NA)
#wdiff <- ifelse(wdiff < 2 & wdiff > -2, wdiff, NA)

plot(density(wdiff,na.rm=T))
plot(wdiff)
mean(wdiff, na.rm=T)

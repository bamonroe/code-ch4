library(ctools)
c.library("MSL","cgen","welfare","halton")

c.source("perSim.R")

options(warn=1)

# Set runs to be a multiple of the number of cores available
NN <- 100
NN <- c.cores() 

rm <- runif(NN, min = -0.50, max = 1.50)
rs <- runif(NN, min =  0.05, max = 1.50)
am <- runif(NN, min =  0.60, max = 1.70)
as <- runif(NN, min =  0.10, max = 0.60)
bm <- runif(NN, min =  0.50, max = 1.70)
bs <- runif(NN, min =  0.10, max = 1.00)
um <- runif(NN, min =  0.08, max = 0.30)
us <- runif(NN, min =  0.05, max = 0.50)
ra <- runif(NN, min = -0.67, max = 0.67)
rb <- runif(NN, min = -0.67, max = 0.67)
ab <- runif(NN, min = -0.67, max = 0.67)
ru <- runif(NN, min = -0.67, max = 0.67)
au <- runif(NN, min = -0.67, max = 0.67)
bu <- runif(NN, min = -0.67, max = 0.67)

EUT    <- data.frame(rbind(pop = 0, rm, rs, am, as, bm, bs, um, us, ra, rb, ab, ru, au, bu))
POW    <- data.frame(rbind(pop = 1, rm, rs, am, as, bm, bs, um, us, ra, rb, ab, ru, au, bu))
INVS   <- data.frame(rbind(pop = 2, rm, rs, am, as, bm, bs, um, us, ra, rb, ab, ru, au, bu))
PRELEC <- data.frame(rbind(pop = 3, rm, rs, am, as, bm, bs, um, us, ra, rb, ab, ru, au, bu))

#runSimEUT <- function(sim.eut) {
runSimEUT <- function(i, SIM) {

	use.eut <- c(F, T, T, F, F, F, F, T, T, F, F, F, T, F, F)
	use.pow <- c(F, T, T, T, T, F, F, T, T, T, F, F, T, T, F)
	use.inv <- c(F, T, T, T, T, F, F, T, T, T, F, F, T, T, F)
	use.pre <- c(F, T, T, T, T, T, T, T, T, T, T, T, T, T, T)

	sim <- SIM[,i]

	if (sim[1] == 0 ) {
		sim <- sim[use.eut]
	  pfunc <- "EUT"
	} else if (sim[1] == 1 ) {
		sim <- sim[use.pow]
	  pfunc <- "pow"
	}	else if (sim[1] == 2 ) {
		sim <- sim[use.inv]
	  pfunc <- "invs"
	}	else if (sim[1] == 3 ) {
		sim <- sim[use.pre]
	  pfunc <- "prelec"
	}

	N <- 100
	H <- 100
	iter <- 50
	control <- list()

#	load(file=paste0("MSL",i,".Rda"))
#	sim.eut <- c(rm = mean(D$r), rs = sd(D$r), um = mean(D$mu), us = sd(D$r), ru = cor(D$r, D$mu))

	#names(sim) <- c("rm", "rs", "um", "us", "ru")

	D <- genChoice(sim, N, inst="HNG", pfunc = pfunc)
	# Clean up the D
	D <- ML.clean(D)

	optimizer <- "BHHH"

	print(sim)

	# I don't have much of an issue starting on the real values, I've don't big grids of starting values before and they all tend to converge on
	# the same result.
	K <- MSL.optim(pars = sim, inst = D, pfunc = pfunc,  HH = H, optimizer = optimizer, iterations = iter, try = F, control = control)
	#K <- tryCatch(MSL.optim(pars = sim, inst = D, pfunc = pfunc,  HH = H, optimizer = optimizer, iterations = iter, try = F, control = control),
	#			  error = function(x) return(NA))

	print(K$parsed)

	save(K, D, file=paste0("MSL",i,"-",pfunc,".Rda"))

	# If MSL failed all is lost
	if (is.na(K[1])) return(NULL)

	perSID <- split(x=D, f=D$ID)

	# Run the main estimation welfare calculation stuff
	wel_res <- lapply(perSID, runInd, K = K)

	res <- list()

	# Grab the bits of the output and aggregate it
	ind_fail <- lapply(wel_res, function(x) x[1])
	res$fails <- mean(unlist(ind_fail), na.rm = T)
	
	ind_sim <- lapply(wel_res, function(x) x[2])
	res$sim_avg <- mean(unlist(ind_sim), na.rm = T)

	eut_win <- lapply(wel_res, function(x) x[3])
	res$eut_avg <- mean(unlist(eut_win), na.rm = T)

	pow_win <- lapply(wel_res, function(x) x[4])
	res$pow_avg <- mean(unlist(pow_win), na.rm = T)

	invs_win <- lapply(wel_res, function(x) x[5])
	res$invs_avg <- mean(unlist(invs_win), na.rm = T)

	prelec_win <- lapply(wel_res, function(x) x[6])
	res$prelec_avg <- mean(unlist(prelec_win), na.rm = T)

	print(unlist(res))

	return(res)

}


#eut    <- lapply(1:NN, runSimEUT)
prelec <- lapply(1:NN, runSimEUT, SIM = PRELEC)

res


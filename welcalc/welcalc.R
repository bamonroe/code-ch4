library(ctools)
c.library("MSL","cgen","welfare","halton")

c.source("perSim.R")

# Set runs to be a multiple of the number of cores available
NN <- c.cores() - 1
#NN <- 1

#rm <- hunif(NN, min = -0.50, max = 1.50)
#rs <- hunif(NN, min =  0.05, max = 1.50)
#um <- hunif(NN, min =  0.08, max = 0.80)
#us <- hunif(NN, min =  0.05, max = 0.50)
#ru <- hunif(NN, min = -0.67, max = 0.67)
#
#SIM <- data.frame(rbind(rm, rs, um, us, ru))
#SIM
#
#
#c.lapply(1:NN, function(i){
#
#	sim.eut <- SIM[,i]
#	N <- 200
#	H <- 150
#	iter <- 50
#	control <- list()
#	D <- genChoice(sim.eut, N, inst="HNG", pfunc = "EUT")
#	K <- MSL.optim(pars = sim.eut, inst = D, pfunc = "EUT",  optimizer = "BFGS", try = F)
#	save(K, D, file=paste0("MSL",i,".Rda"))
#
#})
#
#stop()

#runSimEUT <- function(sim.eut) {
runSimEUT <- function(i) {

	load(file=paste0("MSL",i,".Rda"))
	sim.eut <- c(rm = mean(D$r), rs = sd(D$r), um = mean(D$mu), us = sd(D$r), ru = cor(D$r, D$mu))

#	N <- 200
#	H <- 150
#	iter <- 50
#	control <- list()
#
#	names(sim.eut) <- c("rm", "rs", "um", "us", "ru")
#
#	D <- genChoice(sim.eut, N, inst="HNG", pfunc = "EUT")
	# Clean up the D
	D <- ML.clean(D)
#
#	K <- tryCatch(MSL.optim(pars = sim.eut, inst = D, pfunc = "EUT",  HH = H, optimizer = "BFGS", iterations = iter, try = F, control = control),
#				  error = function(x) return(NA))



	if (is.na(K[1])) return(NULL)
	#save(D, K, file="optimres.Rda")
	#load("optimres.Rda")

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

res <- lapply(1:NN, runSimEUT)



res


library(ctools)
c.library("MSL","cgen","welfare","halton")

c.source("classify.R")

options(warn=1)

# Set runs to be a multiple of the number of cores available
NN <- round(c.cores() * 10 / 4)
NN <- round(c.cores() * 2)
NN <- round(c.cores() * 1)

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

ALL <- data.frame(rbind(rm, rs, am, as, bm, bs, um, us, ra, rb, ab, ru, au, bu))

#runSimEUT <- function(sim.eut) {
simRun <- function(i, SIM) {

	N.eut    <- 500
	N.pow    <- 50
	N.invs   <- 100
	N.prelec <- 350

	use.eut <- c(T, T, F, F, F, F, T, T, F, F, F, T, F, F)
	use.eut <- SIM[use.eut, i]
	eut     <- tryCatch(genChoice(use.eut, N.eut, inst="HNG", pfunc = "EUT"), error = function(e){NA})
	if (!is.data.frame(eut)) return(NULL)
	eut$alpha <- 1
	eut$beta <- 1
	eut$mod <- "EUT"

	use.pow <- c(T, T, T, T, F, F, T, T, T, F, F, T, T, F)
	use.pow <- SIM[use.pow, i]
	pow     <- tryCatch(genChoice(use.pow, N.pow, inst="HNG", pfunc = "pow"), error = function(e){NA})
	if (!is.data.frame(pow)) return(NULL)
	pow$beta <- 1
	pow$mod <- "pow"
	pow$ID <- pow$ID + N.eut

	use.invs <- c(T, T, T, T, F, F, T, T, T, F, F, T, T, F)
	use.invs <- SIM[use.invs, i]
	invs     <- tryCatch(genChoice(use.invs, N.invs, inst="HNG", pfunc = "invs"), error = function(e){NA})
	if (!is.data.frame(invs)) return(NULL)
	invs$beta <- 1
	invs$mod <- "invs"
	invs$ID <- invs$ID + N.eut + N.pow

	use.pre    <- c(T, T, T, T, T, T, T, T, T, T, T, T, T, T)
	use.pre    <- SIM[use.pre, i]
	prelec     <- tryCatch(genChoice(use.pre, N.prelec, inst="HNG", pfunc = "prelec"), error = function(e){NA})
	if (!is.data.frame(prelec)) return(NULL)
	prelec$mod <- "prelec"
	prelec$ID <- prelec$ID + N.eut + N.pow + N.invs

	D <- rbind(eut, pow, invs, prelec)
	# Clean up the D
	D <- ML.clean(D)

	perSID <- split(x=D, f=D$ID)

	# Run the individual level classification stuff
	wel_res <- lapply(perSID, runInd)

	res <- list()

	results <- list(failed = 0, EUT = 0, pow = 0, invs = 0, prelec = 0, correct = 0)

	# Grab the bits of the output and aggregate it
	ind_fail <- lapply(wel_res, function(x) x[1])
	res$fails <- mean(unlist(ind_fail), na.rm = T)
	
	ind_sim <- lapply(wel_res, function(x) x[2])
	res$correct <- mean(unlist(ind_sim), na.rm = T)

	eut_win <- lapply(wel_res, function(x) x[3])
	res$eut_avg <- mean(unlist(eut_win), na.rm = T)

	pow_win <- lapply(wel_res, function(x) x[4])
	res$pow_avg <- mean(unlist(pow_win), na.rm = T)

	invs_win <- lapply(wel_res, function(x) x[5])
	res$invs_avg <- mean(unlist(invs_win), na.rm = T)

	prelec_win <- lapply(wel_res, function(x) x[6])
	res$prelec_avg <- mean(unlist(prelec_win), na.rm = T)

	tot <- 1 - res$fails

	res$correct_tot    <- res$correct    / tot
	res$eut_avg_tot    <- res$eut_avg    / tot
	res$pow_avg_tot    <- res$pow_avg    / tot
	res$invs_avg_tot   <- res$invs_avg   / tot
	res$prelec_avg_tot <- res$prelec_avg / tot
	
	print(res)

	return(res)

}

res <- c.lapplyLB(1:NN, simRun, SIM = ALL)
print(res)


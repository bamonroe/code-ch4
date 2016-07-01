library(ctools)
c.library("dplyr","optimx","halton")

# Clean the Raw data from Glenn into my format
c.source("../HNG/clean.r", "../MSL/optim.r")
c.sourceCpp("../Rcpp/MSL.cpp", on.main=F)

# Now for MSL stuff
sim      <- c(rm=1.3, rs=.3, um=.15, us=.05, rh=.3)
sim[2:4] <- log(sim[2:4]^3)	
sim[5]   <- log((-sim[5] -1) / (sim[5] -1))

config  <- list(method="BFGS", itnmax = 1000)
config  <- list(method="Nelder-Mead", itnmax = 1000)
control <- list(trace=1, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)

HH <- 50

# Demographics
dempars      <- list()
dempars[[1]] <- c(Female=0,black=0,young=0)		# Correlate for rm
dempars[[2]] <- c(Female=0,black=0,young=0)		# Correlate for rs
dempars[[3]] <- c(Female=0,black=0,young=0)		# Correlate for um
dempars[[4]] <- c(Female=0,black=0,young=0)		# Correlate for us
dempars[[5]] <- c(Female=0,black=0,young=0)		# Correlate for rho

# Pass the actual Instrument (D here) to do.optimx.MSL
#out <- try.optimx.MSL(par = sim, inst = D, dempars = dempars, HH = HH, model = "EUT",
#				config = config, control= control)

out <- do.optimx.MSL.EUT(par = sim, inst = D, dempars = dempars, HH = HH,
				config = config, control= control)

out

library(cgen)
library(dplyr)

rmin <- -1.5
rmax <- 1.5
amin <- 0.1
amax <- 2.0
bmin <- 0.1
bmax <- 2.0
umax <- .35
umin <- .01

EUT.pop <- c(rmax = rmax, rmin = rmin, umax = umax, umin = umin, ru = 0)
INV.pop <- c(rmax = rmax, rmin = rmin, amax = amax, amin = amin, umax = umax, us = umin, ra = 0, ru = 0, au = 0)
POW.pop <- c(rmax = rmax, rmin = rmin, amax = amax, amin = amin, umax = umax, us = umin, ra = 0, ru = 0, au = 0)
PRE.pop <- c(rmax = rmax, rmin = rmin, amax = amax, amin = amin, bm = bmax, bs = bmin, um = umax, us = umin, ra = 0, rb =0, ru = 0, au = 0, bu = 0, ab = 0)

n <- 100

# How big are the populations for each model - in parts per parts per thousand
EUT.N <- 1
POW.N <- 1
INV.N <- 1
PRE.N <- 1

prop <- EUT.N + POW.N + INV.N + PRE.N
EUT.N <- round((EUT.N / prop) * n)
POW.N <- round((POW.N / prop) * n)
INV.N <- round((INV.N / prop) * n)
PRE.N <- round((PRE.N / prop) * n)

TOT.N <- 0

# Which instrument
inst <- "HL"
inst <- "HO"
inst <- "LMS30"
inst <- c("HNG", "HO", "HNG.ins")
inst <- "HNG"

# Which population(s)
u.pop <- c("EUT", "POW", "INV", "PRE")

# Generate our datasets
DAT <- list()
if ("EUT" %in% u.pop & EUT.N > 0) {
	DAT[["EUT"]] <- genChoice(EUT.pop, N = EUT.N, inst = inst, pfunc = "EUT", ufunc = "CRRA", unif = T)
	DAT[["EUT"]]$alpha <- NA
	DAT[["EUT"]]$beta  <- NA
	TOT.N <- TOT.N + EUT.N
}
if ("POW" %in% u.pop & POW.N > 0) {
	DAT[["POW"]] <- genChoice(POW.pop, N = POW.N, inst = inst, pfunc = "pow", ufunc = "CRRA", unif = T)
	DAT[["POW"]]$beta <- NA
	TOT.N <- TOT.N + POW.N
} 
if ("INV" %in% u.pop & INV.N > 0) {
	DAT[["INV"]] <- genChoice(INV.pop, N = INV.N, inst = inst, pfunc = "invs", ufunc = "CRRA", unif = T)
	DAT[["INV"]]$beta <- NA
	TOT.N <- TOT.N + INV.N
}
if ("PRE" %in% u.pop & PRE.N > 0) {
	DAT[["PRE"]] <- genChoice(PRE.pop, N = PRE.N, inst = inst, pfunc = "prelec", ufunc = "CRRA", unif = T)
	TOT.N <- TOT.N + PRE.N
} 
DAT <- do.call(rbind, DAT)

# Need to renumber IDs when binding
# First grab number of tasks
TT <- nrow(DAT) / TOT.N
# Replace every TT rows of ID with a counted ID number
for (i in 1:TOT.N) {
	start <- ((i-1)*TT)+1
	end   <- (i*TT)
	DAT$ID[start:end] <- i
}

# list with each element a subject's data
persub <- split(DAT, DAT$ID)

save(persub, file = "subdat.Rda")

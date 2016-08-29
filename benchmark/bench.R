library(ctools)
c.library("microbenchmark")

c.source("../MSL/optim.r","../choice-gen/geninst.r")
c.sourceCpp("../Rcpp/MSL.cpp")

# Define the sim population
sim <- c(rm=.6, rs=.3, um=.15, us=.05, rh=.3)
N <- 250

# Generate a simulated instrument
D <- genHL(sim, N)

head(D, n = 10)

stop()

HH1 <- 50
HH2 <- 150

# Demographics
dempars      <- list()

pars      <- sim
pars[2:4] <- log(sim[2:4]^3)	
pars[5]   <- log((-sim[5] -1) / (sim[5] -1))

# Split full-long dataset to list of per-subject long datsets
INST     <- opt.getInst(D)

# Fill in the elements of covariates not specified with NULL
dempars  <- opt.getDemPars(dempars, pars)

# Make a per-subject list containing covariates for each subject
dempass  <- opt.getDemPass(dempars, D)

# Get the column index of the covnames in covpass
demindex <- opt.getDemIndex(dempars, dempass)

# have all the initial values of the covariates go after the parameters
fullpars <- opt.getFullPars(dempars, pars)

# Split the covariates by ID and then turn them into a list
dempass <- opt.splitDem(dempass)

# Number of Subjects
N <- length(INST)

# Generate Halton Sequences
HR1 <- matrix(hunif(HH1*N ,prime = 3, burn=45 ), nrow = N, ncol = HH1, byrow=F)
HU1 <- matrix(hunif(HH1*N ,prime = 7, burn=45 ), nrow = N, ncol = HH1, byrow=F)

HR2 <- matrix(hunif(HH2*N ,prime = 3, burn=45 ), nrow = N, ncol = HH2, byrow=F)
HU2 <- matrix(hunif(HH2*N ,prime = 7, burn=45 ), nrow = N, ncol = HH2, byrow=F)

# Run the optimization
microbenchmark(
	MSL_EUT_cov(fullpars, covars = dempass, covindex = demindex, h1 = HR1, h2 = HU1, Inst = INST),
	MSL_EUT_cov(fullpars, covars = dempass, covindex = demindex, h1 = HR2, h2 = HU2, Inst = INST)
)


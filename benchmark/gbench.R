library(ctools)
c.library("microbenchmark")

c.source("../choice-gen/geninst.r")
c.sourceCpp("../Rcpp/genEUT.cpp")

set.seed(42)

# Define the sim population
sim <- c(rm=.6, rs=.3, um=.15, us=.05, rh=.3)
N <- 100

# Generate a simulated instrument
D <- genHL(sim, N)
DD <- genHLcpp(sim, N)

microbenchmark(
		genHL(sim, N),
		genHLcpp(sim, N),
		times=50
)

set.seed(42)
D  <- genHNG(sim, N)
set.seed(42)
DD <- genHNGcpp(sim, N)

microbenchmark(
		genHNG(sim, N),
		genHNGcpp(sim, N),
		times=50
)


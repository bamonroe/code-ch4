# Load the HNG cleaned data
library(ctools)
c.library("MSL", "dplyr")
hng_res_dir <- "../data/HNG_res/"
load(paste0(hng_res_dir, "HNG.Rda"))

HNG <- DAT %>%
	filter(Inst == "HNG")

# Some configurations
opt  <- "BHHH" # BHHH optimizer Method
opt  <- "NM"   # Nelder-Mead optimizer Method
opt  <- "BFGS" # BFGS optimizer Method
opt  <- list(PRE = "BFGS")   # set PRE to use BFGS and the rest as NR
opt  <- "NR"   # Newton Rhapson optimizer Method
opt  <- list(EUT = "NR", POW = "NR", INV = "NR", PRE = "BFGS")   # set PRE to use BFGS and the rest as NR
opt  <- list(EUT = "NR", POW = "NR", INV = "NR", PRE = "NR")   # set PRE to use BFGS and the rest as NR
inum <- 500    # Iteration limit

# HNG Transform the outcomes

HNG$A0 <- ((HNG$A0 - 10)/40)/20
HNG$A1 <- ((HNG$A1 - 10)/40)/20
HNG$A2 <- ((HNG$A2 - 10)/40)/20
HNG$B0 <- ((HNG$B0 - 10)/40)/20
HNG$B1 <- ((HNG$B1 - 10)/40)/20
HNG$B2 <- ((HNG$B2 - 10)/40)/20



sub.est <- ML.all(HNG, init = "POOLED", iterations = inum, do.par = T)
sub.df  <- ML.dataframe(sub.est)

save(sub.est, sub.df, file = paste0(hng_res_dir, "HNG_est.Rda"))

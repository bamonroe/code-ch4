library(ctools)
c.library("MSL","cgen","welfare","halton")

c.source("perSim.R")

# Set runs to be a multiple of the number of cores available
NN <- c.cores()

rm <- hunif(NN, min = -0.50, max = 1.50)
rs <- hunif(NN, min =  0.05, max = 1.50)
um <- hunif(NN, min =  0.08, max = 0.80)
us <- hunif(NN, min =  0.05, max = 0.50)
rh <- hunif(NN, min = -0.67, max = 0.67)

SIM <- data.frame(rbind(rm, rs, um, us, rh))
SIM

res <- c.lapply(SIM, runSimEUT)

res


library(ctools)
c.library("MSL", "welfare", "dplyr")

do.clean    <- F
do.estimate <- T
do.appc     <- F
do.fhng     <- F
do.wel      <- T

# Take thr original HNG data and clean it up for use in R and my MSL library
if (do.clean)    source("clean.R", echo = T)

# Estimate the HNG data, both with the HNG transformations of outcomes and without
if (do.estimate) source("estimate.R", echo = T)

# Try to link up the estimates I've got with the appendix C data
if (do.appc)     source("appc.R", echo = T)

# Rename the HNG dataset in a way that matches my format
if (do.fhng)     source("ind_est.R", echo = T)

# Do the welfare calculations on HNG
if (do.wel)      source("welfare.R", echo = T)

library(ctools)
c.library("MSL", "welfare", "dplyr")

do.clean    <- F
do.estimate <- F
do.format   <- F
do.common   <- F
do.wel      <- F
do.plots    <- T

# Take thr original HNG data and clean it up for use in R and my MSL library
if (do.clean)    source("clean.R", echo = T)

# Estimate the HNG data, both with the HNG transformations of outcomes and without
if (do.estimate) source("estimate.R", echo = T)

# Rename the HNG dataset in a way that matches my format
if (do.format)   source("format.R", echo = T)

# Find the common stuff in our estimates
if (do.common)   source("common.R", echo = T)

# Do the welfare calculations on HNG
if (do.wel)      source("welfare.R", echo = T)

# Regenerate a few plots from HNG
if (do.plots)    source("plots.R", echo =T)

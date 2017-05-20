library(ctools)
c.library("dplyr", "MSL", "mgcv", "halton", "ggplot2", "cowplot")

insts   <- c("HNG_1", "HNG", "HO", "LMS20", "LMS30", "SH")
insts   <- c("HNG_1", "HNG")
insts   <- c("HNG_1")

wel_vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel_var  <- wel_vars[1]

# Directories of note
data_dir <- paste0(getwd(), "/data/classify/full/")
fit_dir  <- paste0(getwd(), "/data/lo_fits/")
plot_dir <- paste0(getwd(), "/plots/")

# Which models are we doing this for
mods <- c("EUT", "PRE")

# "defaults" is reverse-ordered.
#The first element, is beaten by the second, which is beaten by the third, and so on.
defaults <- c("EUT", "PRE") 

# Which vars are we going to make predictions/plots for
win_vars <- c("win_05", "default")

# Fraction of the dataset to use, there's 250k oberservations per model
mini_frac <- 1

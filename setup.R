library(ctools)
c.library("dplyr", "MSL", "mgcv", "halton", "ggplot2", "cowplot", "haven", "welfare")

insts   <- c("HNG_1", "HNG", "HO", "LMS20", "LMS30", "SH")
insts   <- c("HNG_1")
insts   <- c("HNG_1", "HNG")

wel_vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel_var  <- wel_vars[1]

# Directories of note
merged_dir   <- paste0(getwd(), "/data/raw/merged")
data_dir     <- paste0(getwd(), "/data/full/")
fit_dir      <- paste0(getwd(), "/data/lo_fits/")
plot_dir     <- paste0(getwd(), "/plots/")

hng_plot_dir <- paste0(plot_dir, "real/")
hng_data_dir <- paste0(getwd(), "/data/HNG/data/")
hng_res_dir  <- paste0(getwd(), "/data/HNG/res/")

# If any of the non-raw dirs don't exist, make them
for (dir in c(data_dir, fit_dir, plot_dir, hng_res_dir, hng_plot_dir)) {
	if (! dir.exists(dir)) dir.create(dir)
}

# Which models are we considering estimates for?
mods     <- c("EUT", "INV", "POW", "PRE")
mods     <- c("EUT", "POW", "PRE")
mods     <- c("EUT", "PRE")
# Which models are we considering subjects for?
pop_mods <- c("EUT", "PRE")

# "defaults" is reverse-ordered.
#The first element, is beaten by the second, which is beaten by the third, and so on.
defaults <- c("EUT", "POW", "PRE")
defaults <- mods

# Which vars are we going to make predictions/plots for
win_vars <- c("win_05", "default")

# Fraction of the dataset to use, there's 250k oberservations per model
mini_frac <- 1

# Export all of this
c.export(ls())

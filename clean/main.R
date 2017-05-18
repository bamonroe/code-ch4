library(ctools)
c.library("dplyr", "MSL")

# Some common config stuff
insts   <- c("HNG_1", "HNG")
insts   <- c("HNG_1", "HNG", "HO", "LMS20", "LMS30", "SH")
data_dir <- "../data/classify/full/"
wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel.var  <- wel.vars[1]

mods <- c("EUT", "PRE")

# "defaults" is reverse-ordered.
#The first element, is beaten by the second, which is beaten by the third, and so on.
defaults <- c("EUT", "PRE") 

# Which things are we "doing"
do_rename <- T # My naming conventions have changed, into better things I think, implement them
do_mini   <- T
do_win    <- T

# Do them
if (do_rename) {
	source("renamer.R")
	null <- c.lapply(insts, mkbak)
}

if (do_mini) {
	source("mini.R")
	null <- c.lapply(insts, mkmini_1, mods = mods)
}

if (do_win) {
	source("winner.R")
	null <- c.lapply(insts, mkwin)
}

if (do_mini) {
	null <- lapply(insts, mkmini_2, mods = mods, wel_var = wel_var)
}

if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

doPAR <- T
lfun <- ifelse(doPAR, c.lapply, lapply)


do_rename     <- T # My naming conventions have changed, into better things I think, implement them
do_mini1      <- T
do_win        <- T

do_win2       <- F
do_win2_merge <- F

do_mini2      <- T

if (do_rename) {
	source("renamer.R")
	null <- lfun(insts, mkbak)
}

if (do_mini1) {
	source("mini.R")
	null <- lfun(insts, mkmini_1, mods = mods)
}

if (do_win) {
	source("winner.R")
	null <- lfun(insts, mkwin)
}
if (do_win2) {
	source("winner.R")
	null <- lapply(insts, win2)
}
if (do_win2_merge) {
	source("winner.R")
	null <- lapply(insts, win2_merge)
}

if (do_mini2) {
	source("mini.R")
	null <- lfun(insts, mkmini_2, mods = mods, wel_var = wel_var)
}


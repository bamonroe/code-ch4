if (!exists("from_main")) {
	here <- getwd()
	setwd("../")
	source("setup.R")
	setwd(here)
}

do_rename <- T # My naming conventions have changed, into better things I think, implement them
do_mini1  <- T
do_mini2  <- T
do_win    <- T

if (do_rename) {
	source("renamer.R")
	null <- c.lapply(insts, mkbak)
}

if (do_mini1) {
	source("mini.R")
	null <- c.lapply(insts, mkmini_1, mods = mods)
}

if (do_win) {
	source("winner.R")
	null <- c.lapply(insts, mkwin)
}

if (do_mini2) {
	source("mini.R")
	null <- lapply(insts, mkmini_2, mods = mods, wel_var = wel_var)
}


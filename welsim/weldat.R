library(ctools)
c.library("dplyr")

dloc <- "../data/classify/rawdat"
wloc <- "../data/classify/weldat"
files <- list.files(dloc)

getIns <- function(dat) {
	print(dat)
	load(paste0(dloc, "/", dat))
	weldat <- lapply(persub, function(sub) {
		sub %>% 
			filter(Inst == "HNG.ins")
	})
	save(weldat, file = paste0(wloc, "/", dat))
}

c.lapply(files, getIns)

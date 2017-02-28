library(ctools)
c.library("dplyr")

rdir <- "../data/classify/rawdat/"
wdir <- "../data/classify/weldat/"

rfiles <- list.files(rdir)

c.lapply(rfiles, function(f) {
	print(f)
	load(paste0(rdir, "/", f))
	weldat <- lapply(persub, function(sub) {
		sub %>%
			filter(Inst == "HNG.ins")
	})
	save(weldat, file = paste0(wdir, "/", f))
})

library(ctools)
c.library("dplyr")

rdir <- "rawdat"
wdir <- "weldat"

rfiles <- list.files(rdir)

c.export("rdir", "wdir")

c.lapply(rfiles, function(f) {
	print(f)
	load(paste0(rdir, "/", f))
	weldat <- lapply(persub, function(sub) {
		sub %>%
			filter(Inst == "HNG.ins")
	})
	save(weldat, file = paste0(wdir, "/wel-", f))
})

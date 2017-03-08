library(ctools)

mergeres <- function(iname) {

	rdir <- "results"
	wcdir <- "welcalc"
	mdir <- "merged"

	rfiles <- list.files(path = rdir, pattern = paste0(iname, "_res"), full.names = T)
	wfiles <- list.files(path = wcdir, pattern = paste0(iname, "-wel-subdat"), full.names = T)

	resdat <- list()
	for (i in 1:length(rfiles)) {
		print(rfiles[i])
		print(wfiles[i])
		load(rfiles[i])
		load(wfiles[i])

		res <- get(paste0(iname, ".res"))
		wel <- get(paste0(iname, ".wel"))

		resdat[[i]] <- cbind(get(paste0(iname, ".res")), get(paste0(iname, ".wel")))
	}

	assign(iname, do.call(rbind, resdat))

	save(list = c(iname), file = paste0(mdir, "/", iname, ".Rda"))

	rm(list= c(iname, "resdat"))
}

insts <- c("SH", "HNG", "HNG_1", "SH", "HO", "LMS20", "LMS30")
out <- lapply(insts, mergeres)

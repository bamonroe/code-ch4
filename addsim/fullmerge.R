mergeres <- function(iname) {
	cat(iname, "\n")
	fnames <- list.files(path = new_merged_dir, pattern = paste0(iname, "-.*\\.Rda"), full.names = T)

	resdat <- lapply(fnames, function(fname, iname) {
		load(fname)
		dat <- get(iname)
		return(dat)
	}, iname = iname)

	assign(iname, do.call(rbind, resdat))

	save(list = c(iname), file = paste0(new_full_dir, "/", iname, ".Rda"))
	save(list = c(iname), file = paste0(data_dir, iname, ".Rda"))

	rm(list= c(iname, "resdat"))
}

insts <- paste0("HNG_", multiples)
out <- c.lapply(insts, mergeres)

perFile <- function(fname) {
	suffix    <- sub(".*-", "", fname)
	save_file <- paste0(new_weldat_dir, "/weldat-", suffix)
	if (file.exists(save_file)) {return()}
	print(save_file)
	load(fname)
	weldat <- lapply(persub, function(sub) {filter(sub, Inst == "HNG.ins")})
	save(weldat, file = save_file)
}

rfiles <- list.files(new_rawdat_dir, pattern = ".*\\.Rda", full.names = T)
c.lapply(rfiles, perFile)

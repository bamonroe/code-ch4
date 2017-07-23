perInst <- function(inst, sub.est, real, suffix) {
		save_file <- paste0(new_df_dir, "/", inst, "-", suffix)
		if (file.exists(save_file)) return()
		dat <- lapply(sub.est, function(sub) {
			sub[[inst]]
		})
		dat <- ML.dataframe(dat)
		dat <- cbind(dat, real)
		assign(inst, dat)
		save(list = inst, file = save_file)
		return(dat)
}

perFile <- function(fname) {
	suffix <- sub(".*-", "", fname)
	insts  <- paste0("HNG_", multiples)

	# Have we done all the things for this file? Then skip this iteration
	skipit <- lapply(insts, function(inst) {
		save_file <- paste0(new_df_dir, "/", inst, "-", suffix)
		file.exists(save_file)
	})
	skipit <- do.call(c, skipit)
	if (all(skipit)) return()

	cat("Instsplit for", fname, "\n")

	load(fname)
	out <- lapply(insts, perInst, sub.est = sub.est, real = real, suffix = suffix)

	names(out) <- insts
	return(out)
}


rr <- c(1:20)
for (i in rr) {
	fpat <- paste0("sub_est_new-1.")

	files <- list.files(path = new_est_dir, pattern = fpat, full.names = T)
	null <- c.lapply(files, perFile)

	print(warnings())
}

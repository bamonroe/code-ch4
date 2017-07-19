perInst <- function(inst, sub.est, real, suffix) {
		save_file <- paste0(new_df_dir, "/", inst, "-", suffix)
		if (file.exists(save_file)) return()
		dat <- lapply(sub.est, function(sub) {
			sub[[inst]]
		})
		dat <- ML.dataframe(dat)
		dat <- cbind(inst, real)
		dat$ID <- (1:nrow(dat)) + round(runif(nrow(dat)) * 1000000)
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

	load(fname)
	out <- lapply(insts, perInst, sub.est = sub.est, real = real, suffix = suffix)

	names(out) <- insts
	return(out)
}

efiles <- list.files(new_est_dir, full.names = T)
null <- lapply(efiles, perFile)


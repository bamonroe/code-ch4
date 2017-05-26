est_2_inst <- function(inst) {
	for (i in 1:20) {
		cat(c("Building", inst, "file num", i, "\n"))
		edir   <- paste0(raw_dir, "estimates/")
		# This vector is needed to match up with the df files in "merged"
		fnum <- c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 1, 20, 21, 22, 23, 24, 25, 2, 3, 4, 5, 6, 7, 8, 9)
		# A list of lists for the instruments in question
		inst_list <- list()
		for (j in fnum) {
			f <- paste0(edir, "sub_est-", i, ".", j, ".Rda")
			load(f)
			ilen <- length(inst_list)
			for (s in 1:length(sub.est)) {
				inst_list[[ilen + s]] <- sub.est[[s]][[inst]]
			}
		}
		rm(sub.est)
		oname <- paste0(inst, "_est")
		assign(oname, inst_list)
		fname <- paste0(est_dir, inst, "-", i, ".Rda")
		cat(c("Saving", inst, "file num", i, "\n"))
		save(list = oname, file = fname)
		rm(list = oname)
		rm(inst_list)
	}
}

merge_est <- function(inst) {
	oname <- paste0(inst, "_est")
	cat(c("Building", inst,"\n"))
	est.dat <- c.lapply(1:20, function(i) {
		cat(c("  loading file", i, "\n"))
		edir  <- paste0(raw_dir, "estimates/")
		fname <- paste0(est_dir, inst, "-", i, ".Rda")
		load(fname)
		est <- get(oname)
		rm(list = oname)
		return(est)
	})
	cat(c("  rbinding\n"))
	est.dat <-do.call(rbind, est.dat)
	assign(oname, est.dat)
	cat(c("  saving\n"))
	save(list = oname, file = paste0(data_dir, inst, "_est.Rda"))
	rm(list = c(oname, "est.dat"))
}


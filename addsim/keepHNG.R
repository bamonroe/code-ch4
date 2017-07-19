
new_rawdat_dir  <- paste0(raw_dir, "new_rawdat")

keep_HNG <- function(fname) {
	cat("  Dropping for", fname, "\n")

	suffix <- sub(".*/", "", fname)
	save_file <- paste0(new_rawdat_dir, "/HNG-", suffix)

	e <- tryCatch(load(fname), error = function(e) {NA})
	if (is.na(e)) return()

	tokeep <- lapply(persub, function(sub) {
		if (sub$pfunc[1] %in% c("invs", "pow")) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	})

	tokeep <- which(unlist(tokeep))
	persub <- persub[tokeep]

	persub <- lapply(persub, function(sub) {
		sub %>%
		filter(Inst == "HNG" | Inst == "HNG.ins")
	})

	save(persub, file = save_file)
}

old_raw_dat <- paste0(raw_dir, "rawdat")
subfiles <- list.files(path = old_raw_dat, pattern = "^subdat", full.names = T)

cat("Dropping non HNG data\n")
lapply(subfiles, keep_HNG)

print(warnings())


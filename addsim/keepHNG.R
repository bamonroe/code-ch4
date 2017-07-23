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
		sub$Inst <- ifelse(sub$Inst == "HNG.lot", "HNG", sub$Inst)
		sub %>%
		filter(Inst == "HNG" | Inst == "HNG.ins")
	})

	save(persub, file = save_file)
}

old_raw_dat <- paste0(raw_dir, "rawdat")
new_raw_dat <- paste0(raw_dir, "new_rawraw")
subfiles0 <- list.files(path = old_raw_dat, pattern = "^subdat", full.names = T)
subfiles1 <- list.files(path = new_raw_dat, pattern = "^subdat", full.names = T)

subfiles <- c(subfiles0, subfiles1)
subfiles <- subfiles1

cat("Dropping non HNG data\n")
c.lapply(subfiles, keep_HNG)

print(warnings())


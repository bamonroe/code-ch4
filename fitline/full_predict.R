fpred <- function(inst) {
	dbug <- 1

	dat_load_suffix <- "-bak.Rda"
	load(paste0(data_dir, inst, dat_load_suffix))
	dat <- get(inst)

	for (class_var in win_vars) {
		cat(c(rep(" ", 2), "Class var:", class_var), "\n")

		fit_name <- paste0(inst, "_", class_var, "_fit")
		fit_load_suffix <- ".Rda"

		load(paste0(fit_dir, fit_name, fit_load_suffix))

		# Grab these things as known objects
		fit <- get(fit_name)

		dat0 <- lapply(mods, function(mod) {
			cat(c(rep(" ", 4), "Mod:", mod), "\n")
			mmods <- c(mods, "NA")
			dat1 <- dat %>% filter(model == mod)
			for (mmod in mmods) {
				cat(c(rep(" ", 6), "MMod:", mmod), "\n")
				vname <- paste0(mod, "_", mmod, "_", class_var)
				cat(c(rep(" ", 8), "Predicting Prob"), "\n")
				dat1[[paste0(vname, "_prob")]] <- predict(fit[[mod]][[mmod]][["prob"]], dat1)
				# Don't predict welfare for "NA" - we don't have any welfare predictions anyway
				if (mmod != "NA") {
					cat(c(rep(" ", 8), "Predicting Wel"), "\n")
					dat1[[paste0(vname, "_wel")]]  <- predict(fit[[mod]][[mmod]][["wel"]], dat1)
				}
			}
			return(dat1)
		})

		# We need to reconcile the names difference from Above
		full_names <- lapply(dat0, names)
		full_names <- do.call(c, full_names)
		full_names <- unique(full_names)

		dat0 <- lapply(dat0, function(dat1, full_names) {
			onames    <- names(dat1)
			new_names <- full_names[(! full_names %in% onames)]
			dat1[, new_names] <- NA
			dat1
		}, full_names = full_names)

		dat <- do.call(rbind, dat0)
	}
	dat %>% head %>% print
	assign(inst, dat)

	save(list = inst, paste0(data_dir, inst, dat_load_suffix))
}

c.lapply(insts, fpred)

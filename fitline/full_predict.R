fpred <- function(inst) {
	#dbug <- 1
	cat(c("\n","Predicting for:", inst, "\n"))

	dat_load_suffix <- "-bak.Rda"
	dat_save_suffix <- "-fitted.Rda"
	load(paste0(data_dir, inst, dat_load_suffix))
	dat <- get(inst)

	for (class_var in win_vars) {
		cat(c(rep(" ", 2), "Class var:", class_var, "\n"))

		fit_name        <- paste0(inst, "_", class_var, "_fit")
		fit_name_na     <- paste0(fit_name, "_na")
		fit_load_suffix <- ".Rda"

		load(paste0(fit_dir, fit_name, fit_load_suffix))

		# Grab these things as known objects
		fit    <- get(fit_name)
		fit_na <- get(fit_name_na)

		dat0 <- lapply(pop_mods, function(mod) {
			cat(c(rep(" ", 4), "Mod:", mod), "\n")
			mmods <- c(mods, "NA")
			dat1 <- dat %>% filter(model == mod)
			for (mmod in mmods) {
				cat(c(rep(" ", 6), "MMod:", mmod, "\n"))

				# Calculate the number of standard errors needed to get to confidence interval
				conf_interval <- 0.95
				conf_se       <- abs(qnorm((1 - conf_interval)/2))
				vname <- paste0(class_var, "_", mmod)

				cat(c(rep(" ", 8), "Predicting Prob - NA", "\n"))
				pred_na  <- predict(fit_na[[mod]][[mmod]][["prob"]], dat1, se.fit = T)
				dat1[[paste0(vname, "_prob_na")]]     <- pred_na$fit
				dat1[[paste0(vname, "_prob_na_se")]]  <- pred_na$se.fit
				dat1[[paste0(vname, "_prob_na_U95")]] <- pred_na$fit + (pred_na$se.fit * conf_se)
				dat1[[paste0(vname, "_prob_na_L95")]] <- pred_na$fit - (pred_na$se.fit * conf_se)

				# Don't predict welfare for "NA" - we don't have any welfare predictions anyway
				if (mmod != "NA") {
					cat(c(rep(" ", 8), "Predicting Prob", "\n"))
					pred  <- predict(fit[[mod]][[mmod]][["prob"]], dat1, se.fit = T)
					dat1[[paste0(vname, "_prob")]]     <- pred$fit
					dat1[[paste0(vname, "_prob_se")]]  <- pred$se.fit
					dat1[[paste0(vname, "_prob_U95")]] <- pred$fit + (pred$se.fit * conf_se)
					dat1[[paste0(vname, "_prob_L95")]] <- pred$fit - (pred$se.fit * conf_se)

					cat(c(rep(" ", 8), "Predicting Wel", "\n"))
					pred  <- predict(fit[[mod]][[mmod]][["wel"]], dat1, se.fit = T)
					dat1[[paste0(vname, "_wel")]]     <- pred$fit
					dat1[[paste0(vname, "_wel_se")]]  <- pred$se.fit
					dat1[[paste0(vname, "_wel_U95")]] <- pred$fit + (pred$se.fit * conf_se)
					dat1[[paste0(vname, "_wel_L95")]] <- pred$fit - (pred$se.fit * conf_se)
				}
			}
			return(dat1)
		})
		dat <- do.call(rbind, dat0)

		# Expected welfare - point estimates only...
		ewel <- paste0(class_var, "_ewel_point")
		dat[[ewel]] <- 0
		for (mod in mods) {
			pname <- paste0(class_var, "_", mod, "_prob")
			wname <- paste0(class_var, "_", mod, "_wel")
			dat[[ewel]] <- dat[[ewel]] + (dat[[pname]] * dat[[wname]])
		}

	}
	#dat %>% head %>% print
	assign(inst, dat)

	save(list = inst, file = paste0(data_dir, inst, dat_save_suffix))
}

c.lapply(insts, fpred)

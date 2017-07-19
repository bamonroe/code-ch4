
perSub <- function(s, sub.est, weldat) {
	sub  <- sub.est[[s]]
	wdat <- weldat[[s]]
	# Calculate
	est.wel <- lapply(sub, function (inst) {
		inst <- lapply(inst, function(mod) {
			if (is.na(mod[1])) {
				return(matrix(rep(NA, 9), nrow = 1))
			} else {
				return(welCalc(mod, wdat, boot = T))
			}
		})
	})

	# The real parameters and pfunc
	r.par <- c(r = wdat$r[1], alpha = wdat$alpha[1], beta = wdat$beta[1], mu =wdat$mu[1])
	r.par <- r.par[which(!is.na(r.par))]
	rmod <- list(estimates = r.par, pfunc = toupper(wdat$pfunc[1]))

	# The real welfare estimates
	r.wel <- welCalc(rmod, wdat, boot =F)

	w.names <- c("WelSurplus", "WelMax", "WelMaxDiff", "WelEfficiency", "CEchoice", "CEmax", "Prob")
	w.names <- colnames(r.wel)

	mod.names <- c(names(est.wel[[1]]), "real")

	# Change the modnames to be slightly shorted and correspond to the simulated data data.frames
	mod.names <- ifelse(mod.names == "PRELEC",  "PRE", mod.names)

	h.names <- lapply(mod.names, function(n) {paste0(n, "_", w.names)})
	h.names <- do.call(c, h.names)

	insts <- paste0("HNG_", multiples)

	out <- lapply(insts, function(inst) {
		erow <- do.call(cbind, est.wel[[inst]])
		erow <- cbind(erow, r.wel)
		colnames(erow) <- h.names
		return(erow)
	})
	names(out) <- insts

	return(out)
}

perFindex <- function(fname) {
	cat(fname, "\n")
	suffix <- sub(".*-", "", fname)
	insts  <- paste0("HNG_", multiples)

	## Have we done all the things for this file? Then skip this iteration
	#skipit <- lapply(insts, function(inst) {
	#	save_file <- paste0(new_merged_dir, "/", inst, "-", suffix)
	#	file.exists(save_file)
	#})
	#skipit <- do.call(c, skipit)
	#if (all(skipit)) return()

	# Load the raw estimates and the insurance task choices for the same subjects
	load(fname)
	load(paste0(new_weldat_dir, "/", "weldat-", suffix))

	# Calculate the welfare stats per subject
	sub.wel <- c.lapply(1:length(sub.est), perSub, sub.est = sub.est, weldat = weldat)

	# Change the per-subject list of instruments to per instrument list of subjects
	byinst <- lapply(insts, function(inst) {
		lapply(sub.wel, function(sub) {sub[[inst]]})
	})
	byinst <- lapply(byinst, function(idat) {do.call(rbind, idat)})
	names(byinst) <- insts

	# Merge the dataframes for each batch of subjects with this batch of welfare
	# calculations
	saved <- lapply(insts, function(inst) {
		dat_file  <- paste0(new_df_dir, "/", inst, "-", suffix)
		save_file <- paste0(new_merged_dir, "/", inst, "-", suffix)
		load(dat_file)
		dat <- get(inst)
		dat <- cbind(dat, byinst[[inst]])
		assign(inst, dat)
		save(list = inst, file = save_file)
		return(dat)
	})
}

# List the files for raw estimates and insurance task choices
efiles <- list.files(new_est_dir,    pattern = ".*\\.Rda", full.names = T)

cat("Estimates File:\n")
lapply(efiles, perFindex)

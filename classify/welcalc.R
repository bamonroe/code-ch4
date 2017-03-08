library(ctools)
c.library("MSL", "welfare")

edir <- "estimates"
wdir <- "weldat"
wcdir <- "welcalc"

e.files <- list.files(edir, full.names = F)
w.files <- list.files(wdir, full.names = F)

elen <- length(e.files)

c.export("edir", "wdir", "wcdir", "e.files", "w.files", "elen")

c.lapplyLB(1:length(e.files), function(f) {

	print(paste0(edir,"/",e.files[f], "  ==  ", f, "/", elen))

	#if (file.exists(paste0(wcdir,"/",w.files[f]))) {
	#	return(NA)
	#}

	load(paste0(edir,"/",e.files[f]))
	load(paste0(wdir,"/",w.files[f]))

	sub.wel <- lapply(1:length(sub.est), function(s) {
		sub  <- sub.est[[s]]
		wdat <- weldat[[s]]

		est.wel <- lapply(sub, function (inst) {
			inst <- lapply(inst, function(mod) {
				if (is.na(mod[1])) {
					return(matrix(rep(NA, 7), nrow = 1))
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

		mod.names <- c(names(est.wel$HNG), "real")

		# Change the modnames to be slightly shorted and correspond to the simulated data data.frames
		mod.names <- ifelse(mod.names == "EUT",  "eut", mod.names)
		mod.names <- ifelse(mod.names == "INVS", "inv", mod.names)
		mod.names <- ifelse(mod.names == "POW",  "pow", mod.names)
		mod.names <- ifelse(mod.names == "PRE",  "pre", mod.names)
		mod.names <- ifelse(mod.names == "PRELEC",  "pre", mod.names)
		
		h.names <- lapply(mod.names, function(n) {paste0(n, ".", w.names)})
		h.names <- do.call(c, h.names)

		insts <- c("SH", "HNG", "HNG_1", "SH", "HO", "LMS20", "LMS30")

		out <- lapply(insts, function(inst) {
			erow <- do.call(cbind, est.wel[[inst]])
			erow <- cbind(erow, r.wel)
			colnames(erow) <- h.names
			return(erow)
		})
		names(out) <- insts

		return(out)
	})

	insts <- c("SH", "HNG", "HNG_1", "SH", "HO", "LMS20", "LMS30")

	lapply(insts, function(inst) {
		assign(paste0(inst, ".wel"), lapply(sub.wel, function(sub) {sub[[inst]]}))
		assign(paste0(inst, ".wel"), do.call(rbind, get(paste0(inst, ".wel"))))
		save(list = paste0(inst, ".wel"), file = paste0(wcdir, "/", inst, "-", w.files[f]))
	})

})


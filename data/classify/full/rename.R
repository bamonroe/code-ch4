library(MSL)
insts <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")

for (inst in insts) {
	print(paste("Starting", inst))

	load(paste0(inst, ".Rda"))

	dat <- get(inst)

	varnames <- names(dat)

	nnames <- c()

	for(var in varnames) {
		if (length(grep("\\.", var)) == 1) {
			nvar <- gsub("\\.", "_", var)
		} else {
			nvar <- var
		}
		nnames <- c(nnames, nvar)
	}

	eut_pos <- grep("eut", nnames)
	eut_rep <- sub("eut", "EUT", nnames[eut_pos])
	nnames[eut_pos] <- eut_rep

	pow_pos <- grep("pow", nnames)
	pow_rep <- sub("pow", "POW", nnames[pow_pos])
	nnames[pow_pos] <- pow_rep

	inv_pos <- grep("inv", nnames)
	inv_rep <- sub("inv", "INV", nnames[inv_pos])
	nnames[inv_pos] <- inv_rep

	pre_pos <- grep("pre", nnames)
	pre_rep <- sub("pre", "PRE", nnames[pre_pos])
	nnames[pre_pos] <- pre_rep

	colnames(dat) <- nnames

	print(paste("Dropping weird things from", inst))

	for (m in c("EUT", "POW", "INV", "PRE")) {
		var <- paste0(m, "_r_Est")

		dat[which((dat[[var]] > 0.99) & (dat[[var]] < 1.01)), grep(m, names(dat)) ] <- NA
		dat[which(dat[[var]] < -15), grep(m, names(dat)) ] <- NA
		dat[which(dat[[var]] > 15), grep(m, names(dat)) ] <- NA

	}

	dat[which(dat$INV_alpha_Est < 0.28), grep("INV", names(dat))] <- NA
	dat[which(dat$INV_alpha_Est > 5.00), grep("INV", names(dat))] <- NA
	dat[which(dat$POW_alpha_Est > 5.00), grep("POW", names(dat))] <- NA
	dat[which(dat$PRE_alpha_Est > 5.00), grep("PRE", names(dat))] <- NA

	print(paste("Picking new winners for", inst))

	dat <- ML.winner(dat)

	assign(inst, dat)

	print(paste("Saving", inst))

	save(list=inst, file = paste0(inst, "-bak.Rda"))

	rm(list=inst)

}


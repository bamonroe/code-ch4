do.choice  <- T
do.sub     <- T
do.pval    <- T

load(paste0(hng_res_dir, "filtered.Rda"))
load(paste0(hng_res_dir, "HNG.Rda"))

HNG <- DAT %>%
	filter(Inst == "HNG")
INS <- DAT %>%
	filter(Inst == "HNG.ins") %>%
	ML.clean()

# Split the data by subject
ins.dat <- split(INS, INS$ID)

dat <- c()

# Go through each subject
if (do.choice) {
per.choice <- c.lapply(1:length(ins.dat), function (i) {

	# Get each subject's data, and estimates
	dat    <- ins.dat[[i]]
	est    <- sub.est[[i]]
	win    <- sub.df$win_05[i]
	enames <- names(est)

	if (is.na(win)) return(rep(NA, 9))
	# Loop through each model for each subject
	sub.wel <- lapply(enames, function(en) {
		e <- est[[en]]
		# If this model isn't the winner
		if (en != win)   return("nowin")
		if (is.na(e[1])) {
			nomat <- matrix(NA, nrow = 24, ncol = 9)
			colnames(nomat) <- c("WelSurplus", "WelMax", "WelMaxDiff", "WelEfficiency", "WellEfficiency2", "CEchoice", "CEuchoice", "CEmax", "Prob")
			return(nomat)
		}

		# split data by question ID
		cdat <- split(dat, dat$QID)

		# loop through each choice
		ww <- lapply(cdat, function(d) {
			# If the model is NA, skip it
			if (is.na(e[1])) return(rep(NA, 9))
			# Estimate each choice at each subject's estimates
			welCalc(e, d, boot = T, H = 500)
		})

		ww <- do.call(rbind, ww)

		return(ww)
	})

	names(sub.wel) <- enames

	sub.wel <- sub.wel[[win]]

	return(sub.wel)

})
per.choice <- do.call(rbind, per.choice)
per.choice <- data.frame(per.choice)

per.choice <- per.choice[which(!is.na(per.choice[,1])), ]

dat <- c("per.choice")

}

if (do.sub) {
per.sub <- c.lapply(1:length(ins.dat), function (i) {

	# Get each subject's data, and estimates
	dat    <- ins.dat[[i]]
	est    <- sub.est[[i]]
	win    <- sub.df$win_05[i]
	enames <- names(est)

	if (is.na(win)) return(rep(NA, 9))
	# Loop through each model for each subject
	sub.wel <- lapply(enames, function(en) {
		e <- est[[en]]
		# If this model isn't the winner
		if (en != win)   return("nowin")
		if (is.na(e[1])) {
			nomat <- matrix(NA, nrow = 24, ncol = 9)
			colnames(nomat) <- c("WelSurplus", "WelMax", "WelMaxDiff", "WelEfficiency", "WellEfficiency2", "CEchoice", "CEuchoice", "CEmax", "Prob")
			return(nomat)
		}

		# Estimate each choice at each subject's estimates
		ww <- welCalc(e, dat, boot = T, H = 500)

		return(ww)
	})

	names(sub.wel) <- enames

	sub.wel <- sub.wel[[win]]

	return(sub.wel)

})
per.sub <- do.call(rbind, per.sub)
per.sub <- data.frame(per.sub)

per.sub <- per.sub[which(!is.na(per.sub[,1])), ]

dat <- c("per.sub")

}

if (do.pval) {
pvals <- lapply(1:length(ins.dat), function (i) {
	# Get each subject's data, and estimates
	win.rdu  <- sub.df$RDU_win_05[i]
	win.full <- sub.df$win_05[i]

	if (is.na(win.rdu)) {
		out <- data.frame(WinnerRDU = NA, pvals = NA)
	} else {
		pval <- sub.df[[paste0(win.rdu,"_pval")]][i]
		out  <- data.frame(WinnerRDU = win.rdu, pvals = pval)
	}

	if (is.na(win.full)) {
		out$Winner <- NA
		out$pfull  <- NA
	} else {
		pval.full <- sub.df[[paste0(win.full,"_pval")]][i]
		out$Winner <- win.full
		out$pfull  <- win.full
	}

	return(out)

})
pvals <- do.call(rbind, pvals)
#pvals <- data.frame(pvals)
#colnames(pvals) <- c("Winner", "WinnerRDU", "pvals")
dat <- c(dat, "pvals")
}

save(list = dat, file = paste0(hng_res_dir, "wel_pval.Rda"))


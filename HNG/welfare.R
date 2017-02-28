library(ctools)
c.library("MSL", "welfare", "dplyr", "ggplot2")

load("HNG_est.Rda")
load("../data/HNG/HNG.Rda")

HNG <- DAT %>%
	filter(Inst == "HNG")
INS <- DAT %>%
	filter(Inst == "HNG.ins") %>%
	ML.clean()

# Split the data by subject
ins.dat <- split(INS, INS$ID)

# Go through each subject
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
#
#per.sub <- c.lapply(1:length(ins.dat), function (i) {
#
#	# Get each subject's data, and estimates
#	dat <- ins.dat[[i]]
#	est <- sub.est[[i]]
#	win <- sub.df$win_05[i]
#	enames <- names(est)
#
#	if (is.na(win)) return(rep(NA, 9))
#	# Loop through each model for each subject
#	sub.wel <- lapply(enames, function(en) {
#		e <- est[[en]]
#		# If this model isn't the winner
#		if (en != win)   return("nowin")
#		if (is.na(e[1])) {
#			nomat <- matrix(NA, nrow = 1, ncol = 9)
#			colnames(nomat) <- c("WelSurplus", "WelMax", "WelMaxDiff", "WelEfficiency", "WellEfficiency2", "CEchoice", "CEuchoice", "CEmax", "Prob")
#			return(nomat)
#		} 
#		ww <- welCalc(e, dat, boot = T, H = 500)
#		return(ww)
#	})
#
#	names(sub.wel) <- enames
#	sub.wel <- sub.wel[[win]]
#
#	return(sub.wel)
#})
#
#per.sub2 <- c.lapply(1:length(ins.dat), function (i) {
#
#	# Get each subject's data, and estimates
#	dat <- ins.dat[[i]]
#	est <- sub.est[[i]]
#	win <- sub.df$win_05[i]
#	enames <- names(est)
#
#	if (is.na(win)) return(rep(NA, 9))
#	# Loop through each model for each subject
#	sub.wel <- lapply(enames, function(en) {
#		e <- est[[en]]
#		# If this model isn't the winner
#		if (en != win)   return("nowin")
#		if (is.na(e[1])) {
#			nomat <- matrix(NA, nrow = 24, ncol = 9)
#			colnames(nomat) <- c("WelSurplus", "WelMax", "WelMaxDiff", "WelEfficiency", "WellEfficiency2", "CEchoice", "CEuchoice", "CEmax", "Prob")
#			return(nomat)
#		} 
#
#		# split data by question ID
#		cdat <- split(dat, dat$QID)
#
#		# loop through each choice
#		ww <- lapply(cdat, function(d) {
#			# If the model is NA, skip it
#			if (is.na(e[1])) return(rep(NA, 9))
#			# Estimate each choice at each subject's estimates
#			welCalc(e, d, boot = T, H = 500)
#		})
#
#		ww <- do.call(rbind, ww)
#		ww <- colSums(ww) / 24
#
#		return(ww)
#	})
#
#	names(sub.wel) <- enames
#
#	sub.wel <- sub.wel[[win]]
#
#	return(sub.wel)
#
#})
#

per.choice <- do.call(rbind, per.choice)
per.choice <- data.frame(per.choice)

per.choice <- per.choice[which(!is.na(per.choice[,1])), ]

p <- ggplot(per.choice)
p <- p + geom_vline(xintercept = 0, color = "red")
p <- p + geom_density(aes(WelSurplus), color = "blue")
p <- p + labs(title = paste0("N = ", nrow(per.choice), ", Mean = ", round(mean(per.choice$WelSurplus), digits = 2), ", Actual decisions made"), x = "Consumer Surplus ($)", y = "Density")

ggsave(filename = "HNG_CS.pdf", plot = p, device = "pdf")

p

#per.sub     <- do.call(rbind, per.sub)
#per.sub     <- data.frame(per.sub)
#per.sub     <- per.sub[which(!is.na(per.sub[,1])), ]
#per.sub$Eff <- per.sub$WelSurplus / per.sub$WelMax
#per.sub$we2 <- (per.sub$CEchoice - per.sub$CEuchoice) / per.sub$CEmax
#
#per.sub2     <- do.call(rbind, per.sub2)
#per.sub2     <- data.frame(per.sub2)
#per.sub2     <- per.sub2[which(!is.na(per.sub2[,1])), ]
#per.sub2$Eff <- per.sub2$WelSurplus / per.sub2$WelMax
#per.sub2$we2 <- (per.sub2$CEchoice - per.sub2$CEuchoice) / per.sub2$CEmax
#
#
#p0 <- ggplot(per.sub2)
#q <- p0 + geom_density(aes(we2), color = "blue", bw = 0.05)
#q <- q + labs(title = "Actual decisions made", x = "Welfare Efficiency (%)", y = "Density")
#
#r <- p0 + geom_density(aes(WelEfficiency2), color = "blue", bw = 0.01)
#r <- r  + labs(title = "Actual decisions made", x = "Welfare Efficiency (%)", y = "Density")
#
#r

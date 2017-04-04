library(dplyr)
library(ggplot2)
library(cowplot)
library(MSL)


plotme <- function(dat, mod, par, sfrac) {
	print(paste("Plotting Winners"))
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# ggplot2 has special requirements when making plots in grids, I haven't decided if I approve,
	# it is definitly RAM hungry. Anything on the y-axis of every subplot needs to be in 1 vairable
	# This makes sense for Y axis, we're plotting unique Y values after all, but it does mean that
	# the X axis variables need to be duplicated for every subplot. This can be seriously data-hungry

	# Anyway, We're going to make a grid of plots split by winning model and significance level
	# So for every model, we need to stack the dataset for each of the 3 significance levels. So 
	# we're stacking the x and y axis 12 times... lots of ram

	# For each model, we need to know if that model was declared a winner for each subject by sig level
	dat$cor <- 1
	dat$sig <- 0
	
	# Make the 3 different sig level duplications
	dat01 <- dat
	dat01$sig <- 1
	dat05 <- dat
	dat05$sig <- 2
	dat10 <- dat
	dat10$sig <- 3
	
	# EUT
	# If the EUT model was declared winner, put 1, otherwise 0
	dat01$cor <- ifelse(dat$win_01 == "EUT", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "EUT", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "EUT", 1, 0)
	
	# Drop crazy EUT estimates, and sample some fraction from the remainder
	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	dat.eut <- dat.eut %>%
		sample_frac(sfrac)
	
	# RDU Power
	dat01$cor <- ifelse(dat$win_01 == "POW", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "POW", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "POW", 1, 0)
	
	# Drop crazy POW estimates, and sample some fraction from the remainder
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	dat.pow <- dat.pow %>%
		sample_frac(sfrac)
	
	# RDU INV
	dat01$cor <- ifelse(dat$win_01 == "INV", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "INV", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "INV", 1, 0)
	
	# Drop crazy INV estimates, and sample some fraction from the remainder
	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	dat.inv <- dat.inv %>%
		sample_frac(sfrac)
	
	# EDU Prelec
	dat01$cor <- ifelse(dat$win_01 == "PRE", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "PRE", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "PRE", 1, 0)
	
	# Drop crazy PRE estimates, and sample some fraction from the remainder
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	dat.pre <- dat.pre %>%
		sample_frac(sfrac)
	
	# Now we need to stack all the model datasets together
	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)
	# Try to do some ram recovery
	rm(dat.eut, dat.pow, dat.inv, dat.pre)
	# Make Fctors
	dat$sig <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))

	# We're putting lines on each subplot. These lines subset the data some more, thus the need for lots
	# of observations
	dat$mugrp <- 0
	dat$mugrp <- ifelse(dat$mu < .1, 1, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .20 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .30 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, labels=c("0.01 < mu < 0.10", "0.10 < mu < 0.20", "0.20 < mu < 0.30"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat <- dat %>%
		select_(par, "cor", "sig", "mod", "mugrp") %>%
		filter(!is.na(cor), !is.na(mod))

	print(paste("Now plotting", mod, "with", nrow(dat), "rows"))
	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + facet_grid(sig~mod)
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = mugrp))
	p <- p + geom_smooth(span = 0.15, aes(color = mugrp))
	p <- p + labs(title = paste(mod, "Subjects, N =",N), x = paste(par, "Value"), y = "Frequency of Winning", color = "MU Group")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

plot.wel <- function(dat, mod, par, sfrac, wel.var) {

	#dbug <- 1
	#print(paste("here:", dbug)) ; dbug <- dbug + 1

	print(paste("Plotting Welfare"))

	wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
	#wel.var <- "Prob"

	dat <- dat %>%
		filter(!is.na(dat$win_01), !is.na(dat$win_05), !is.na(dat$win_10))
	N <- nrow(dat)

	dat$cor <- 1
	dat$sig <- 0

	dat01 <- dat
	dat01$sig <- 1
	dat05 <- dat
	dat05$sig <- 2
	dat10 <- dat
	dat10$sig <- 3

	dat01$cor <- ifelse(dat$win_01 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)

	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	dat.eut <- dat.eut %>%
		sample_frac(sfrac)

	dat01$cor <- ifelse(dat$win_01 == "POW", dat[[paste0("POW_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "POW", dat[[paste0("POW_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "POW", dat[[paste0("POW_", wel.var)]], NA)

	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	dat.pow <- dat.pow %>%
		sample_frac(sfrac)

	dat01$cor <- ifelse(dat$win_01 == "INV", dat[[paste0("INV_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "INV", dat[[paste0("INV_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "INV", dat[[paste0("INV_", wel.var)]], NA)

	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	dat.inv <- dat.inv %>%
		sample_frac(sfrac)

	dat01$cor <- ifelse(dat$win_01 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)

	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	dat.pre <- dat.pre %>%
		sample_frac(sfrac)

	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)
	dat$sig <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))

	dat$mugrp <- 0
	dat$mugrp <- ifelse(dat$mu < .1, 1, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .2 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .3 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, labels=c("0.01 < mu < 0.1", "0.1 < mu < 0.2", "0.2 < mu < 0.3"))

	# Take the difference between the estimate and the real values for the welfare metric
	dat$cor <- dat$cor - dat[[paste0("real_", wel.var)]]
	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.

	dat <- dat %>%
		select_(par, "cor", "sig", "mod", "mugrp") %>%
		filter(!is.na(cor))

	rows <- nrow(dat)

	print(paste("Now plotting", mod, "with", rows, "rows"))
	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + facet_grid(sig~mod, scales = "free_y")
	#p <- p + facet_grid(sig~mod)
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = mugrp))
	p <- p + geom_smooth(span = 0.15, aes(color = mugrp))
	p <- p + labs(title = paste(mod, "Subjects"), x = paste(par, "Value"), y = paste("Estimated", wel.var, "- Real", wel.var), color = "MU Group")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

allmu <- function(dat, sfrac) {
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	dat$cor <- 1
	dat$sig <- 0
	
	dat01 <- dat
	dat01$sig <- 1
	dat05 <- dat
	dat05$sig <- 2
	dat10 <- dat
	dat10$sig <- 3

	dat01$cor <- ifelse(dat$win_01 == "EUT", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "EUT", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "EUT", 1, 0)
	
	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	
	dat01$cor <- ifelse(dat$win_01 == "POW", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "POW", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "POW", 1, 0)
	
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	
	dat01$cor <- ifelse(dat$win_01 == "INV", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "INV", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "INV", 1, 0)
	
	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	
	dat01$cor <- ifelse(dat$win_01 == "PRE", 1, 0)
	dat05$cor <- ifelse(dat$win_05 == "PRE", 1, 0)
	dat10$cor <- ifelse(dat$win_10 == "PRE", 1, 0)
	
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	
	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)
	dat$sig   <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod   <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))
	dat$model <- factor(dat$model, c("EUT", "pow", "invs", "prelec"), labels = c("EUT Sub", "RDU Power Sub", "RDU Inverse-S Sub", "RDU Prelec Sub"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat <- dat %>%
		select_("mu", "cor", "sig", "mod", "model") %>%
		filter(!is.na(cor), !is.na(mod))

	print("Now Plotting All")
	p <- ggplot(dat, aes_string(x = "mu", y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + scale_x_continuous(limits = c(0.01,0.30))
	p <- p + facet_grid(model~mod)
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
	#p <- p + geom_smooth(span = 0.15, aes(color = sig))
	p <- p + geom_smooth(aes(color = sig))
	p <- p + labs(title = paste("All", "Subjects, N =", N), x = paste("Lambda", "Value"), y = "Frequency of Winning", color = "Significance\nValue")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	rm(dat)
	return(p)
}

P_AB_ALL <- function(dat, sfrac) {
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# Given that you have been declared N, whats the likelihood you were M
	# so P(M) on the Y axis, the row facet will be the true model,

	# Probability you are the row model, given you've been declared the column model

	print("here0")

	# Need to make Y = model
	dat$y <- 0
	dat$sig <- 0
	
	dat.eut <- dat %>% filter(win_05 == "EUT")
	dat.eut.eut <- dat.eut
	dat.pow.eut <- dat.eut
	dat.inv.eut <- dat.eut
	dat.pre.eut <- dat.eut

	dat.pow <- dat %>% filter(win_05 == "POW")
	dat.eut.pow <- dat.pow
	dat.pow.pow <- dat.pow
	dat.inv.pow <- dat.pow
	dat.pre.pow <- dat.pow

	dat.inv <- dat %>% filter(win_05 == "INV")
	dat.eut.inv <- dat.inv
	dat.pow.inv <- dat.inv
	dat.inv.inv <- dat.inv
	dat.pre.inv <- dat.inv

	dat.pre <- dat %>% filter(win_05 == "PRE")
	dat.eut.pre <- dat.pre
	dat.pow.pre <- dat.pre
	dat.inv.pre <- dat.pre
	dat.pre.pre <- dat.pre

	print("here1")
	print("here1.1")

	dat.eut.eut$y <- ifelse(dat.eut$model == "EUT", 1, 0)
	dat.pow.eut$y <- ifelse(dat.eut$model == "EUT", 1, 0)
	dat.inv.eut$y <- ifelse(dat.eut$model == "EUT", 1, 0)
	dat.pre.eut$y <- ifelse(dat.eut$model == "EUT", 1, 0)

	dat.eut <- rbind(dat.eut.eut, dat.pow.eut, dat.inv.eut, dat.pre.eut)
	dat.eut$mod <- 1

	print("here2")
	dat.eut.pow$y <- ifelse(dat.pow$model == "pow", 1, 0)
	dat.pow.pow$y <- ifelse(dat.pow$model == "pow", 1, 0)
	dat.inv.pow$y <- ifelse(dat.pow$model == "pow", 1, 0)
	dat.pre.pow$y <- ifelse(dat.pow$model == "pow", 1, 0)

	dat.pow <- rbind(dat.eut.pow, dat.pow.pow, dat.inv.pow, dat.pre.pow)
	dat.pow$mod <- 2

	print("here3")
	dat.eut.inv$y <- ifelse(dat.inv$model == "invs", 1, 0)
	dat.pow.inv$y <- ifelse(dat.inv$model == "invs", 1, 0)
	dat.inv.inv$y <- ifelse(dat.inv$model == "invs", 1, 0)
	dat.pre.inv$y <- ifelse(dat.inv$model == "invs", 1, 0)

	dat.inv <- rbind(dat.eut.inv, dat.pow.inv, dat.inv.inv, dat.pre.inv)
	dat.inv$mod <- 3

	print("here4")
	dat.eut.pre$y <- ifelse(dat.pre$model == "prelec", 1, 0)
	dat.pow.pre$y <- ifelse(dat.pre$model == "prelec", 1, 0)
	dat.inv.pre$y <- ifelse(dat.pre$model == "prelec", 1, 0)
	dat.pre.pre$y <- ifelse(dat.pre$model == "prelec", 1, 0)

	dat.pre <- rbind(dat.eut.pre, dat.pow.pre, dat.inv.pre, dat.pre.pre)
	dat.pre$mod <- 4

	print("here5")
	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)

	dat$sig   <- 0
	#dat$sig   <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod   <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))
	dat$model <- factor(dat$model, c("EUT", "pow", "invs", "prelec"), labels = c("EUT Sub", "RDU Power Sub", "RDU Inverse-S Sub", "RDU Prelec Sub"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	print("here6")
	dat <- dat %>%
		select_("mu", "y", "sig", "mod", "model") %>%
		filter(!is.na(y), !is.na(mod))

	print("Now Plotting All")
	p <- ggplot(dat, aes_string(x = "mu", y = "y"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + scale_x_continuous(limits = c(0.01,0.30))
	p <- p + facet_grid(model~mod)
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
	#p <- p + geom_smooth(span = 0.15, aes(color = sig))
	#p <- p + geom_smooth(aes(color = sig))
	p <- p + geom_smooth(aes())
	p <- p + labs(title = paste("All", "Subjects, N =", N), x = paste("Lambda", "Value"), y = "Frequency of Winning", color = "Significance\nValue")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	rm(dat)
	return(p)
}

all.wel <- function(dat, sfrac, wel.var) {

	dat <- dat %>%
		filter(!is.na(dat$win_01), !is.na(dat$win_05), !is.na(dat$win_10))
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	dat$cor <- 1
	dat$sig <- 0
	
	dat01 <- dat
	dat01$sig <- 1
	dat05 <- dat
	dat05$sig <- 2
	dat10 <- dat
	dat10$sig <- 3
	
	dat01$cor <- ifelse(dat$win_01 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)
	
	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	
	dat01$cor <- ifelse(dat$win_01 == "POW", dat[[paste0("POW_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "POW", dat[[paste0("POW_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "POW", dat[[paste0("POW_", wel.var)]], NA)
	
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2

	dat01$cor <- ifelse(dat$win_01 == "INV", dat[[paste0("INV_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "INV", dat[[paste0("INV_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "INV", dat[[paste0("INV_", wel.var)]], NA)

	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	
	dat01$cor <- ifelse(dat$win_01 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win_05 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win_10 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)
	
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	
	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)
	dat$sig   <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod   <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))
	dat$model <- factor(dat$model, c("EUT", "pow", "invs", "prelec"), labels = c("EUT Sub", "RDU Power Sub", "RDU Inverse-S Sub", "RDU Prelec Sub"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat$cor <- dat$cor - dat[[paste0("real_", wel.var)]]
	dat <- dat %>%
		select_("mu", "cor", "sig", "mod", "model") %>%
		filter(!is.na(cor), !is.na(mod))
	

	print("Now Plotting All")
	p <- ggplot(dat, aes_string(x = "mu", y = "cor"))
#	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + scale_x_continuous(limits = c(0.01,0.35))
	p <- p + facet_grid(model~mod, scales = "free_y")
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
	p <- p + geom_smooth(span = 0.15, aes(color = sig))
	p <- p + labs(title = paste("All", "Subjects, N =", N), x = paste("Lambda", "Value"), y = "Frequency of Winning", color = "Significance\nValue")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

welfare5 <- function(dat, mod, par, sfrac, wel.var, legpos = "none") {

	#dbug <- 1
	#print(paste("here:", dbug)) ; dbug <- dbug + 1

	print(paste("Plotting Welfare for 5%"))

	wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
	#wel.var <- "Prob"

	dat <- dat %>%
		filter(!is.na(dat$win_01), !is.na(dat$win_05), !is.na(dat$win_10))
	N <- nrow(dat)

	dat$cor <- 1

	dat05 <- dat

	dat05$cor <- ifelse(dat$win_05 == "EUT", dat[[paste0("EUT_", wel.var)]], NA)

	dat.eut <- dat05
	dat.eut$mod <- 1
	dat.eut <- dat.eut %>%
		sample_frac(sfrac)

	dat05$cor <- ifelse(dat$win_05 == "POW", dat[[paste0("POW_", wel.var)]], NA)

	dat.pow <- dat05
	dat.pow$mod <- 2
	dat.pow <- dat.pow %>%
		sample_frac(sfrac)

	dat05$cor <- ifelse(dat$win_05 == "INV", dat[[paste0("INV_", wel.var)]], NA)

	dat.inv <- dat05
	dat.inv$mod <- 3
	dat.inv <- dat.inv %>%
		sample_frac(sfrac)

	dat05$cor <- ifelse(dat$win_05 == "PRE", dat[[paste0("PRE_", wel.var)]], NA)

	dat.pre <- dat05
	dat.pre$mod <- 4
	dat.pre <- dat.pre %>%
		sample_frac(sfrac)

	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)
	dat$mod <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))

	dat$mugrp <- 0
	dat$mugrp <- ifelse(dat$mu < .1, 1, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .2 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .3 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, labels=c("0.01 < mu < 0.1", "0.1 < mu < 0.2", "0.2 < mu < 0.3"))

	# Take the difference between the estimate and the real values for the welfare metric
	dat$cor <- dat$cor - dat[[paste0("real_", wel.var)]]
	dat$cor <- abs(dat$cor)
	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.

	dat <- dat %>%
		select_(par, "cor", "mod", "mugrp") %>%
		filter(!is.na(cor))

	rows <- nrow(dat)

	print(paste("Now plotting", mod, "with", rows, "rows"))
	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + facet_grid(mugrp~., scales = "free_y")
	#p <- p + facet_grid(~mugrp, scales = "free_y")
	p <- p + geom_smooth(span = 0.15, aes(color = mod))
	p <- p + labs(title = paste(mod, "Subjects"), x = paste(par, "Value"), y = paste("Absolute Value of Estimated", wel.var, "- Real", wel.var), color = "Classified Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = legpos)
	
	return(p)
}

correct <- function(dat) {

	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	dat$cor <- 0
	dat$sig <- 0
	
	dat01 <- dat
	dat01$sig <- 1
	dat05 <- dat
	dat05$sig <- 2
	dat10 <- dat
	dat10$sig <- 3
	
	dat01$cor <- ifelse(dat$win_01 == 1 & dat$model != "EUT", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win_05 == 1 & dat$model != "EUT", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win_10 == 1 & dat$model != "EUT", 1, dat10$cor)

	dat01$cor <- ifelse(dat$win_01 == 2 & dat$model != "pow", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win_05 == 2 & dat$model != "pow", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win_10 == 2 & dat$model != "pow", 1, dat10$cor)

	dat01$cor <- ifelse(dat$win_01 == 3 & dat$model != "invs", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win_05 == 3 & dat$model != "invs", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win_10 == 3 & dat$model != "invs", 1, dat10$cor)

	dat01$cor <- ifelse(dat$win_01 == 4 & dat$model != "prelec", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win_05 == 4 & dat$model != "prelec", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win_10 == 4 & dat$model != "prelec", 1, dat10$cor)
	
	dat <- rbind(dat01, dat05, dat10)
	
	dat$sig   <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$model <- factor(dat$model, c("EUT", "pow", "invs", "prelec"), labels = c("EUT Sub", "RDU Power Sub", "RDU Inverse-S Sub", "RDU Prelec Sub"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat <- dat %>%
		select_("mu", "cor", "sig", "model") %>%
		filter(!is.na(cor), !is.na(model))
	
	print(paste("Now plotting All"))
	p <- ggplot(dat, aes_string(x = "mu", y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + scale_x_continuous(limits = c(0.01,0.35))
	p <- p + facet_grid(~model)
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
	p <- p + geom_smooth(span = 0.15, aes(color = sig))
	p <- p + labs(title = paste("All", "Subjects, N =", N), x = paste("Lambda", "Value"), y = "Frequency of False Positives", color = "Significance\nValue")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

per_inst <- function(inst) {

print(paste("Plotting for instrument", inst))

# plot config
dev.type <- "pdf"
width   <- 10
height  <- 8.88
width5  <- 4.5
height5 <- 11
units <- "in"
plot_dir <- "../plots/"
# Data config
win_frac <- 1
wel_frac <- 1
all_frac <- 1

wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel.var  <- wel.vars[1]

# Full data plot
p.eut <- F
p.pow <- F
p.inv <- F
p.pre <- T
p.all <- F
p.AB_ALL <- F
p.cor <- F

p.allwell <- T

p.win <- F
p.wel <- F
p.wel5 <- F

# EUT Subjects
if (p.eut) {
	if(p.win) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "EUT") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("win"), r, mu, model)

#		print(paste0("Number of subjects in EUT Win: ",nrow(FDAT)))
		p <- plotme(FDAT, "EUT", "r", sfrac = win_frac)
#		print("saving plot")
		ggsave(paste0("eut-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "EUT") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
						starts_with("EUT"), starts_with("win"), r, mu)

#		print(paste0("Number of subjects in EUT Welfare: ",nrow(FDAT)))
		p <- plot.wel(FDAT, "EUT", "r", sfrac = wel_frac, wel.var)
		ggsave(paste0("eut-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
	}
	if (p.wel5) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "EUT") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
						starts_with("EUT"), starts_with("win"), r, mu)

		p <- welfare5(FDAT, "EUT", "r", sfrac = wel_frac, wel.var)
		ggsave(paste0("eut-welfare5-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width5, height = height5, units = units)

		rm(FDAT, p)

		rm(list=inst)
	}
}

# POW Subjects
if (p.pow) {
	if(p.win) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "pow") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("win"), r, mu, alpha, model)

#		print(paste0("Number of subjects in POW Win: ",nrow(FDAT)))
		p <- plotme(FDAT, "POW", "alpha", win_frac)
#		print("saving plot")
		ggsave(paste0("pow-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "pow") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
			starts_with("POW"), starts_with("win"), r, alpha, beta, mu)

#		print(paste0("Number of subjects in POW Welfare: ",nrow(FDAT)))
		p <- plot.wel(FDAT, "POW", "alpha", wel_frac, wel.var)
#		print("saving plot")
		ggsave(paste0("pow-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel5) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "pow") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("Est"), ends_with(wel.var),
			       starts_with("POW"), starts_with("win"), r, alpha, mu)

		p <- welfare5(FDAT, "POW", "alpha", sfrac = wel_frac, wel.var)
		ggsave(paste0("pow-welfare5-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width5, height = height5, units = units)

		rm(FDAT, p)

		rm(list=inst)
	}
}

# INV Subjects
if (p.inv) {
	if(p.win) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "invs") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			filter(alpha > .3) %>%
			select(starts_with("win"), r, mu, alpha, model)

#		print(paste0("Number of subjects in INV Win: ",nrow(FDAT)))
		p <- plotme(FDAT, "Inverse S", "alpha", win_frac)
#		print("saving plot")
		ggsave(paste0("inv-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "invs") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			filter(alpha > 0.3) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
			       starts_with("INV"), starts_with("win"), r, alpha, beta, mu)

#		print(paste0("Number of subjects in INV Wel: ",nrow(FDAT)))
		p <- plot.wel(FDAT, "Inverse S", "alpha", wel_frac, wel.var)
#		print("saving plot")
		ggsave(paste0("inv-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel5) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "invs") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("Est"), ends_with(wel.var),
			       starts_with("INV"), starts_with("win"), r, alpha, mu)

		p <- welfare5(FDAT, "Inverse S", "alpha", sfrac = wel_frac, wel.var)
		ggsave(paste0("inv-welfare5-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width5, height = height5, units = units)

		rm(FDAT, p)

		rm(list=inst)
	}
}

# PRE Subjects
if (p.pre) {
	if(p.win) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "prelec") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("win"), r, mu, alpha, beta, model)

#		print(paste0("Number of subjects in PRE Win: ",nrow(FDAT)))
		p <- plotme(filter(FDAT, beta > 1.2 | beta < 0.8), "Prelec",    "alpha", win_frac)
#		print("saving plot")
		ggsave(paste0("pre-a-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(p)
		print("Beginning with Prelec winner beta")
		p <- plotme(filter(FDAT, alpha > 1.2 | alpha < 0.8), "Prelec",    "beta", win_frac)
#		print("saving plot")
		ggsave(paste0("pre-b-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "prelec") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
			starts_with("PRE"), starts_with("win"), r, alpha, beta, mu)

#		print(paste0("Number of subjects in PRE Welfare: ",nrow(FDAT)))
		print("Beginning with Prelec welfare alpha")
		p <- plot.wel(filter(FDAT, beta > 1.2 | beta < 0.8), "Prelec",    "alpha", wel_frac, wel.var)
		ggsave(paste0("pre-a-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)

		print("Beginning with Prelec welfare beta")
		p <- plot.wel(filter(FDAT, alpha > 1.2 | alpha < 0.8), "Prelec",    "beta", wel_frac, wel.var)
		ggsave(paste0("pre-b-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)

		rm(FDAT, p)
		rm(list=inst)
	}
	if (p.wel5) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "prelec") %>%
			select(starts_with("real"), ends_with("Est"), ends_with(wel.var),
			starts_with("PRE"), starts_with("win"), r, alpha, beta, mu)

		print("Beginning with Prelec welfare5 alpha")
		#p <- welfare5(filter(FDAT, beta > 1.2 | beta < 0.8), "Prelec", "alpha", sfrac = wel_frac, wel.var)
		p <- welfare5(FDAT, "Prelec", "alpha", sfrac = wel_frac, wel.var)
		ggsave(paste0("pre-a-welfare5-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width5, height = height5, units = units)

		print("Beginning with Prelec welfare5 beta")
		#p <- welfare5(filter(FDAT, alpha > 1.2 | alpha < 0.8), "Prelec", "beta", sfrac = wel_frac, wel.var)
		p <- welfare5(FDAT, "Prelec", "beta", sfrac = wel_frac, wel.var)
		ggsave(paste0("pre-b-welfare5-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width5, height = height5, units = units)

		rm(FDAT, p)
		rm(list=inst)
	}
}

# All Subjects with MU
if (p.all) {
	if(p.win) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			filter((alpha < 2 & alpha > 1.2 | alpha < 0.8) | is.na(alpha),
						(beta < 2 & beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
			select(starts_with("win"), mu, model) %>%
			sample_frac(all_frac)

			#select(starts_with("win"), r, alpha, beta, mu) %>%

		p <- allmu(FDAT, win_frac)
#				print("saving plot")
		ggsave(paste0("mu-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if(p.wel) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			filter((alpha < 2 & alpha > 1.2 | alpha < 0.8) | is.na(alpha),
						(beta < 2 & beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
			sample_frac(all_frac)

		p <- all.wel(FDAT, win_frac, wel.var)
#				print("saving plot")
		ggsave(paste0("mu-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
}

# P_AB
if (p.AB_ALL) {
	if(p.win) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			filter((alpha < 2 & alpha > 1.2 | alpha < 0.8) | is.na(alpha),
						(beta < 2 & beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
			select(starts_with("win"), mu, model) %>%
			sample_frac(all_frac)

			#select(starts_with("win"), r, alpha, beta, mu) %>%

		p <- P_AB_ALL(FDAT, win_frac)
#				print("saving plot")
		ggsave(paste0("P_AB-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
	if(p.wel) {
		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			filter((alpha < 2 & alpha > 1.2 | alpha < 0.8) | is.na(alpha),
						(beta < 2 & beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
			sample_frac(all_frac)

		p <- all.wel(FDAT, win_frac, wel.var)
#				print("saving plot")
		ggsave(paste0("mu-welfare-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
		rm(FDAT, p)
		rm(list=inst)
	}
}

# Correct models
if (p.cor) {
load(paste0("../data/classify/full/", inst, "-bak.Rda"))
height <- height/3
results <- get(inst) %>%
	filter((alpha > 1.2 | alpha < 0.8) | is.na(alpha),
	       (beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
	sample_frac(1)

#print(paste0("Number of subjects in results: ",nrow(results)))

p <- correct(results)
#		print("saving plot")
ggsave(paste0("correct-winners-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)
rm(FDAT, p)
}

# All Welfare
if (p.allwell) {

		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "EUT") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
						starts_with("EUT"), starts_with("win"), r, mu)

		p.eut <- welfare5(FDAT, "EUT", "r", sfrac = wel_frac, wel.var, legpos = "right")

		rm(FDAT)

		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "pow") %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("Est"), ends_with(wel.var),
			       starts_with("POW"), starts_with("win"), r, alpha, mu)

		p.pow <- welfare5(FDAT, "POW", "alpha", sfrac = wel_frac, wel.var)

		rm(FDAT)

		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "invs") %>%
			filter(alpha > 0.3) %>%
			filter(!is.na(win_01), !is.na(win_05), !is.na(win_10)) %>%
			select(starts_with("real"), ends_with("Est"), ends_with(wel.var),
			       starts_with("INV"), starts_with("win"), r, alpha, mu)

		p.inv <- welfare5(FDAT, "Inverse S", "r", sfrac = wel_frac, wel.var)

		rm(FDAT)

		load(paste0("../data/classify/full/", inst, "-bak.Rda"))
		FDAT <- get(inst) %>%
			filter(model == "prelec") %>%
			select(starts_with("real"), ends_with("Est"), ends_with(wel.var),
			starts_with("PRE"), starts_with("win"), r, alpha, beta, mu)

		print("Beginning with Prelec welfare5 alpha")
		#p.prea <- welfare5(filter(FDAT, beta > 1.2 | beta < 0.8), "Prelec", "alpha", sfrac = wel_frac, wel.var)
		p.prea <- welfare5(FDAT, "Prelec", "alpha", sfrac = wel_frac, wel.var)

		print("Beginning with Prelec welfare5 beta")
		#p.preb <- welfare5(filter(FDAT, alpha > 1.2 | alpha < 0.8), "Prelec", "beta", sfrac = wel_frac, wel.var)
		p.preb <- welfare5(FDAT, "Prelec", "beta", sfrac = wel_frac, wel.var)

		rm(FDAT)

		rm(list=inst)

		legend <- get_legend(p.eut)

		# Plot the middle

		theme_set(theme_grey())

		mm <- plot_grid(p.pow + ylab("") + theme(strip.background = element_blank(), strip.text.y = element_blank()), 
		                p.inv + ylab("") + theme(strip.background = element_blank(), strip.text.y = element_blank()),
		                p.prea + ylab("") + theme(strip.background = element_blank(), strip.text.y = element_blank()),
		                ncol = 3)

		# Join the middle and left side with the y axis lable
		mm <- plot_grid(p.eut + theme(legend.position = "none", strip.background = element_blank(), strip.text.y = element_blank()), 
										mm,
										ncol = 2,
										rel_widths = c(1.05, 3)
										)

		# Join with the right side and the facet labels
		mm <- plot_grid(mm,
		                p.preb + ylab(""),
										ncol = 2,
										rel_widths = c(4.1, 1.1))

		# Set the default theme
		#theme_set(theme_grey())

		# Join the actual plots with the legend
		mm <- plot_grid(mm, legend, ncol =2 , rel_widths = c(1.2, .1))

		cowplot::save_plot("../plots/welfare5.pdf", mm, device = "pdf", base_aspect_ratio = 2, base_height = 10)
}

}

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")

for(i in instruments) {
	per_inst(i)
}

print(warnings())

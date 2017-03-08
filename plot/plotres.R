library(dplyr)
library(ggplot2)
#load("../data/classify/merged/HNG.Rda")

plotme <- function(dat, mod, par, sfrac) {
	# How many subjects do we have? To put in Title
	dat <- dat %>%
		filter(!is.na(dat$win.s01), !is.na(dat$win.s05), !is.na(dat$win.s10))
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
	dat01$cor <- ifelse(dat$win.s01 == 1, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 1, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 1, 1, 0)
	
	# Drop crazy EUT estimates, and sample some fraction from the remainder
	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	dat.eut <- dat.eut %>%
		filter(eut.r.Est < 10, eut.r.Est > -10) %>%
		sample_frac(sfrac)
	
	# RDU Power
	dat01$cor <- ifelse(dat$win.s01 == 2, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 2, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 2, 1, 0)
	
	# Drop crazy POW estimates, and sample some fraction from the remainder
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	dat.pow <- dat.pow %>%
		filter(pow.r.Est < 10, pow.r.Est > -10,
		       pow.alpha.Est < 10, pow.alpha.Est > .05) %>%
		sample_frac(sfrac)
	
	# RDU INV
	dat01$cor <- ifelse(dat$win.s01 == 3, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 3, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 3, 1, 0)
	
	# Drop crazy INV estimates, and sample some fraction from the remainder
	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	dat.inv <- dat.inv %>%
		filter(inv.r.Est < 10, inv.r.Est > -10,
		       inv.alpha.Est < 10, inv.alpha.Est > .05) %>%
		sample_frac(sfrac)
	
	# EDU Prelec
	dat01$cor <- ifelse(dat$win.s01 == 4, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 4, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 4, 1, 0)
	
	# Drop crazy PRE estimates, and sample some fraction from the remainder
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	dat.pre <- dat.pre %>%
		filter(pre.r.Est < 10, pre.r.Est > -10,
		       pre.alpha.Est < 10, pre.alpha.Est > .05,
		       pre.beta.Est  < 10, pre.beta.Est  > .05) %>%
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
	dat$mugrp <- ifelse(dat$mu < .23 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .35 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, labels=c("0.01 < mu < 0.10", "0.10 < mu < 0.23", "0.23 < mu < 0.35"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat <- dat %>%
		select_(par, "cor", "sig", "mod", "mugrp") %>%
		filter(!is.na(cor), !is.na(mod))

	print(paste("Now plotting", mod, "with", nrow(dat), "rows"))
	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + facet_grid(sig~mod)
	p <- p + geom_smooth(method="loess", span = 0.15, aes(color = mugrp))
	#p <- p + geom_smooth(span = 0.15, aes(color = mugrp))
	p <- p + labs(title = paste(mod, "Subjects, N =",N), x = paste(par, "Value"), y = "Frequency of Winning", color = "MU Group")
	p <- p + theme(plot.title = element_text(hjust = 0.5))
	
	return(p)
}

plot.wel <- function(dat, mod, par, sfrac, wel.var) {

	wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
	#wel.var <- "Prob"

	dat <- dat %>%
		filter(!is.na(dat$win.s01), !is.na(dat$win.s05), !is.na(dat$win.s10))
	N <- nrow(dat)

	dat$cor <- 1
	dat$sig <- 0

	dat01 <- dat
	dat01$sig <- 1
	dat05 <- dat
	dat05$sig <- 2
	dat10 <- dat
	dat10$sig <- 3

	dat01$cor <- ifelse(dat$win.s01 == 1, dat[[paste0("eut.", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win.s05 == 1, dat[[paste0("eut.", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win.s10 == 1, dat[[paste0("eut.", wel.var)]], NA)

	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	dat.eut <- dat.eut %>%
		filter(eut.r.Est < 10, eut.r.Est > -10) %>%
		sample_frac(sfrac)

	dat01$cor <- ifelse(dat$win.s01 == 2, dat[[paste0("pow.", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win.s05 == 2, dat[[paste0("pow.", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win.s10 == 2, dat[[paste0("pow.", wel.var)]], NA)

	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	dat.pow <- dat.pow %>%
		filter(pow.r.Est < 10, pow.r.Est > -10,
		       pow.alpha.Est < 10, pow.alpha.Est > .05) %>%
		sample_frac(sfrac)

	dat01$cor <- ifelse(dat$win.s01 == 3, dat[[paste0("inv.", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win.s05 == 3, dat[[paste0("inv.", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win.s10 == 3, dat[[paste0("inv.", wel.var)]], NA)

	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	dat.inv <- dat.inv %>%
		filter(inv.r.Est < 10, inv.r.Est > -10,
		       inv.alpha.Est < 10, inv.alpha.Est > .05) %>%
		sample_frac(sfrac)

	dat01$cor <- ifelse(dat$win.s01 == 4, dat[[paste0("PRE.", wel.var)]], NA)
	dat05$cor <- ifelse(dat$win.s05 == 4, dat[[paste0("PRE.", wel.var)]], NA)
	dat10$cor <- ifelse(dat$win.s10 == 4, dat[[paste0("PRE.", wel.var)]], NA)

	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	dat.pre <- dat.pre %>%
		filter(pre.r.Est < 10, pre.r.Est > -10,
		       pre.alpha.Est < 10, pre.alpha.Est > .05,
		       pre.beta.Est  < 10, pre.beta.Est  > .05) %>%
		sample_frac(sfrac)

	dat <- rbind(dat.eut, dat.pow, dat.inv, dat.pre)
	dat$sig <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))

	dat$mugrp <- 0
	dat$mugrp <- ifelse(dat$mu < .08, 1, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .17 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .35 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, labels=c("0.01 < mu < 0.08", "0.08 < mu < 0.17", "0.17 < mu < 0.35"))

	# Take the difference between the estimate and the real values for the welfare metric
	dat$cor <- dat$cor - dat[[paste0("real.", wel.var)]]
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
	p <- p + theme(plot.title = element_text(hjust = 0.5))
	
	return(p)
}

allmu <- function(dat, sfrac) {

	dat <- dat %>%
		filter(!is.na(dat$win.s01), !is.na(dat$win.s05), !is.na(dat$win.s10))
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
	
	dat01$cor <- ifelse(dat$win.s01 == 1, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 1, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 1, 1, 0)
	
	dat.eut <- rbind(dat01, dat05, dat10)
	dat.eut$mod <- 1
	dat.eut <- dat.eut %>%
		filter(eut.r.Est < 10, eut.r.Est > -10)
	
	dat01$cor <- ifelse(dat$win.s01 == 2, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 2, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 2, 1, 0)
	
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	dat.pow <- dat.pow %>%
		filter(pow.r.Est < 10, pow.r.Est > -10,
		       pow.alpha.Est < 10, pow.alpha.Est > .05)
	
	dat01$cor <- ifelse(dat$win.s01 == 3, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 3, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 3, 1, 0)
	
	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	dat.inv <- dat.inv %>%
		filter(inv.r.Est < 10, inv.r.Est > -10,
		       inv.alpha.Est < 10, inv.alpha.Est > .05)
	
	dat01$cor <- ifelse(dat$win.s01 == 4, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 4, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 4, 1, 0)
	
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	dat.pre <- dat.pre %>%
		filter(pre.r.Est < 10, pre.r.Est > -10,
		       pre.alpha.Est < 10, pre.alpha.Est > .05,
		       pre.beta.Est  < 10, pre.beta.Est  > .05)
	
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
	p <- p + scale_x_continuous(limits = c(0.01,0.35))
	p <- p + facet_grid(model~mod)
	#p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
	p <- p + geom_smooth(span = 0.15, aes(color = sig))
	p <- p + labs(title = paste("All", "Subjects, N =", N), x = paste("Mu", "Value"), y = "Frequency of Winning", color = "Significance\nValue")
	p <- p + theme(plot.title = element_text(hjust = 0.5))
	
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
	
	dat01$cor <- ifelse(dat$win.s01 == 1 & dat$model != "EUT", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win.s05 == 1 & dat$model != "EUT", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win.s10 == 1 & dat$model != "EUT", 1, dat10$cor)

	dat01$cor <- ifelse(dat$win.s01 == 2 & dat$model != "pow", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win.s05 == 2 & dat$model != "pow", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win.s10 == 2 & dat$model != "pow", 1, dat10$cor)

	dat01$cor <- ifelse(dat$win.s01 == 3 & dat$model != "invs", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win.s05 == 3 & dat$model != "invs", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win.s10 == 3 & dat$model != "invs", 1, dat10$cor)

	dat01$cor <- ifelse(dat$win.s01 == 4 & dat$model != "prelec", 1, dat01$cor)
	dat05$cor <- ifelse(dat$win.s05 == 4 & dat$model != "prelec", 1, dat05$cor)
	dat10$cor <- ifelse(dat$win.s10 == 4 & dat$model != "prelec", 1, dat10$cor)
	
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
	p <- p + labs(title = paste("All", "Subjects, N =", N), x = paste("Mu", "Value"), y = "Frequency of False Positives", color = "Significance\nValue")
	p <- p + theme(plot.title = element_text(hjust = 0.5))
	
	return(p)
}

# plot config
dev.type <- "pdf"
width <- 12
height <- 8
units <- "in"
path <- "plots"
# Data config
win_frac <- .25
wel_frac <- 1
all_frac <- .4

wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel.var <- wel.vars[2]

# Full data plot
p.eut <- F
p.pow <- F
p.inv <- F
p.pre <- F
p.all <- F
p.cor <- T

p.win <- T
p.wel <- T

# EUT Subjects
if (p.eut) {
	if(p.win) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "EUT") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"),
			       starts_with("eut"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in EUT Win: ",nrow(HNG)))
		p <- plotme(HNG, "EUT", "r", sfrac = win_frac)
		print("saving plot")
		ggsave(paste0("../plots/eut-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
	if (p.wel) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "EUT") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
						starts_with("eut"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in EUT Welfare: ",nrow(HNG)))
		p <- plot.wel(HNG, "EUT", "r", sfrac = wel_frac, wel.var)
		print("saving plot")
		ggsave(paste0("../plots/eut-welfare.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
}

# POW Subjects
if (p.pow) {
	if(p.win) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "pow") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"),
			starts_with("pow"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in POW Win: ",nrow(HNG)))
		p <- plotme(HNG, "POW", "alpha", win_frac)
		print("saving plot")
		ggsave(paste0("../plots/pow-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
	if (p.wel) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "pow") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
			starts_with("pow"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in POW Welfare: ",nrow(HNG)))
		p <- plot.wel(HNG, "POW", "alpha", wel_frac, wel.var)
		print("saving plot")
		ggsave(paste0("../plots/pow-welfare.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
}

# INV Subjects
if (p.inv) {
	if(p.win) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "invs") %>%
			filter(alpha > .2) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"),
			       starts_with("inv"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in INV Win: ",nrow(HNG)))
		p <- plotme(HNG, "Inverse S", "alpha", win_frac)
		print("saving plot")
		ggsave(paste0("../plots/inv-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
	if (p.wel) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "invs") %>%
			filter(alpha > 0.4) %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
			       starts_with("inv"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in INV Wel: ",nrow(HNG)))
		p <- plot.wel(HNG, "Inverse S", "alpha", wel_frac, wel.var)
		print("saving plot")
		ggsave(paste0("../plots/inv-welfare.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
}

# PRE Subjects
if (p.pre) {
	if(p.win) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "prelec") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"),
			starts_with("pre"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in PRE Win: ",nrow(HNG)))
		p <- plotme(HNG, "Prelec",    "alpha", win_frac)
		print("saving plot")
		ggsave(paste0("../plots/pre-a-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(p)
		print("Beginning with Prelec winner beta")
		p <- plotme(HNG, "Prelec",    "beta", win_frac)
		print("saving plot")
		ggsave(paste0("../plots/pre-b-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
	if (p.wel) {
		load("../data/classify/merged/HNG.Rda")
		HNG <- HNG %>%
			filter(model == "prelec") %>%
			select(starts_with("real"), ends_with("CEdiff"), ends_with("Est"), ends_with(wel.var),
			starts_with("pre"), starts_with("win"), r, alpha, beta, mu)

		print(paste0("Number of subjects in PRE Welfare: ",nrow(HNG)))
		print("Beginning with Prelec welfare alpha")
		p <- plot.wel(HNG, "Prelec",    "alpha", wel_frac, wel.var)
		print("saving plot")
		ggsave(paste0("../plots/pre-a-welfare.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(p)
		print("Beginning with Prelec welfare beta")
		p <- plot.wel(HNG, "Prelec",    "beta", wel_frac, wel.var)
		print("saving plot")
		ggsave(paste0("../plots/pre-b-welfare.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
		rm(HNG, p)
	}
}

# All Subjects with MU
if (p.all) {
load("../data/classify/merged/HNG.Rda")
HNG <- HNG %>%
	filter((alpha < 2 & alpha > 1.2 | alpha < 0.8) | is.na(alpha),
	       (beta < 2 & beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
	sample_frac(all_frac)

print(paste0("Number of subjects in results: ",nrow(HNG)))

p <- allmu(HNG, win_frac)
		print("saving plot")
ggsave(paste0("../plots/mu-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
rm(HNG, p)
}

# Correct models
if (p.cor) {
load("../data/classify/merged/HNG.Rda")
height <- height/3
results <- HNG %>%
	filter((alpha > 1.2 | alpha < 0.8) | is.na(alpha),
	       (beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
	sample_frac(1)

print(paste0("Number of subjects in results: ",nrow(results)))

p <- correct(results)
		print("saving plot")
ggsave(paste0("../plots/correct-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
rm(HNG, p)
}

library(dplyr)
library(ggplot2)
load("results.Rda")

plotme <- function(dat, mod, par) {
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
	
	dat.dat <- rbind(dat01, dat05, dat10)
	dat.dat$mod <- 1
	
	dat01$cor <- ifelse(dat$win.s01 == 2, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 2, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 2, 1, 0)
	
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	
	dat01$cor <- ifelse(dat$win.s01 == 3, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 3, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 3, 1, 0)
	
	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	
	dat01$cor <- ifelse(dat$win.s01 == 4, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 4, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 4, 1, 0)
	
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	
	dat <- rbind(dat.dat, dat.pow, dat.inv, dat.pre)
	dat$sig <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))


	dat$mugrp <- 0
	dat$mugrp <- ifelse(dat$mu < .1, 1, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .23 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .35 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, labels=c("0.01 < mu < 0.10", "0.10 < mu < 0.23", "0.23 < mu < 0.35"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat <- dat %>%
		select_(par, "cor", "sig", "mod", "mugrp")
	
	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	#p <- p + geom_point(alpha = 0.05)
	#p <- p + facet_grid(mod~sig, scales = "free_y")
	#p <- p + facet_grid(mod~sig)
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + facet_grid(sig~mod)
	p <- p + geom_smooth(method="loess", span = 0.15, aes(color = mugrp))
	p <- p + labs(title = paste(mod, "Subjects, N =",N), x = paste(par, "Value"), y = "Frequency of Winning", color = "MU Group")
	p <- p + theme(plot.title = element_text(hjust = 0.5))
	
	return(p)
}

allmu <- function(dat) {

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
	
	dat.dat <- rbind(dat01, dat05, dat10)
	dat.dat$mod <- 1
	
	dat01$cor <- ifelse(dat$win.s01 == 2, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 2, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 2, 1, 0)
	
	dat.pow <- rbind(dat01, dat05, dat10)
	dat.pow$mod <- 2
	
	dat01$cor <- ifelse(dat$win.s01 == 3, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 3, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 3, 1, 0)
	
	dat.inv <- rbind(dat01, dat05, dat10)
	dat.inv$mod <- 3
	
	dat01$cor <- ifelse(dat$win.s01 == 4, 1, 0)
	dat05$cor <- ifelse(dat$win.s05 == 4, 1, 0)
	dat10$cor <- ifelse(dat$win.s10 == 4, 1, 0)
	
	dat.pre <- rbind(dat01, dat05, dat10)
	dat.pre$mod <- 4
	
	dat <- rbind(dat.dat, dat.pow, dat.inv, dat.pre)
	dat$sig   <- factor(dat$sig, 1:3, labels = c("s01", "s05", "s10"))
	dat$mod   <- factor(dat$mod, 1:4, labels = c("EUT", "RDU Power", "RDU Inverse-S", "RDU Prelec"))
	dat$model <- factor(dat$model, c("EUT", "pow", "invs", "prelec"), labels = c("EUT Sub", "RDU Power Sub", "RDU Inverse-S Sub", "RDU Prelec Sub"))

	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.
	dat <- dat %>%
		select_("mu", "cor", "sig", "mod", "model")
	
	p <- ggplot(dat, aes_string(x = "mu", y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + scale_x_continuous(limits = c(0.01,0.35))
	p <- p + facet_grid(model~mod)
	p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
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
		select_("mu", "cor", "sig", "model")
	
	p <- ggplot(dat, aes_string(x = "mu", y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + scale_x_continuous(limits = c(0.01,0.35))
	p <- p + facet_grid(~model)
	p <- p + geom_smooth(method="loess", span = 0.15, aes(color = sig))
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
s_frac <- 1

# Full data plot
p.eut <- T
p.pow <- T
p.inv <- T
p.pre <- T
p.all <- T
p.cor <- T

print(paste0("Full number of subjects in results: ",nrow(results)))

# Get rid of "crazy" estimates
# None of the real values approach these levels
results <- results %>%
	filter(eut.r.Est < 10, eut.r.Est > -10,
	       pow.r.Est < 10, pow.r.Est > -10,
	       inv.r.Est < 10, inv.r.Est > -10,
	       pre.r.Est < 10, pre.r.Est > -10,
	       pow.alpha.Est < 10, pow.alpha.Est > .05,
	       inv.alpha.Est < 10, inv.alpha.Est > .05,
	       pre.alpha.Est < 10, pre.alpha.Est > .05,
	       pre.beta.Est  < 10, pre.beta.Est  > .05)

print(paste0("Number of subjects in results with non-crazy estimates: ",nrow(results)))

# EUT Subjects
if (p.eut) {
eut <- results %>%
	filter(model == "EUT") %>%
	select(starts_with("eut"), starts_with("win"), r, alpha, beta, mu) %>%
	sample_frac(s_frac)

print(paste0("Number of subjects in eut: ",nrow(eut)))

p <- plotme(eut, "EUT", "r")
ggsave(paste0("eut-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)

rm(eut, p)
}

# POW Subjects
if (p.pow) {
pow <- results %>%
	filter(model == "pow") %>%
	select(starts_with("pow"), starts_with("win"), r, alpha, beta, mu) %>%
	sample_frac(s_frac)

print(paste0("Number of subjects in pow: ",nrow(pow)))

p <- plotme(pow, "Power",     "alpha")
ggsave(paste0("pow-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)

rm(pow, p)
}

# INV Subjects
if (p.inv) {
inv <- results %>%
	filter(model == "invs") %>%
	select(starts_with("inv"), starts_with("win"), r, alpha, beta, mu) %>%
	sample_frac(s_frac)

print(paste0("Number of subjects in inv: ",nrow(inv)))

p <- plotme(inv, "Inverse S", "alpha")
ggsave(paste0("inv-winners.", dev.type), path = path, plot = p, device = dev.type, width = width, height = height, units = units)

rm(inv, p)
}

# PRE Subjects
if (p.pre) {
pre <- results %>%
	filter(model == "prelec") %>%
	select(starts_with("pre"), starts_with("win"), r, alpha, beta, mu) %>%
	sample_frac(s_frac)

print(paste0("Number of subjects in pre: ",nrow(pre)))

p <- plotme(pre, "Prelec",    "alpha")
ggsave(paste0("pre-a-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)

rm(p)

p <- plotme(pre, "Prelec",    "beta")
ggsave(paste0("pre-b-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)

rm(pre, p)
}

# All Subjects with MU
if (p.all) {
results <- results %>%
	filter((alpha > 1.2 | alpha < 0.8) | is.na(alpha),
	       (beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
	sample_frac(s_frac)

print(paste0("Number of subjects in results: ",nrow(results)))

p <- allmu(results)
ggsave(paste0("mu-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
}

# Correct models
if (p.cor) {
height <- height/3
results <- results %>%
	filter((alpha > 1.2 | alpha < 0.8) | is.na(alpha),
	       (beta > 1.2 | beta < 0.8) | is.na(beta) ) %>%
	sample_frac(s_frac)

print(paste0("Number of subjects in results: ",nrow(results)))

p <- correct(results)
ggsave(paste0("correct-winners.", dev.type), plot = p, device = dev.type, path = path, width = width, height = height, units = units)
}

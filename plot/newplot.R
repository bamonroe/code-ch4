library(dplyr)
library(ggplot2)
library(cowplot)
library(MSL)

theme_set(theme_grey())

plot_win <- function(dat, par, win_var) {

#	dbug <- 0
#	print(paste0("here", dbug)) ; dbug <- dbug + 1

	print(paste("Plotting Winners"))
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# For each model, we need to know if that model was declared a winner for each subject by sig level
	dat[[win_var]] <- ifelse(is.na(dat[[win_var]]), "NA", dat[[win_var]])

	dat$cor     <- 0
	dat$win     <- 0
	dat$conv    <- 0

	dat.eut     <- dat
	dat.eut$cor <- ifelse(dat[[win_var]] == "EUT", 1, 0)
	dat.eut$win <- 1

	dat.pre     <- dat
	dat.pre$cor <- ifelse(dat[[win_var]] == "PRE", 1, 0)
	dat.pre$win <- 2

	dat.na      <- dat
	dat.na$cor  <- ifelse(dat[[win_var]] == "NA", 1, 0)
	dat.na$win  <- 3

	dat.conv <- rbind(dat.eut, dat.pre) %>%
		filter_(win_var != "NA")
	dat.conv$conv <- 1

	dat <- rbind(dat.eut, dat.pre, dat.na)
	dat$conv <- 2

	dat <- rbind(dat, dat.conv)

	dat[which(dat$model == "EUT"),    "model"] <- 1
	dat[which(dat$model == "prelec"), "model"] <- 2

	dat$conv  <- factor(dat$conv,  levels = 1:2, labels = c("Converged Only", "All Data"))
	dat$win   <- factor(dat$win,   levels = 1:3, labels = c("EUT", "RDU Prelec", "NA"))
	dat$model <- factor(dat$model, levels = 1:2, labels = c("EUT", "RDU Prelec"))

	dat <- dat %>%
		select_(par, "model", "cor", "win", "conv")

	print(paste("Now plotting with", nrow(dat), "rows"))

	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + geom_smooth(span = 0.15, aes(color = win))
	p <- p + facet_grid(conv~model)
	p <- p + labs(title = paste("Subjects, N =", N), x = paste(par, "Value"), y = "Frequency of Winning", color = "Winning Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

	print(p)

	return(p)
}

plot_wel <- function(dat, par, wel_var, win_var) {
	#dbug <- 1
	#print(paste("here:", dbug)) ; dbug <- dbug + 1
	print(paste("Plotting Welfare for 5%"))

	dat$cor <- 1
	dat05 <- dat

	dat05$cor   <- ifelse(dat[[win_var]] == "EUT", dat[[paste0("EUT_", wel_var)]], NA)
	dat.eut     <- dat05
	dat.eut$win <- 1

	dat05$cor   <- ifelse(dat[[win_var]] == "PRE", dat[[paste0("PRE_", wel_var)]], NA)
	dat.pre     <- dat05
	dat.pre$win <- 2

	dat <- rbind(dat.eut, dat.pre)
	dat[which(dat$model == "EUT"),    "model"] <- 1
	dat[which(dat$model == "prelec"), "model"] <- 2

	dat$win   <- factor(dat$win, c(1,2), labels = c("EUT", "RDU Prelec"))
	dat$model <- factor(dat$model, levels = 1:2, labels = c("EUT", "RDU Prelec"))

	# take the difference between the estimate and the real values for the welfare metric
	dat$cor <- dat$cor - dat[[paste0("real_", wel_var)]]

	dat <- dat %>%
#		filter(mu < 0.20) %>%
		select_(par, "model", "cor", "win")

	rows <- nrow(dat)

	print(paste("Now plotting welfare with", rows, "rows"))

	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + facet_grid(~model)
#	p <- p + geom_point(aes(color = mod), alpha = 0.1)
	p <- p + geom_smooth(span = 0.15, aes(color = win))
	p <- p + coord_cartesian(ylim = c(-80,2.5))
	p <- p + labs(title = paste("All Subjects"), x = paste(par, "Value"), y = paste("Absolute Value of Estimated", wel_var, "- Real", wel_var), color = "Classified Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

plot_win_ind <- function(dat, par, mod, win_var) {

#	dbug <- 0
#	print(paste0("here", dbug)) ; dbug <- dbug + 1

	print(paste("Plotting Winners"))
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# For each model, we need to know if that model was declared a winner for each subject by sig level
	dat[[win_var]] <- ifelse(is.na(dat[[win_var]]), "NA", dat[[win_var]])

	dat$cor     <- 0
	dat$win     <- 0
	dat$conv    <- 0

	dat.eut     <- dat
	dat.eut$cor <- ifelse(dat[[win_var]] == "EUT", 1, 0)
	dat.eut$win <- 1

	dat.pre     <- dat
	dat.pre$cor <- ifelse(dat[[win_var]] == "PRE", 1, 0)
	dat.pre$win <- 2

	dat.na      <- dat
	dat.na$cor  <- ifelse(dat[[win_var]] == "NA", 1, 0)
	dat.na$win  <- 3

	dat.conv <- rbind(dat.eut, dat.pre) %>%
		filter_(win_var != "NA")
	dat.conv$conv <- 1

	dat <- rbind(dat.eut, dat.pre, dat.na)
	dat$conv <- 2

	dat <- rbind(dat, dat.conv)

	# duplicate
	row0 <- nrow(dat)
	dat0 <- dat
	dat  <- dat[rep(seq_len(row0), length(par)), ]
	dat$par <- NA
	dat$par_grp <- NA
	for (i in 1:length(par)) {
		start <- (i-1)*row0 + 1
		end   <- i*row0
		dat$par[start:end]     <- dat0[[par[i]]]
		dat$par_grp[start:end] <- i
	}

	# Drop some things
	rm(dat0, dat.eut, dat.pre, dat.conv)

	# Make things factored
	dat$conv    <- factor(dat$conv,    levels = 1:2, labels = c("Converged Only", "All Data"))
	dat$win     <- factor(dat$win,     levels = 1:3, labels = c("EUT", "RDU Prelec", "NA"))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par), labels = par)

#	dat %>% summary %>% print

	dat <- dat %>%
		select_("par", "par_grp", "model", "cor", "win", "conv")

	print(paste("Now plotting with", nrow(dat), "rows"))

	p <- ggplot(dat, aes_string(x = "par", y = "cor"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + geom_smooth(span = 0.15, aes(color = win))
	p <- p + facet_grid(conv~par_grp, scales = "free_x")
	p <- p + labs(title = paste(mod, "Subjects, N =", N), x = paste("Parameter Value"), y = "Frequency of Winning", color = "Winning Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

	print(p)

	return(p)
}

plot_wel_ind <- function(dat, par, wel_var, mod, win_var) {

#	dbug <- 0
#	print(paste0("here", dbug)) ; dbug <- dbug + 1

	print(paste("Plotting Welfare for", mod))
	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# For each model, we need to know if that model was declared a winner for each subject by sig level
	dat[[win_var]] <- ifelse(is.na(dat[[win_var]]), "NA", dat[[win_var]])

	dat$cor     <- 0
	dat$win     <- 0

	dat.eut     <- dat
	dat.eut$cor <- ifelse(dat[[win_var]] == "EUT", dat[[paste0("EUT_", wel_var)]], NA)
	dat.eut$win <- 1

	dat.pre     <- dat
	dat.pre$cor <- ifelse(dat[[win_var]] == "PRE", dat[[paste0("PRE_", wel_var)]], NA)
	dat.pre$win <- 2

	dat <- rbind(dat.eut, dat.pre)

	# take the difference between the estimate and the real values for the welfare metric
	dat$cor <- dat$cor - dat[[paste0("real_", wel_var)]]

	# duplicate
	row0 <- nrow(dat)
	dat0 <- dat
	dat  <- dat[rep(seq_len(row0), length(par)), ]
	dat$par <- NA
	dat$par_grp <- NA
	for (i in 1:length(par)) {
		start <- (i-1)*row0 + 1
		end   <- i*row0
		dat$par[start:end]     <- dat0[[par[i]]]
		dat$par_grp[start:end] <- i
	}

	# Drop some things
	rm(dat0, dat.eut, dat.pre)

	# Make things factored
	dat$win     <- factor(dat$win,     levels = 1:3, labels = c("EUT", "RDU Prelec", "NA"))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par), labels = par)

#	dat %>% summary %>% print

	dat <- dat %>%
		select_("par", "par_grp", "model", "cor", "win")

	print(paste("Now plotting with", nrow(dat), "rows"))

	p <- ggplot(dat, aes_string(x = "par", y = "cor"))
	p <- p + geom_smooth(span = 0.15, aes(color = win))
	p <- p + facet_grid(~par_grp, scales = "free_x")
	p <- p + coord_cartesian(ylim = c(-80,2.5))
	p <- p + labs(title = paste(mod, "Subjects, N =", N), x = paste("Parameter Value"), y = "Frequency of Winning", color = "Winning Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

	print(p)

	return(p)
}

welfare5 <- function(dat, mod, par, wel_var, win_var, legpos = "none", yaxis = F) {

	#dbug <- 1
	#print(paste("here:", dbug)) ; dbug <- dbug + 1

	print(paste("Plotting Welfare for 5%"))

	N <- nrow(dat)

	dat$cor <- 1

	dat05 <- dat

	dat05$cor <- ifelse(dat[[win_var]] == "EUT", dat[[paste0("EUT_", wel_var)]], NA)

	dat.eut <- dat05
	dat.eut$mod <- 1

	dat05$cor <- ifelse(dat[[win_var]] == "PRE", dat[[paste0("PRE_", wel_var)]], NA)

	dat.pre <- dat05
	dat.pre$mod <- 2

	dat <- rbind(dat.eut, dat.pre)
	dat$mod <- factor(dat$mod, c(1, 2), labels = c("EUT", "RDU Prelec"))

	# Take the difference between the estimate and the real values for the welfare metric
	dat$cor <- dat$cor - dat[[paste0("real_", wel_var)]]
	#dat$cor <- abs(dat$cor)
	# Reduce the final datafram as much as possible, its duplicated once and the entire dataframe is saved
	# in the plot. This can really chow RAM, so only keep the variables needed for the plot.

	dat$mugrp <- 0
	dat$mugrp <- ifelse(dat$mu < .1, 1, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .2 & dat$mugrp ==0, 2, dat$mugrp)
	dat$mugrp <- ifelse(dat$mu < .3 & dat$mugrp ==0, 3, dat$mugrp)
	dat$mugrp <- factor(dat$mugrp, levels=1:3, 
											labels=c(latex2exp::TeX("$0.01\\, <\\, \\lambda\\, <\\, 0.1$", output = "text"),
															 latex2exp::TeX("$0.1\\, <\\, \\lambda\\, <\\, 0.2$",  output = "text"),
															 latex2exp::TeX("$0.2\\, <\\, \\lambda\\, <\\, 0.3$",  output = "text")))

	dat <- dat %>%
		select_(par, "cor", "mod", "mugrp") %>%
		filter(!is.na(cor))

	rows <- nrow(dat)

	p <- ggplot(dat, aes_string(x = par, y = "cor"))
	p <- p + facet_grid(mugrp~., labeller = label_parsed)
#	p <- p + geom_point(aes(color = mod), alpha = 0.1)
	p <- p + geom_smooth(span = 0.15, aes(color = mod))
	p <- p + coord_cartesian(ylim = c(-80,2.5))
	p <- p + labs(title = paste(mod, "Subjects"), x = paste(par, "Value"), y = paste("Absolute Value of Estimated", wel_var, "- Real", wel_var), color = "Classified Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = legpos)
	
	return(p)
}

per_inst <- function(inst) {

print(paste("Plotting for instrument", inst))

# plot config
dev.type <- "pdf"
width   <- 10
height  <- 4.44
units   <- "in"
plot_dir <- "../plots/"
# Data config
win_frac <- 1
wel_frac <- 1
all_frac <- 1

wel_vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel_var  <- wel_vars[1]

# Full data plot
do_win_all <- T
do_win_ind <- T

do_wel_all <- T
do_wel_ind <- T

do_ep_wel <- T

# Get the Data
load(paste0("../data/classify/full/", inst, "-bak.Rda"))
FDAT <- get(inst)
rm(list = inst)

win_vars <- c("win_05", "default")

for (win_var in win_vars) {

	print(paste("win_var is", win_var))

if (do_win_all) {
	RDAT <- FDAT %>%
		select_(win_var, "r", "mu", "model") %>%
		filter(model == "EUT" | model == "prelec")

	p <- plot_win(RDAT , "mu", win_var)

	ggsave(paste0(win_var, "-mu-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height * 2, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_wel_all) {
	RDAT <- FDAT %>%
		select_(win_var, "r", "mu", "model", ~starts_with("real"), ~ends_with(wel_var)) %>%
		filter(model == "EUT" | model == "prelec") %>%
		filter_(!is.na(win_var), "r" > 0)

	p <- plot_wel(RDAT, "mu", wel_var, win_var)

	ggsave(paste0(win_var, "-mu-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_win_ind) {
	RDAT <- FDAT %>%
		select_(win_var, "r", "mu", "alpha", "beta", "model")

	mod <- "Prelec"
	p <- plot_win_ind(RDAT %>% filter(model == "prelec"), c("alpha", "beta"), mod = mod, win_var = win_var)
	ggsave(paste0(win_var, "-", mod, "-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = 2*height, units = units)

	mod <- "EUT"
	p <- plot_win_ind(RDAT %>% filter(model == "EUT"), c("r", "mu"), mod = mod, win_var = win_var)
	ggsave(paste0(win_var, "-", mod, "-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = 2*height, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_wel_ind) {
	RDAT <- FDAT %>%
		select_(win_var, "r", "mu", "alpha", "beta", "model", ~starts_with(paste0("real_", wel_var)), ~ends_with(wel_var)) %>%
		filter_(!is.na(win_var))

	mod <- "Prelec"
	p <- plot_wel_ind(RDAT %>% filter(model == "prelec"), c("alpha", "beta"), wel_var = wel_var, mod = mod, win_var = win_var)
	ggsave(paste0(win_var, "-", mod, "-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)

	mod <- "EUT"
	p <- plot_wel_ind(RDAT %>% filter(model == "EUT"), c("r", "mu"), wel_var = wel_var, mod = mod, win_var = win_var)
	ggsave(paste0(win_var, "-", mod, "-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_wel_all) {
	RDAT <- FDAT %>%
		select_(win_var, "r", "mu", "model", ~starts_with("real"), ~ends_with(wel_var)) %>%
		filter(model == "EUT" | model == "prelec") %>%
		filter_(!is.na(win_var), "r" > 0)

	p <- plot_wel(RDAT , "mu", wel_var, win_var)

	ggsave(paste0(win_var, "-mu-", inst, ".", dev.type), plot = p, device = dev.type, path = plot_dir, width = width, height = height, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_ep_wel) {

	RDAT <- FDAT %>%
		filter(model == "EUT") %>%
		filter_(!is.na(win_var)) %>%
		select_(~starts_with("real"), ~ends_with("Est"), ~ends_with(wel_var),
		       ~starts_with("EUT"), win_var, "r", "mu")

	p.eut <- welfare5(RDAT, "EUT", "r", wel_var, win_var, legpos = "right", yaxis = T)
	rm(RDAT)

	RDAT <- FDAT %>%
		filter(model == "prelec") %>%
		filter_(!~is.na(win_var)) %>%
		select_(~starts_with("real"), ~ends_with("Est"), ~ends_with(wel_var),
		~starts_with("PRE"), win_var, "r", "alpha", "beta", "mu")

	print("Beginning with Prelec welfare5 alpha")
	p.prea <- welfare5(RDAT, "Prelec", "alpha", wel_var, win_var)

	print("Beginning with Prelec welfare5 beta")
	p.preb <- welfare5(RDAT, "Prelec", "beta", wel_var, win_var)

	rm(RDAT)

	legend <- get_legend(p.eut)

	# Plot the middle
	theme_set(theme_grey())

	# Join the middle and left side with the y axis lable
	mm <- plot_grid(p.eut + theme(legend.position = "none", strip.background = element_blank(), strip.text.y = element_blank()), 
	                p.prea + ylab("") + theme(strip.background = element_blank(), strip.text.y = element_blank()),
	                p.preb + ylab(""),
	                ncol = 3,
	                rel_widths = c(1.05, 1, 1.1))

	# Set the default theme
	#theme_set(theme_grey())

	# Join the actual plots with the legend
	mm <- plot_grid(mm, legend, ncol =2 , rel_widths = c(1.2, .15))

	cowplot::save_plot(paste0(win_var, "-ep-welfare5.pdf"), mm, device = "pdf", path = plot_dir, base_aspect_ratio = 1.4, base_height = 10)
}

}

}

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1", "HNG")
instruments <- c("HNG_1")

for(i in instruments) {
	per_inst(i)
}

print(warnings())

label_fun <- function(mnames) {
	ifelse(mnames == "EUT", "EUT",
		 ifelse(mnames == "POW", "RDU Power",
				ifelse(mnames == "INV", "RDU Inverse S",
					ifelse(mnames == "PRE", "RDU Prelec",
						ifelse(mnames == "NA", "NA", 
							ifelse(mnames == "Expected", "Expected", mnames))))))
}

wv_label_fun <- function(mnames) {

	nnames <- list(win_05 = "Win at 5%", default = "Default")

	for (win_var in win_vars) {
		mnames <- ifelse(mnames == win_var, nnames[[win_var]], mnames)
		for (inst in insts) {
			mnames <- ifelse(mnames == paste0(inst, "_", win_var), paste(inst, nnames[[win_var]], collapse = " "), mnames)
		}
	}

}

par_model <- function(mnames) {
	if (mnames == "EUT") {
		out <- c("r", "mu")
	} else if (mnames == "POW") {
		out <- c("alpha")
	} else if (mnames == "INV") {
		out <- c("alpha")
	} else if (mnames == "PRE") {
		out <- c("r", "mu", "alpha", "beta")
	}
	return(out)
}

ylim_model <- function(mnames) {
	if (mnames == "EUT") {
		out <- c(-10, 0)
	} else if (mnames == "POW") {
		out <- c(-20, 0)
	} else if (mnames == "INV") {
		out <- c(-20, 0)
	} else if (mnames == "PRE") {
		out <- c(-25, 0)
	}
	return(out)
}

plot_win <- function(dat, par, models, win_var) {
	#dbug <- 1
	#print(paste0("here", dbug)) ; dbug <- dbug + 1

	if (length(par) > 1 & length(models) > 1) 
		stop(paste("Pars is of length", length(par), ", and Models is of length", length(models), ", this is wrong"))
	else if (length(models) > 1)
		do_by <- "model"
	else
		do_by <- "par"

	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# Define the plot title
	title_mods <- paste(label_fun(models), collapse = ", ")
	title      <- paste(title_mods, "Subjects, N = ", N)

	cat(c(rep(" ", 4), "Plotting Winners for", title_mods, "\n"))

	dat[[win_var]] <- ifelse(is.na(dat[[win_var]]), "NA", dat[[win_var]])

	dat$point <- NA
	dat$U95   <- NA
	dat$L95   <- NA
	dat$win   <- NA
	dat$conv  <- NA

	dat1 <- lapply(mods, function(mod) {
		dat0       <- dat
		dat0$point <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_prob")]],     dat0$point)
		dat0$U95   <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_prob_U95")]], dat0$U95)
		dat0$L95   <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_prob_L95")]], dat0$L95)
		dat0$win   <- which(mods == mod)
		dat0$conv  <- 1
		return(dat0)
	})

	mmods <- c(mods, "NA")
	dat2 <- lapply(mmods, function(mod) {
		dat0       <- dat
		dat0$point <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_prob_na")]],     dat0$point)
		dat0$U95   <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_prob_na_U95")]], dat0$U95)
		dat0$L95   <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_prob_na_L95")]], dat0$L95)
		dat0$win   <- which(mmods == mod)
		dat0$conv  <- 2
		return(dat0)
	})

	dat1 <- do.call(rbind, dat1)
	dat2 <- do.call(rbind, dat2)
	dat  <- rbind(dat1, dat2)

	rm(dat1, dat2)

	# Make "model" an integer so it can be easily factored
	for (mod in pop_mods) {
		dat[which(dat$model == mod), "model"] <- which(pop_mods == mod)
	}

	# duplicate dataset for each parameter to be used
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
	rm(dat0)

	# Factor things
	dat$conv  <- factor(dat$conv,  levels = 1:2,                labels = c("Converged Only", "All Data"))
	dat$win   <- factor(dat$win,   levels = 1:length(mmods),    labels = label_fun(mmods))
	dat$model <- factor(dat$model, levels = 1:length(pop_mods), labels = label_fun(pop_mods))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),  labels = par)

	dat <- dat %>%
		select_("par", "par_grp", "model", "point", "U95", "L95", "win", "conv") %>%
		filter(!is.na(point))

	#dat %>% summary %>% print

	cat(c(rep(" ", 4), "Now plotting with", nrow(dat), "rows", "\n"))

	p <- ggplot(dat, aes_string(x = "par", y = "point", color = "win", linetype = "win"))
	p <- p + scale_y_continuous(limits = c(0,1))
	p <- p + geom_smooth(span = 0.15, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = U95), linetype = 5, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = L95), linetype = 5, se = F)

	if (do_by == "model") {
		p    <- p + facet_grid(conv~model)
		xlab <- paste(par, "Value")
	}
	else if (do_by == "par") {
		p    <- p + facet_grid(conv~par_grp, scales = "free_x")
		xlab <- "Parameter Value"
	}

	p <- p + labs(title = title, x = xlab, y = "Frequency of Winning")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

	return(p)
}

plot_wel <- function(dat, par, models, wel_var, win_var, ylim = c(-45, 5)) {
	#dbug <- 1
	#print(paste0("here", dbug)) ; dbug <- dbug + 1

	if (length(par) > 1 & length(models) > 1) 
		stop(paste("Pars is of length", length(par), ", and Models is of length", length(models), ", this is wrong"))
	else if (length(models) > 1)
		do_by <- "model"
	else
		do_by <- "par"

	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# Define the plot title
	title_mods <- paste(label_fun(models), collapse = ", ")
	title      <- paste(title_mods, "Subjects, N = ", N)

	cat(c(rep(" ", 4), "Plotting Welfare for", title_mods, "\n"))

	dat[[win_var]] <- ifelse(is.na(dat[[win_var]]), "NA", dat[[win_var]])

	dat$point <- NA
	dat$U95   <- NA
	dat$L95   <- NA
	dat$win   <- NA

	mmods <- c(mods, "Expected")
	dat1 <- lapply(mmods, function(mod) {
		dat0       <- dat
		if (mod != "Expected") {
			dat0$point <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_wel")]],     dat0$point)
			dat0$U95   <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_wel_U95")]], dat0$U95)
			dat0$L95   <- ifelse(dat0[[win_var]] == mod, dat0[[paste0(win_var, "_", mod, "_wel_L95")]], dat0$L95)
			dat0$win   <- which(mmods == mod)
		} else {
			dat0$point <- dat0[[paste0(win_var, "_ewel_point")]]
			dat0$win   <- which(mmods == mod)
		}
		return(dat0)
	})

	dat <- do.call(rbind, dat1)

	# Make "model" an integer so it can be easily factored
	for (mod in pop_mods) {
		dat[which(dat$model == mod), "model"] <- which(pop_mods == mod)
	}

	# duplicate dataset for each parameter to be used
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
	rm(dat0)

	dat$win     <- factor(dat$win,     levels = 1:length(mmods),    labels = label_fun(mmods))
	dat$model   <- factor(dat$model,   levels = 1:length(pop_mods), labels = label_fun(pop_mods))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),      labels = par)

	dat <- dat %>%
		select_("par", "par_grp", "model", "point", "U95", "L95", "win") %>%
		filter(!is.na(point))

	cat(c(rep(" ", 4), "Now plotting welfare with", nrow(dat), "rows", "\n"))

	p <- ggplot(dat, aes_string(x = "par", y = "point", color = "win", linetype = "win"))
	p <- p + facet_grid(~model)
	p <- p + geom_smooth(span = 0.15, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = U95), linetype = 5, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = L95), linetype = 5, se = F)

	if (do_by == "model") {
		p    <- p + facet_grid(~model)
		xlab <- paste(par, "Value")
	}
	else if (do_by == "par") {
		p    <- p + facet_grid(~par_grp, scales = "free_x")
		xlab <- "Parameter Value"
	}

	p <- p + coord_cartesian(ylim = ylim)
	p <- p + labs(title = title, x = xlab, y = paste("Value of Estimated", wel_var, "- Real", wel_var), color = "Classified Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

plot_exwel <- function(dat, par, models, wel_var, class_vars = win_vars, ylim = c(-45, 5)) {
	#dbug <- 1
	#print(paste0("here", dbug)) ; dbug <- dbug + 1

	if (length(par) > 1 & length(models) > 1) 
		stop(paste("Pars is of length", length(par), ", and Models is of length", length(models), ", this is wrong"))
	else if (length(models) > 1)
		do_by <- "model"
	else
		do_by <- "par"

	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# How many cols for the wrapping
	wrap_cols <- ceiling(sqrt(length(par)))

	# Define the plot title
	title_mods <- paste(label_fun(models), collapse = ", ")
	title      <- paste(title_mods, "Subjects, N = ", N)

	cat(c(rep(" ", 4), "Plotting Welfare for", title_mods, "\n"))

	dat$point <- NA
	dat$win   <- NA

	dat1 <- lapply(class_vars, function(ww) {
		dat0       <- dat
		dat0$point <- dat0[[paste0(ww, "_ewel_point")]]
		dat0$win   <- which(class_vars == ww)
		return(dat0)
	})

	dat <- do.call(rbind, dat1)

	# Make "model" an integer so it can be easily factored
	for (mod in pop_mods) {
		dat[which(dat$model == mod), "model"] <- which(pop_mods == mod)
	}

	# duplicate dataset for each parameter to be used
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
	rm(dat0)

#	cat("\n\n\n")
#	class_vars %>% print
#	wv_label_fun(class_vars) %>% print
#	cat("\n\n\n")

	dat$win     <- factor(dat$win,     levels = 1:length(class_vars), labels = class_vars)
	dat$model   <- factor(dat$model,   levels = 1:length(mods), labels = label_fun(mods))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),  labels = par)

	dat <- dat %>%
		select_("par", "par_grp", "model", "point", "win") %>%
		filter(!is.na(point))

	cat(c(rep(" ", 4), "Now plotting welfare with", nrow(dat), "rows", "\n"))

	p <- ggplot(dat, aes_string(x = "par", y = "point", color = "win", linetype = "win"))
	p <- p + geom_smooth(span = 0.15, se = F)

	if (do_by == "model") {
		p    <- p + facet_wrap(~model, ncol = wrap_cols)
		xlab <- paste(par, "Value")
	}
	else if (do_by == "par") {
		p    <- p + facet_wrap(~par_grp, ncol = wrap_cols, scales = "free_x")
		xlab <- "Parameter Value"
	}

	p <- p + coord_cartesian(ylim = ylim)
	p <- p + labs(title = title, x = xlab, y = paste("Value of Estimated", wel_var, "- Real", wel_var), color = "Classified Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

plot_exwel_diff <- function(dat, par, models, wel_var, class_var1 = win_vars[1], class_var2 = win_vars[2], ylim = c(-45, 5)) {
	#dbug <- 1
	#print(paste0("here", dbug)) ; dbug <- dbug + 1

	if (length(par) > 1 & length(models) > 1) 
		stop(paste("Pars is of length", length(par), ", and Models is of length", length(models), ", this is wrong"))
	else if (length(models) > 1)
		do_by <- "model"
	else
		do_by <- "par"

	# How many subjects do we have? To put in Title
	N <- nrow(dat)

	# How many cols for the wrapping
	wrap_cols <- ceiling(sqrt(length(par)))

	# Define the plot title
	title_mods <- paste(label_fun(models), collapse = ", ")
	title      <- paste(title_mods, "Subjects, N = ", N)

	cat(c(rep(" ", 4), "Plotting Welfare for", title_mods, "\n"))

	dat$point <- NA
	dat$win   <- NA


	dat1 <- lapply(1:length(class_var1), function(ww) {
		cv1 <- class_var1[ww]
		cv2 <- class_var2[ww]
		dat0       <- dat
		dat0$point <- dat0[[paste0(cv1, "_ewel_point")]] - dat0[[paste0(cv2, "_ewel_point")]]
		dat0$win   <- ww
		return(dat0)
	})

	dat <- do.call(rbind, dat1)

	# Make "model" an integer so it can be easily factored
	for (mod in pop_mods) {
		dat[which(dat$model == mod), "model"] <- which(pop_mods == mod)
	}

	# duplicate dataset for each parameter to be used
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
	rm(dat0)

#	cat("\n\n\n")
#	class_vars %>% print
#	wv_label_fun(class_vars) %>% print
#	cat("\n\n\n")

#	dat$win     <- factor(dat$win,     levels = 1:length(class_vars), labels = class_vars)
	dat$model   <- factor(dat$model,   levels = 1:length(mods), labels = label_fun(mods))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),  labels = par)

	dat <- dat %>%
		select_("par", "par_grp", "model", "point", "win") %>%
		filter(!is.na(point))

	cat(c(rep(" ", 4), "Now plotting welfare with", nrow(dat), "rows", "\n"))

	#p <- ggplot(dat, aes_string(x = "par", y = "point", color = "win", linetype = "win"))
	p <- ggplot(dat, aes_string(x = "par", y = "point"))
	p <- p + geom_smooth(span = 0.15, se = F)

	if (do_by == "model") {
		p    <- p + facet_wrap(~model, ncol = wrap_cols)
		xlab <- paste(par, "Value")
	}
	else if (do_by == "par") {
		p    <- p + facet_wrap(~par_grp, ncol = wrap_cols, scales = "free_x")
		xlab <- "Parameter Value"
	}

	p <- p + coord_cartesian(ylim = ylim)
	p <- p + labs(title = title, x = xlab, y = paste("Value of Estimated", wel_var, "- Real", wel_var), color = "Classified Model")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
	return(p)
}

welfare5 <- function(dat, mod, par, wel_var, win_var, legpos = "none", yaxis = F) {

	#dbug <- 1
	#print(paste("here:", dbug)) ; dbug <- dbug + 1

	cat(c("Plotting Welfare for 5%", "\n"))

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

cat(c("Plotting for instrument", inst, "\n"))

inst_plot_dir <- paste0(plot_dir, "/", inst, "/")
if (!dir.exists(inst_plot_dir)) {
	dir.create(inst_plot_dir, recursive = T)
}

# Get the Data
load(paste0(data_dir, inst, "-fitted.Rda"))
FDAT <- get(inst)
rm(list = inst)

do_ep_wel <- F

for (win_var in win_vars) {

	cat(c(rep(" ", 2), "Win_var is", win_var, "\n"))

if (do_win_all) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	p <- plot_win(dat = RDAT , par = c("mu"), models = mods, win_var = win_var)

	ggsave(paste0(win_var, "-all-win-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height * 2, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_wel_all) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	p <- plot_wel(dat = RDAT, par = c("mu"), models = mods, wel_var = wel_var, win_var = win_var)

	ggsave(paste0(win_var, "-mu-wel-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_win_ind) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		p <- plot_win(filter(RDAT, model == mod) , par = par_model(mod), models = mod, win_var = win_var)
		ggsave(paste0(win_var, "-", mod, "-win-",  inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = 2*height, units = units)
	}

	rm(list = c("p", "RDAT"))
}

if (do_wel_ind) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		p <- plot_wel(filter(RDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, win_var = win_var, ylim = ylim_model(mod))
		ggsave(paste0(win_var, "-", mod, "-wel-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height, units = units)
	}

	rm(list = c("p", "RDAT"))
}

if (do_ep_wel) {

	RDAT <- FDAT %>%
		filter(model == "EUT") %>%
		select_(~starts_with("real"), ~ends_with("Est"), ~ends_with(wel_var),
		       ~starts_with("EUT"), win_var, "r", "mu")

	p.eut <- welfare5(RDAT, "EUT", "r", wel_var, win_var, legpos = "right", yaxis = T)
	rm(RDAT)

	RDAT <- FDAT %>%
		filter(model == "PRE") %>%
		select_(~starts_with("real"), ~ends_with("Est"), ~ends_with(wel_var),
		~starts_with("PRE"), win_var, "r", "alpha", "beta", "mu")

	cat("Beginning with Prelec welfare5 alpha\n")
	p.prea <- welfare5(RDAT, "Prelec", "alpha", wel_var, win_var)

	cat("Beginning with Prelec welfare5 beta\n")
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

	cowplot::save_plot(paste0(win_var, "-ep-welfare5.pdf"), mm, device = "pdf", path = inst_plot_dir, base_aspect_ratio = 1.4, base_height = 10)
}

}

if (do_exwel_ind) {
	RDAT <- FDAT %>%
		select_(~ends_with("_ewel_point"), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		hmult <- sqrt(length(par_model(mod)))
		p <- plot_exwel(filter(RDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, ylim = ylim_model("PRE"))
		ggsave(paste0(mod, "-exwel-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
	}

	rm(list = c("p", "RDAT"))
}

if (do_exwel_diff_ind) {
	RDAT <- FDAT %>%
		select_(~ends_with("_ewel_point"), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		hmult <- sqrt(length(par_model(mod)))
		p <- plot_exwel_diff(filter(RDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, ylim = c(-5, 2.5))
		ggsave(paste0(mod, "-exwel-diff-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
	}

	rm(list = c("p", "RDAT"))
}

}

all_exwel <- function(insts) {

cat(c("Plotting for all instruments", "\n"))

# Get the Data
FDAT <- c.lapply(insts, function(inst) {
	cat(c(rep(" ", 2), "Loading", inst, "\n"))
	load(paste0(data_dir, inst, "-fitted.Rda"))
	dat <- get(inst)
	dat$inst <- inst
	for (win_var in win_vars) {
		dat[[paste0(inst, "_", win_var, "_ewel_point")]] <- dat[[paste0(win_var, "_ewel_point")]]
	}
	return(dat)
})

dnames <- lapply(FDAT, names) %>% unlist %>% unique

FDAT <- lapply(FDAT, function(dat) {
	onames  <- names(dat)
	nonames <- dnames[which(! dnames %in% onames)]
	dat[,nonames] <- NA
	return(dat)
})

FDAT <- do.call(rbind, FDAT)

FDAT <- FDAT %>%
	select(ends_with("_ewel_point"), r, mu, alpha, beta, model)

new_vars <- c()
for (inst in insts) {
	for (win_var in win_vars) {
		new_vars <- c(new_vars, paste0(inst, "_", win_var))
	}
}

c.lapply(pop_mods, function(mod) {
	hmult <- sqrt(length(par_model(mod)))
	p <- plot_exwel(filter(FDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, class_vars = new_vars, ylim = ylim_model("PRE"))
	ggsave(paste0(mod, "-exwel-full.", dev.type), plot = p, device = dev.type, path = plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
})

}

all_exwel_diff <- function(insts) {

cat(c("Plotting for all instruments", "\n"))

# Get the Data
FDAT <- c.lapply(insts, function(inst) {
	cat(c(rep(" ", 2), "Loading", inst, "\n"))
	load(paste0(data_dir, inst, "-fitted.Rda"))
	dat <- get(inst)
	dat$inst <- inst
	for (win_var in win_vars) {
		dat[[paste0(inst, "_", win_var, "_ewel_point")]] <- dat[[paste0(win_var, "_ewel_point")]]
	}
	return(dat)
})

dnames <- lapply(FDAT, names) %>% unlist %>% unique

FDAT <- lapply(FDAT, function(dat) {
	onames  <- names(dat)
	nonames <- dnames[which(! dnames %in% onames)]
	dat[,nonames] <- NA
	return(dat)
})

FDAT <- do.call(rbind, FDAT)

FDAT <- FDAT %>%
	select(ends_with("_ewel_point"), r, mu, alpha, beta, model)

new_vars <- c()
for (inst in insts) {
	for (win_var in win_vars) {
		new_vars <- c(new_vars, paste0(inst, "_", win_var))
	}
}

c.lapply(pop_mods, function(mod) {
	hmult <- sqrt(length(par_model(mod)))
	p <- plot_exwel_diff(filter(FDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, class_vars = new_vars, ylim = ylim_model("PRE"))
	ggsave(paste0(mod, "-exwel-full.", dev.type), plot = p, device = dev.type, path = plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
})

}



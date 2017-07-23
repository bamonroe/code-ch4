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

inst_labels <- function(inst) {
	ifelse(inst == "HNG_1", "1",
		ifelse(inst == "HNG_3", "3",
			ifelse(inst == "HNG_5", "5",
				ifelse(inst == "HNG_7", "7",
					ifelse(inst == "HNG_9", "9",
						ifelse(inst == "HNG_11", "11",
							ifelse(inst == "HNG", "13", inst)))))))
}

par_labels <- function(pars) {
	ifelse(pars == "mu", "lambda", pars)
}

ylim_model <- function(mnames, inst = "HNG_1") {

	i1 <- length(inst) == 1
	i2 <- length(inst) == 2

	if ((i1) & (inst[1] == "HNG")) {
		if (mnames[1] == "EUT") {
			out <- c(-10, 0)
		} else if (mnames[1] == "POW") {
			out <- c(-20, 0)
		} else if (mnames[1] == "INV") {
			out <- c(-20, 0)
		} else if (mnames[1] == "PRE") {
			out <- c(-20, 8)
		}
	} else if ((i1) & (inst[1] == "HNG_1")) {
		if (mnames[1] == "EUT") {
			out <- c(-10, 0)
		} else if (mnames[1] == "POW") {
			out <- c(-20, 0)
		} else if (mnames[1] == "INV") {
			out <- c(-20, 0)
		} else if (mnames[1] == "PRE") {
			out <- c(-25, 0)
		}
	} else {
		if (mnames[1] == "EUT") {
			out <- c(-10, 0)
		} else if (mnames[1] == "POW") {
			out <- c(-20, 0)
		} else if (mnames[1] == "INV") {
			out <- c(-20, 0)
		} else if (mnames[1] == "PRE") {
			out <- c(-25, 0)
		}
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
		dat0$point <- dat0[[paste0(win_var, "_", mod, "_prob")]]
		dat0$U95   <- dat0[[paste0(win_var, "_", mod, "_prob_U95")]]
		dat0$L95   <- dat0[[paste0(win_var, "_", mod, "_prob_L95")]]
		dat0$win   <- which(mods == mod)
		dat0$conv  <- 1
		#colnames(dat0) %>% length %>% print
		#colnames(dat0) %>% print
		return(dat0)
	})

	mmods <- c(mods, "NA")
	dat2 <- lapply(mmods, function(mod) {
		dat0       <- dat
		dat0$point <- dat0[[paste0(win_var, "_", mod, "_prob_na")]]
		dat0$U95   <- dat0[[paste0(win_var, "_", mod, "_prob_na_U95")]]
		dat0$L95   <- dat0[[paste0(win_var, "_", mod, "_prob_na_L95")]]
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
	par <- par_labels(par)

	dat$conv    <- factor(dat$conv,    levels = 1:2,                labels = c("Converged Only", "All Data"))
	dat$win     <- factor(dat$win,     levels = 1:length(mmods),    labels = label_fun(mmods))
	dat$model   <- factor(dat$model,   levels = 1:length(pop_mods), labels = label_fun(pop_mods))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),      labels = par)

	dat <- dat %>%
		select_("par", "par_grp", "model", "point", "U95", "L95", "win", "conv") %>%
		filter(!is.na(point))

	#dat %>% summary %>% print

	cat(c(rep(" ", 6), "Now plotting with", nrow(dat), "rows", "\n"))

	p <- ggplot(dat, aes_string(x = "par", y = "point", color = "win", linetype = "win"))
	p <- p + scale_y_continuous(limits = c(0,1))

	if (do_by == "model") {
		p    <- p + facet_grid(conv~model)
		xlab <- paste(par, "Value")
	}
	else if (do_by == "par") {
		p    <- p + facet_grid(conv~par_grp, scales = "free_x")
		xlab <- "Parameter Value"
	}

	#p <- p + geom_point()
	p <- p + geom_smooth(span = 0.15, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = U95), linetype = 5, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = L95), linetype = 5, se = F)

	p <- p + labs(title = title, x = xlab, y = "Frequency of Winning", color = "Winning Model")
	p <- p + scale_linetype(guide = "none")
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
			dat0$point <- dat0[[paste0(win_var, "_", mod, "_wel")]]
			dat0$U95   <- dat0[[paste0(win_var, "_", mod, "_wel_U95")]]
			dat0$L95   <- dat0[[paste0(win_var, "_", mod, "_wel_L95")]]
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

	par <- par_labels(par)

	dat$win     <- factor(dat$win,     levels = 1:length(mmods),    labels = label_fun(mmods))
	dat$model   <- factor(dat$model,   levels = 1:length(pop_mods), labels = label_fun(pop_mods))
	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),      labels = par)

	dat <- dat %>%
		select_("par", "par_grp", "model", "point", "U95", "L95", "win") %>%
		filter(!is.na(point))

	cat(c(rep(" ", 4), "Now plotting welfare with", nrow(dat), "rows", "\n"))

	p <- ggplot(dat, aes_string(x = "par", y = "point", color = "win", linetype = "win"))
	p <- p + facet_wrap(~model, ncol = 2)
	p <- p + geom_smooth(span = 0.15, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = U95), linetype = 5, se = F)
	p <- p + geom_smooth(span = 0.15, aes(y = L95), linetype = 5, se = F)

	if (do_by == "model") {
		p    <- p + facet_wrap(~model, ncol = 2)
		xlab <- paste(par, "Value")
	}
	else if (do_by == "par") {
		p    <- p + facet_wrap(~par_grp, ncol = 2, scales = "free_x")
		xlab <- "Parameter Value"
	}

	p <- p + coord_cartesian(ylim = ylim)
	p <- p + labs(title = title, x = xlab, y = paste("Value of Estimated", wel_var, "- Real", wel_var), color = "Classified Model")
	p <- p + scale_linetype(guide = "none")
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

	par <- par_labels(par)

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
	p <- p + scale_linetype(guide = "none")
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

	par <- par_labels(par)

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
	p <- p + scale_linetype(guide = "none")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
	
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
	hmult <- (2*length(par_model("EUT")) / 2) ^ 0.7
	ggsave(paste0(win_var, "-all-win-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height * hmult, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_wel_all) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	p <- plot_wel(dat = RDAT, par = c("mu"), models = mods, wel_var = wel_var, win_var = win_var)
	hmult <- (length(par_model("EUT")) / 2) ^ 0.7
	ggsave(paste0(win_var, "-mu-wel-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height * hmult, units = units)

	rm(list = c("p", "RDAT"))
}

if (do_win_ind) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		p <- plot_win(filter(RDAT, model == mod) , par = par_model(mod), models = mod, win_var = win_var)

		hmult <- ( ifelse(mod == "EUT", 2, 1) * length(par_model(mod)) / 2) ^ 0.7
		ggsave(paste0(win_var, "-", mod, "-win-",  inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height * hmult, units = units)
	}

	rm(list = c("p", "RDAT"))
}

if (do_wel_ind) {
	RDAT <- FDAT %>%
		select_(~starts_with(win_var), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		p <- plot_wel(filter(RDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, win_var = win_var, ylim = ylim_model(mod, inst))
		hmult <- (length(par_model(mod)) / 2) ^ 0.7
		ggsave(paste0(win_var, "-", mod, "-wel-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, width = width, height = height * hmult, units = units)
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
		hmult <- (length(par_model(mod)) / 2) ^ 0.8
		p <- plot_exwel(filter(RDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, ylim = ylim_model("PRE", inst))
		ggsave(paste0(mod, "-exwel-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
	}

	rm(list = c("p", "RDAT"))
}

if (do_exwel_diff_ind) {
	RDAT <- FDAT %>%
		select_(~ends_with("_ewel_point"), "r", "mu", "alpha", "beta", "model")

	for (mod in pop_mods) {
		hmult <- (length(par_model(mod)) / 2) ^ 0.8
		p <- plot_exwel_diff(filter(RDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, ylim = c(-5, 2.5))
		ggsave(paste0(mod, "-exwel-diff-", inst, ".", dev.type), plot = p, device = dev.type, path = inst_plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
	}

	rm(list = c("p", "RDAT"))
}

print(warnings())

}




all_exwel <- function(insts) {

cat(c("Plotting for all instruments", "\n"))

# Get the Data
FDAT <- c.lapply(insts, function(inst) {
	cat(c(rep(" ", 2), "Loading", inst, "\n"))
	load(paste0(data_dir, inst, "-fitted.Rda"))
	dat <- get(inst)
	dat$inst <- inst

	if (inst == "HNG_1") {
		win_vars2 <- win_vars
	} else {
		win_vars2 <- "win_05"
	}

	for (win_var in win_vars2) {
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
	if (inst == "HNG") {
		win_vars2 <- "win_05"
	} else {
		win_vars2 <- win_vars
	}
	for (win_var in win_vars2) {
		new_vars <- c(new_vars, paste0(inst, "_", win_var))
	}
}

c.lapply(pop_mods, function(mod) {
	hmult <- (length(par_model(mod)) / 2) ^ 0.8
	p <- plot_exwel(filter(FDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, class_vars = new_vars, ylim = ylim_model("PRE", insts))
	ggsave(paste0(mod, "-exwel-full.", dev.type), plot = p, device = dev.type, path = plot_dir, scale = 1, width = width, height = height * hmult, units = units)
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
	p <- plot_exwel_diff(filter(FDAT, model == mod) , par = par_model(mod), models = mod, wel_var = wel_var, class_vars = new_vars, ylim = ylim_model("PRE", insts))
	ggsave(paste0(mod, "-exwel-full.", dev.type), plot = p, device = dev.type, path = plot_dir, scale = 1.2, width = width, height = height * hmult, units = units)
})

}


all_correct_prob <- function(insts) {

dbug <- 0

cat(c("Plotting Prob of Correct for all instruments", "\n"))

# Get the Data
FDAT <- lapply(insts, function(inst) {
	cat(c(rep(" ", 2), "Loading", inst, "\n"))
	load(paste0(data_dir, inst, "-fitted.Rda"))
	dat <- get(inst)
	dat$inst <- inst
	dat[["correct_prob"]] <- ifelse(dat[["model"]] == "PRE", dat$win_05_PRE_prob,
	                         ifelse(dat[["model"]] == "EUT", dat$win_05_EUT_prob, NA))
	dat <- dat %>%
		select(correct_prob, r, mu, alpha, beta, model, inst)
	return(dat)
})

FDAT <- do.call(rbind, FDAT)

all_dat <- split(FDAT, FDAT$model)

cat(c("Plotting for all dat", "\n"))
null <- lapply(names(all_dat), plot_correct, fdat = all_dat, fsuffix = "ALL", insts = insts)

FDAT <- FDAT %>%
	filter(model == "PRE" | model == "EUT")

all_dat <- split(FDAT, FDAT$model)

cat(c("Plotting for converged dat", "\n"))
null <- lapply(names(all_dat), plot_correct, fdat = all_dat, fsuffix = "CON", insts = insts)

}

plot_correct <- function(mod, fdat, fsuffix, insts) {
		#dbug <- 0
		#cat("here", dbug, "\n") ; dbug <- dbug + 1

	par <- par_model(mod)
	dat <- fdat[[mod]]

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

	par <- par_labels(par)

	dat$par_grp <- factor(dat$par_grp, levels = 1:length(par),      labels = par)
	dat$inst    <- factor(dat$inst,    levels = insts,    labels = inst_labels(insts))

	p <- ggplot(dat, aes(x = par, y = correct_prob, color = inst, linetype = inst))
	p <- p + scale_y_continuous(limits = c(0,1))

	wrap_cols <- ceiling(sqrt(length(par)))
	p    <- p + facet_wrap(~par_grp, scales = "free_x", ncol = wrap_cols)

	#p <- p + geom_point()
	p <- p + geom_smooth(se = F)

	title_mods <- label_fun(mod)
	title      <- paste("Probability of Correct Classification for", title_mods, "Subjects")

	p <- p + labs(title = title, x = "Parameter Value", y = "Probability of Correct Classification", color = "Instument Repetitions")
	p <- p + scale_linetype(guide = "none")
	p <- p + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

	hmult <- sqrt(length(par))

	cat("  Saving for", mod, "\n")
	ggsave(paste0(mod, "-cprob-full-", fsuffix, ".", dev.type), plot = p, device = dev.type, path = plot_dir, 
	       scale = 1.2, width = width, height = height * hmult, units = units)
}

get_fit <- function(mod, gam_mod = gam_mod, pop_mod, ptype, dat) {
	#cat(rep(" ", 6), "Mod:", mod, "\n")
	fit <- predict(gam_mod[[pop_mod]][[mod]][[ptype]], dat, type ="response")
	fit
}

pred_per_mod <- function(pop_mod, gam_mod, ptype, dat, mods) {
	cat(rep(" ", 4), "Pop Mod:", pop_mod, "\n")
	dat <- dat[[pop_mod]]
	out <- lapply(mods, get_fit, gam_mod = gam_mod, pop_mod = pop_mod, ptype = ptype, dat = dat)
	out <- do.call(cbind, out)
	colnames(out) <- c(mods)
	out
}

pred_per_win <- function(class_var, dat, inst) {
	#dbug <- 1
	#print(paste("Here:", dbug)) ; dbug <- dbug + 1

	cat(rep(" ", 2), "Class Var:", class_var, "\n")

	fname <- paste0(inst, "_", class_var, "_fit")
	load(paste0(fit_dir, fname, ".Rda"))

	gam_mod <- get(fname)
	mmods <- c(mods)
	cat(rep(" ", 3), "Type Var: prob\n")
	prob <- lapply(pop_mods, pred_per_mod, gam_mod = gam_mod, ptype = "prob", dat = dat, mods = mmods)
	names(prob) <- pop_mods
	cat(rep(" ", 3), "Type Var: wel\n")
	wel  <- lapply(pop_mods, pred_per_mod, gam_mod = gam_mod, ptype = "wel",  dat = dat, mods = mmods)
	names(wel) <- pop_mods

	expec <- lapply(pop_mods, function(pop_mod) {
		expec <- prob[[pop_mod]] * wel[[pop_mod]]
		rowSums(as.matrix(expec))
	})

	prob  <- lapply(prob,  colMeans)
	wel   <- lapply(wel,   colMeans)
	expec <- lapply(expec, mean)

	prob  <- do.call(rbind, prob)
	wel   <- do.call(rbind, wel)
	expec <- do.call(rbind, expec)
	wel   <- cbind(wel, expec)

	colnames(prob) <- mmods
	rownames(prob) <- paste0(pop_mods, "_pop")
	colnames(wel)  <- c(mmods, "Expected")
	rownames(wel)  <- paste0(pop_mods, "_pop")

	cat(rep(" ", 3), "Type Var: prob-na\n")
	gam_mod <- get(paste0(fname, "_na"))
	mmods <- c(mods, "NA")
	prob_na <- lapply(pop_mods, pred_per_mod, gam_mod = gam_mod, ptype = "prob", dat = dat, mods = mmods)
	prob_na <- lapply(prob_na,  colMeans)
	prob_na <- do.call(rbind, prob_na)
	colnames(prob_na) <- mmods
	rownames(prob_na) <- paste0(pop_mods, "_pop")

	save(prob, wel, prob_na, file = paste0(fit_dir, inst, "-", class_var, "-table.Rda"))

}

ln_mean <- function(mean, sd) {
	log(mean / sqrt(1 + ( (sd^2) / (mean^2) )))
}

ln_sd <- function(mean, sd) {
	sqrt( log(1 + ( (sd^2) / (mean^2) )))
}

mk_pop <- function(pop_mod, NN) {

#	dbug <- 1
#	print(paste("Here:", dbug)) ; dbug <- dbug + 1

	pop <- list(
		EUT = list(means = c(r = 0.5, mu = 0.1),                            std = c(r = 0.1, mu = 0.02)),
		POW = list(means = c(r = 0.5, alpha = 0.65, mu = 0.1),              std = c(r = 0.1, alpha = 0.1, mu = 0.02)),
		INV = list(means = c(r = 0.5, alpha = 0.65, mu = 0.1),              std = c(r = 0.1, alpha = 0.1, mu = 0.02)),
		PRE = list(means = c(r = 0.5, alpha = 0.65, beta = 0.65, mu = 0.1), std = c(r = 0.1, alpha = 0.1, beta = 0.1, mu = 0.02))
	)
	per     <- list(EUT = 3, POW = 1, INV = 1, PRE = 1)

	# Just use the pop mod we're considering
	pop     <- pop[[pop_mod]]
	portion <- per[[pop_mod]] / do.call(sum, per)
	NN      <- ceiling(NN * portion)

	# Generate uncorelated random normal distributions
	mm  <- rep(0, length(pop$means))
	sig <- matrix(0, nrow = length(pop$means), ncol = length(pop$means))
	diag(sig) <- 1
	dat <- mvhnorm(NN, mm, sig) %>% as.data.frame

	# Change parameters values to accomodate log-normal
	means <- ln_mean(pop$means, pop$std)
	std   <- ln_sd(pop$means, pop$std)

	# The first parameter is normally distributed
	means[1] <- pop$means[1]
	std[1]   <- pop$std[1]

	# Adjust the distributions for the parameter means
	for(i in 1:ncol(dat)) {
		dat[,i] <- (dat[,i]*std[i]) + means[i]
	}

	# Adjust the ones that need adjusting for log-normal
	dat[, seq(2,ncol(dat))] <- exp(dat[, seq(2,ncol(dat))])

	# All the parameter names
	all_names <- c("r", "alpha", "beta", "mu")
	# The ones for this population
	dnames <- names(pop$means)

	# Label the parameter values for this pop
	colnames(dat) <- dnames

	# Need to add all parameters to dat, so make NAs if they're not sued
	not_names <- all_names[! all_names %in% dnames]
	dat[,not_names] <- NA

	# Label this population
	dat$model <- pop_mod

	return(dat)
}

pred_pop <- function(inst) {
	cjat("Predicting Pop for", inst, "\n")

	dat <- lapply(pop_mods, mk_pop, NN = 10000)
	names(dat) <- pop_mods

	lapply(win_vars, pred_per_win, dat = dat, inst = inst)


lapply(insts, pred_pop)

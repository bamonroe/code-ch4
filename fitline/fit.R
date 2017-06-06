fit_gam <- function(win_mod, dat, mod, wel_var, class_var) {
#dbug <- 1
#print(paste("here", dbug)) ; dbug <- dbug + 1

	cat(c(rep(" ",6),"Win:", win_mod),"\n")

	dat  <- dat %>% filter(model == mod)

	dat$win <- ifelse(dat[[class_var]] == win_mod, 1, 0)

	cat(c(rep(" ",8),"Fitting Probability","\n"))

	if (mod == "EUT") {
		pfit <- gam(win ~ s(r) + s(mu), data = dat, family = binomial(link = logit))
		#pfit <- gam(win ~ s(r) + s(mu), data = dat, family = binomial())
		#pfit <- gam(win ~ s(r) + s(mu), data = dat)
	} else if (mod == "POW") {
		pfit <- gam(win ~ s(r) + s(alpha) + s(mu), data = dat, family = binomial(link = logit))
		#pfit <- gam(win ~ s(r) + s(alpha) + s(mu), data = dat, family = binomial())
		#pfit <- gam(win ~ s(r) + s(alpha) + s(r, alpha) + s(mu), data = dat)
	} else if (mod == "INV") {
		pfit <- gam(win ~ s(r) + s(alpha) + s(mu), data = dat, family = binomial(link = logit))
		#pfit <- gam(win ~ s(r) + s(alpha) + s(mu), data = dat, family = binomial())
		#pfit <- gam(win ~ s(r) + s(alpha) + s(r, alpha) + s(mu), data = dat)
	} else if (mod == "PRE") {
		#pfit <- gam(win ~ s(r) + s(alpha) + s(beta) + s(alpha, beta)+ s(r, alpha, beta) + s(mu), data = dat)
		pfit <- gam(win ~ s(r) + s(alpha) + s(beta) + s(mu), data = dat, family = binomial(link = logit))
		#pfit <- gam(win ~ s(r) + s(alpha) + s(beta) + s(mu), data = dat, family = binomial())
		#pfit <- gam(win ~ s(r) + s(alpha) + s(beta) + s(mu), data = dat)
	} else {
		stop("Not a correct model")
	}

	# NA models don't have estimates to make welfare predictions
	if (win_mod == "NA") return(list(prob = pfit, wel = NULL))

	dat <- dat %>% filter(win == 1)

	dat$wel <- dat[[paste0(win_mod, "_", wel_var)]] - dat[[paste0("real_", wel_var)]]

	#dat %>% apply(1, factor) %>% t %>% as.data.frame %>% select_("win", class_var, "model", "wel") %>% summary %>% print

	cat(c(rep(" ",8),"Fitting Welfare","\n"))
	if (mod == "EUT") {
		wfit <- gam(wel ~ s(r) + s(mu), data = dat)
	} else if (mod == "POW") {
		#wfit <- gam(wel ~ s(r) + s(alpha) + s(r, alpha) + s(mu), data = dat)
		wfit <- gam(wel ~ s(r) + s(alpha) + s(mu), data = dat)
	} else if (mod == "INV") {
		#wfit <- gam(wel ~ s(r) + s(alpha) + s(r, alpha) + s(mu), data = dat)
		wfit <- gam(wel ~ s(r) + s(alpha) + s(mu), data = dat)
	} else if (mod == "PRE") {
		#wfit <- gam(wel ~ s(r) + s(alpha) + s(beta) + s(alpha, beta)+ s(r, alpha, beta) + s(mu), data = dat)
		wfit <- gam(wel ~ s(r) + s(alpha) + s(beta) + s(mu), data = dat)
	} else {
		stop("Not a correct model")
	}

	return(list(prob = pfit, wel = wfit))
}

fit_per_class <- function(class_var, dat, inst) {
	cat(c(rep(" ",2),"Class Var:", class_var),"\n")

	load_suffix <- "-bak.Rda"
	save_suffix <- ".Rda"

	gam_fits <- lapply(pop_mods, function(mod) {
		cat(c(rep(" ",4),"Mod:", mod),"\n")
		dat0  <- dat[which(dat[[class_var]] != "NA"),]
		mmods <- c(mods)
		out   <- lapply(mmods, fit_gam,
		                dat = dat0, mod = mod, wel_var = wel_var, class_var = class_var)
		names(out) <- mmods
		return(out)
	})

	gam_fits_na <- lapply(pop_mods, function(mod) {
		cat(c(rep(" ",4),"NA Mod:", mod),"\n")
		mmods <- c(mods, "NA")
		dat0  <- dat
		out   <- lapply(mmods, fit_gam,
		                dat = dat0, mod = mod, wel_var = wel_var, class_var = class_var)
		names(out) <- mmods
		return(out)
	})

	names(gam_fits)    <- pop_mods
	names(gam_fits_na) <- pop_mods

	fit_name    <- paste0(inst, "_", class_var, "_fit")
	fit_name_na <- paste0(fit_name, "_na")

	assign(fit_name,    gam_fits)
	assign(fit_name_na, gam_fits_na)

	save(list=c(fit_name, fit_name_na), file=paste0(fit_dir, fit_name, save_suffix))
	cat("\n")

}

fitter <- function(inst) {
	#dbug <- 1
	#print(paste("here", dbug)) ; dbug <- dbug + 1
	load_suffix <- "-bak.Rda"
	load(paste0(data_dir, inst, load_suffix))

	cat(c("\n", "Fitting for", inst),"\n")

	# Load the instrument into a known var
	dat <- get(inst)

	if (length(insts) > length(win_vars)) {
		null <- lapply(win_vars, fit_per_class, dat, inst)
	} else {
		null <- c.lapply(win_vars, fit_per_class, dat, inst)
	}
	cat("\n")

}

if (length(insts) > length(win_vars)) {
	c.lapply(insts, fitter)
} else {
	lapply(insts, fitter)
}


library(halton)
library(dplyr)
library(mgcv)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
fit_dir    <- "../data/lo_fits/"
load_suffix  <- "_loess-mini.Rda"
load_suffix  <- "_loess-gam.Rda"
save_suffix <- "-win.Rda"

# mvhnorm is a multi-variate normal distribution using halton draws

# Using the log-normal distribution, get the pre-exponential mean and sd

ln_mean <- function(mean, sd) {
	log(mean / sqrt(1 + ( (sd^2) / (mean^2) )))
}

ln_sd <- function(mean, sd) {
	sqrt( log(1 + ( (sd^2) / (mean^2) )))
}

mk_matrix <- function(H, means, sd, burn) {

	sigma <- matrix(0, nrow = length(means), ncol = length(means))
	diag(sigma) <- sd^2

	mvhnorm(H, means, sigma, burn = burn) %>% data.frame

}

mk_pop <- function(mod, HH, props) {
	burn <- 40
	eut_prop <- props[["EUT"]]
	pow_prop <- props[["POW"]]
	inv_prop <- props[["INV"]]
	pre_prop <- props[["PRE"]]

	if (props[[mod]] == 0) {
		pdat <- data.frame(matrix(NA, ncol = 5))
		colnames(pdat) <- c("r", "mu", "alpha", "beta", "model")
		return(pdat)
	}

	r_mean <- get("r_mean", envir = .GlobalEnv)
	r_sd   <- get("r_sd", envir = .GlobalEnv)

	mu_mean0 <- get("mu_mean0", envir = .GlobalEnv)
	mu_sd0   <- get("mu_sd0", envir = .GlobalEnv)
	mu_mean  <- ln_mean(mu_mean0, mu_sd0)
	mu_sd    <- ln_sd(mu_mean0, mu_sd0)

	alpha_mean0 <- get("alpha_mean0", envir = .GlobalEnv)
	alpha_sd0   <- get("alpha_sd0", envir = .GlobalEnv)
	alpha_mean  <- ln_mean(alpha_mean0, alpha_sd0)
	alpha_sd    <- ln_sd(alpha_mean0, alpha_sd0)

	alpha2_mean0 <- get("alpha2_mean0", envir = .GlobalEnv)
	alpha2_sd0   <- get("alpha2_sd0", envir = .GlobalEnv)
	alpha2_mean  <- ln_mean(alpha2_mean0, alpha2_sd0)
	alpha2_sd    <- ln_sd(alpha2_mean0, alpha2_sd0)


	beta_mean0 <- get("beta_mean0", envir = .GlobalEnv)
	beta_sd0   <- get("beta_sd0", envir = .GlobalEnv)
	beta_mean  <- ln_mean(beta_mean0, beta_sd0)
	beta_sd    <- ln_sd(beta_mean0, beta_sd0)

	beta2_mean0 <- get("beta2_mean0", envir = .GlobalEnv)
	beta2_sd0   <- get("beta2_sd0", envir = .GlobalEnv)
	beta2_mean  <- ln_mean(beta2_mean0, beta2_sd0)
	beta2_sd    <- ln_sd(beta2_mean0, beta2_sd0)

	pop_mix <- get("pop_mix", envir = .GlobalEnv)

	eut_means <- c(r = r_mean, mu = mu_mean)
	eut_sd    <- c(r = r_sd, mu = mu_sd)

	pow_means1 <- c(r = r_mean, mu = mu_mean, alpha = alpha_mean)
	pow_sd1    <- c(r = r_sd, mu = mu_sd, alpha = alpha_sd)
	pow_means2 <- c(r = r_mean, mu = mu_mean, alpha = alpha2_mean)
	pow_sd2    <- c(r = r_sd, mu = mu_sd, alpha = alpha2_sd)

	inv_means1 <- c(r = r_mean, mu = mu_mean, alpha = alpha_mean)
	inv_sd1    <- c(r = r_sd, mu = mu_sd, alpha = alpha_sd)
	inv_means2 <- c(r = r_mean, mu = mu_mean, alpha = alpha2_mean)
	inv_sd2    <- c(r = r_sd, mu = mu_sd, alpha = alpha2_sd)

	pre_means1 <- c(r = r_mean, mu = mu_mean, alpha = alpha_mean, beta = beta_mean)
	pre_sd1    <- c(r = r_sd, mu = mu_sd, alpha = alpha_sd, beta = beta_sd)
	pre_means2 <- c(r = r_mean, mu = mu_mean, alpha = alpha2_mean, beta = beta2_mean)
	pre_sd2    <- c(r = r_sd, mu = mu_sd, alpha = alpha2_sd, beta = beta2_sd)


	# Now make the correlated matricies
	if (mod == "EUT") {
	pdat <- mk_matrix(HH * eut_prop, eut_means, eut_sd , burn = burn)
	pdat$alpha <- NA
	pdat$beta  <- NA
	pdat$model <- 1
	}

	if (mod == "POW") {
	pdat <- mk_matrix(HH * pow_prop * pop_mix, pow_means1, pow_sd1, burn = burn + HH )
	pdat <- mk_matrix(HH * pow_prop * pop_mix, pow_means2, pow_sd2, burn = burn + 2*HH ) %>% rbind(pdat)
	pdat$beta <- NA
	pdat$model <- 2
	}

	if (mod == "INV") {
	pdat <- mk_matrix(HH * inv_prop * pop_mix, inv_means1, inv_sd1 , burn = burn + 3*HH )
	pdat <- mk_matrix(HH * inv_prop * pop_mix, inv_means2, inv_sd2 , burn = burn + 4*HH ) %>% rbind(pdat)
	pdat$beta <- NA
	pdat$model <- 3
	}

	if (mod == "PRE") {
	pdat <- mk_matrix(HH * pre_prop * pop_mix, pre_means1, pre_sd1 , burn = burn + 5*HH )
	pdat <- mk_matrix(HH * pre_prop * pop_mix, pre_means2, pre_sd2 , burn = burn + 6*HH ) %>% rbind(pdat)
	pdat$model <- 4
	}

	colnames(pdat) <- c("r", "mu", "alpha", "beta", "model")

	pdat$mu    <- exp(pdat$mu)
	pdat$alpha <- exp(pdat$alpha)
	pdat$beta  <- exp(pdat$beta)
	return(pdat)
}

who_won <- function(lo_obj, HH, props) {

	lo_names <- names(lo_obj)

	predictions <- lapply(lo_names, function(mname, HH, props) {
		pdat <- mk_pop(mname, HH, props)
		pdat[[paste0(mname,"_EUT")]] <- predict(lo_obj[[mname]][["EUT"]], pdat)
		pdat[[paste0(mname,"_POW")]] <- predict(lo_obj[[mname]][["POW"]], pdat)
		pdat[[paste0(mname,"_INV")]] <- predict(lo_obj[[mname]][["INV"]], pdat)
		pdat[[paste0(mname,"_PRE")]] <- predict(lo_obj[[mname]][["PRE"]], pdat)
		pdat
	}, HH = HH, props = props)

	pred_sums <- lapply(predictions, function(pdat) {
		eut <- pdat %>% select(ends_with("EUT")) %>% sum(na.rm = T)
		pow <- pdat %>% select(ends_with("POW")) %>% sum(na.rm = T)
		inv <- pdat %>% select(ends_with("INV")) %>% sum(na.rm = T)
		pre <- pdat %>% select(ends_with("PRE")) %>% sum(na.rm = T)
		data.frame(EUT = eut, POW = pow, INV = inv, PRE = pre)
	}) 

	pred_mat <- do.call(rbind, pred_sums)
	rownames(pred_mat) <- paste(colnames(pred_mat), "subjects")

	list(predictions = predictions, pred_sums = pred_mat)
}


HH <- 10000

r_mean       <- 0.6
r_sd         <- 0.1

mu_mean0     <- 0.10
mu_sd0       <- 0.02

alpha_mean0  <- .6
alpha_sd0    <- 0.15
alpha2_mean0 <- 1.6
alpha2_sd0   <- 0.15

beta_mean0   <- .6
beta_sd0     <- 0.15
beta2_mean0  <- 1.6
beta2_sd0    <- 0.15

pop_mix   <- 0.5

eut_prop <- .5
pow_prop <- .1
inv_prop <- .1
pre_prop <- .3

# make sure the props sum to 1
eut_prop0 <- eut_prop / (eut_prop + pow_prop + inv_prop + pre_prop)
pow_prop0 <- pow_prop / (eut_prop + pow_prop + inv_prop + pre_prop)
inv_prop0 <- inv_prop / (eut_prop + pow_prop + inv_prop + pre_prop)
pre_prop0 <- pre_prop / (eut_prop + pow_prop + inv_prop + pre_prop)
props <- list(EUT = eut_prop0, POW = pow_prop0, INV = inv_prop0, PRE = pre_prop0)

for (inst in instruments) {

	load(paste0(fit_dir, inst, load_suffix))

	mod_fit <- get(paste0(inst, "_loess"))

	#list(predictions = predictions, pred_sums = pred_mat)
	winners <- who_won(mod_fit, HH, props)

	pred_sums <- winners$pred_sums
	pred_dat  <- winners$predictions

	pred_sums %>% print

	(pred_sums / HH) %>% print

	print(colSums(pred_sums) / HH)

	print(rowSums(pred_sums) / HH)
	print(sum(rowSums(pred_sums) / HH))
	print(unlist(props))

	save(winners, file = paste0(fit_dir, inst, save_suffix))

}


opt <- "nr"

ind_est <- read_dta(paste0(hng_data_dir, "individual_estimates_", opt, ".dta"))
load(paste0(hng_res_dir, "HNG_est.Rda"))

hng.df <- ind_est %>%
	select(sid, compare, starts_with("r_"), starts_with("mu_"), starts_with("gamma_"), starts_with("eta_"), starts_with("phi_"), starts_with("eut_"), starts_with("rdu") )

models <- c(eut = "EUT", rduP = "POW", rduI = "INV", rduPR = "PRE")
parameters <- list(r = "r", mu = "mu", gamma = "alpha", eta = "beta", phi = "alpha")
suffixes <- c(hi = "U95", low = "L95", pe = "Est", se = "SE")

for (m in names(models)) {
	for (p in names(parameters)) {
		for (s in names(suffixes)) {
			Gm <- m
			Gp <- p
			Gs <- s

			Bm <- models[[m]]
			Bp <- parameters[[p]]
			Bs <- suffixes[[s]]

			Gname <- paste0(Gp, "_", Gm, "_", Gs) 
			Bname <- paste0(Bm, "_", Bp, "_", Bs)

			if (! Gname %in% names(hng.df)) next

			hng.df[[Bname]] <- hng.df[[Gname]]
			hng.df[[Gname]] <- NULL

		}
	}
}

suffixes <- c(ll = "LL", eut = "pval", converged = "convcode")

for (m in names(models)) {
	for (s in names(suffixes)) {
		Gm <- m
		Gs <- s

		Bm <- models[[m]]
		Bs <- suffixes[[s]]

		Gname <- paste0(Gm, "_", Gs) 
		Bname <- paste0(Bm, "_", Bs)

		if (! Gname %in% names(hng.df)) next

		hng.df[[Bname]] <- hng.df[[Gname]]
		hng.df[[Gname]] <- NULL
	}
}


hng.df <- hng.df %>%
	select(sid, starts_with("EUT_", ignore.case = F), starts_with("POW_", ignore.case = F), starts_with("INV_", ignore.case = F), starts_with("PRE_", ignore.case = F)) 

hng.df$EUT_pval <- 1

hng.df$EUT_mu_SE  <- 0
hng.df$EUT_mu_L95 <- 0
hng.df$EUT_mu_U95 <- 0

hng.df$POW_mu_SE  <- 0
hng.df$POW_mu_L95 <- 0
hng.df$POW_mu_U95 <- 0

hng.df$INV_mu_SE  <- 0
hng.df$INV_mu_L95 <- 0
hng.df$INV_mu_U95 <- 0

hng.df$PRE_mu_SE  <- 0
hng.df$PRE_mu_L95 <- 0
hng.df$PRE_mu_U95 <- 0

# Need to reverse HNG's convcodes. 0 is good, 1 is not in my system, in their's its the opposite

for (m in c("EUT", "POW", "INV", "PRE")) {
	var <- paste0(m, "_convcode")

	hng.df[[var]]  <- ifelse(is.na(hng.df[[var]]), 3, hng.df[[var]])
	hng.df[[var]]  <- ifelse(hng.df[[var]] == 0, 2,  hng.df[[var]])
	hng.df[[var]]  <- ifelse(hng.df[[var]] == 1, 0,  hng.df[[var]])
	hng.df[[var]]  <- ifelse(hng.df[[var]] == 2, 1,  hng.df[[var]])
	hng.df[[var]]  <- ifelse(hng.df[[var]] == 3, NA, hng.df[[var]])

	# Drop non-converged observations
	hng.df[which((hng.df[[var]] > 1)), ] <- NA

}

# HNG drop observations that they consider to have "crazy" values
# Stata Code:
#	* non-monotonic inverse-S
#	replace rduI_converged=0 if gamma_rduI_pe < 0.28
#	replace rduP_converged=0 if gamma_rduP_pe > 5
#	replace rduI_converged=0 if gamma_rduI_pe > 5
#
#	* r too close to 1 or too large
#	foreach m in eut rduI rduP rduPR {
#		replace `m'_converged=0 if r_`m'_pe > 0.99 & r_`m'_pe < 1.01
#		replace `m'_converged=0 if r_`m'_pe > 15
#		replace `m'_converged=0 if r_`m'_pe < -15
#	}
#
#	* excessive eta
#	replace rduPR_converged=0 if eta_rduPR_pe > 20

for (m in c("EUT", "POW", "INV", "PRE")) {
	var <- paste0(m, "_r_Est")

	hng.df[which((hng.df[[var]] > 0.99) & (hng.df[[var]] < 1.01)), grep(m, names(hng.df)) ] <- NA
	hng.df[which(hng.df[[var]] < -15), grep(m, names(hng.df)) ] <- NA
	hng.df[which(hng.df[[var]] > 15), grep(m, names(hng.df)) ] <- NA

	sub.df[which((sub.df[[var]] > 0.99) & (sub.df[[var]] < 1.01)), grep(m, names(sub.df)) ] <- NA
	sub.df[which(sub.df[[var]] < -15), grep(m, names(sub.df))] <- NA
	sub.df[which(sub.df[[var]] > 15), grep(m, names(sub.df))] <- NA

}

hng.df[which(hng.df$INV_alpha_Est < 0.28), grep("INV", names(hng.df))] <- NA
hng.df[which(hng.df$INV_alpha_Est > 5.00), grep("INV", names(hng.df))] <- NA
hng.df[which(hng.df$POW_alpha_Est > 5.00), grep("POW", names(hng.df))] <- NA

# HNG don't do these two rules, but it isn't clear why not given the above limitations on crazy values
#hng.df[which(hng.df$PRE_alpha_Est > 5.00), grep("PRE", names(hng.df))] <- NA
#hng.df[which(hng.df$PRE_beta_Est > 5.00),  grep("PRE", names(hng.df))] <- NA

sub.df[which(sub.df$INV_alpha_Est < 0.28), grep("INV", names(sub.df))] <- NA
sub.df[which(sub.df$INV_alpha_Est > 5.00), grep("INV", names(sub.df))] <- NA
sub.df[which(sub.df$POW_alpha_Est > 5.00), grep("POW", names(sub.df))] <- NA

# HNG don't do these two rules, but it isn't clear why not given the above limitations on crazy values
#sub.df[which(sub.df$PRE_alpha_Est > 5.00), grep("PRE", names(sub.df))] <- NA
#sub.df[which(sub.df$PRE_beta_Est > 5.00),  grep("PRE", names(sub.df))] <- NA

# Get rid of all Power and Inverse S subjects
#hng.df[, grep("INV", names(hng.df))] <- NA
#hng.df[, grep("POW", names(hng.df))] <- NA
#
#sub.df[, grep("INV", names(sub.df))] <- NA
#sub.df[, grep("POW", names(sub.df))] <- NA

hng.df <- hng.df[1:111, order(colnames(hng.df))]
sub.df <- sub.df[,order(colnames(sub.df))]

hng.df <- ML.winner(hng.df)
sub.df <- ML.winner(sub.df)

save(hng.df, sub.df, sub.est, file = paste0(hng_res_dir, "filtered.Rda"))

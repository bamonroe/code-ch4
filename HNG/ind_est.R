library(dplyr)
hng_dir <- "../data/HNG/"
load(paste0(hng_dir, "ind_est.Rda"))
load("HNG_est.Rda")

DAT <- ind_est %>%
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

			if (! Gname %in% names(DAT)) next

			DAT[[Bname]] <- DAT[[Gname]]
			DAT[[Gname]] <- NULL
		
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

			if (! Gname %in% names(DAT)) next

			DAT[[Bname]] <- DAT[[Gname]]
			DAT[[Gname]] <- NULL
	}
}


DAT <- DAT %>%
	select(sid, starts_with("EUT_", ignore.case = F), starts_with("POW_", ignore.case = F), starts_with("INV_", ignore.case = F), starts_with("PRE_", ignore.case = F)) 

DAT$EUT_pval <- 1

DAT$EUT_mu_SE  <- 0
DAT$EUT_mu_L95 <- 0
DAT$EUT_mu_U95 <- 0

DAT$POW_mu_SE  <- 0
DAT$POW_mu_L95 <- 0
DAT$POW_mu_U95 <- 0

DAT$INV_mu_SE  <- 0
DAT$INV_mu_L95 <- 0
DAT$INV_mu_U95 <- 0

DAT$PRE_mu_SE  <- 0
DAT$PRE_mu_L95 <- 0
DAT$PRE_mu_U95 <- 0

DAT <- DAT[1:111, order(colnames(DAT))]
sub.df.t <- sub.df.t[,order(colnames(sub.df.t))]

models <- c(eut = "EUT", rduP = "POW", rduI = "INV", rduPR = "PRE")
parameters <- list(r = "r", gamma = "alpha", eta = "beta", phi = "alpha")
suffixes <- c(hi = "U95", low = "L95", pe = "Est", se = "SE")

DIFF <- list()

for (m in names(models)) {
	for (p in names(parameters)) {
		for (s in names(suffixes)) {

			Bm <- models[[m]]
			Bp <- parameters[[p]]
			Bs <- suffixes[[s]]

			Bname <- paste0(Bm, "_", Bp, "_", Bs)

			DIFF[[Bname]] <- sub.df.t[[Bname]] - DAT[[Bname]]
			DIFF[[Bname]] <- ifelse(is.na(sub.df.t[[Bname]]) & is.nan(DAT[[Bname]]), 0, DIFF[[Bname]])

		}
	}
}

DIFF <- do.call(cbind, DIFF)
DIFF



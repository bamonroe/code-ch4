mkwin <- function(inst) {
	cat(paste("Winner:", inst, "\n"))
	load_suffix <- "-bak.Rda"
	save_suffix <- "-bak.Rda"
	load(paste0(data_dir, inst, load_suffix))
	dat <- get(inst)

	# Apply the exclusionary rules
	for (mod in mods) {
		var <- paste0(mod, "_r_Est")
		dat[which((dat[[var]] > 0.99) & (dat[[var]] < 1.01)), grep(mod, names(dat)) ] <- NA
		dat[which(dat[[var]] < -15), grep(mod, names(dat)) ] <- NA
		dat[which(dat[[var]] > 15),  grep(mod, names(dat)) ] <- NA
	}
	dat[which(dat$PRE_beta_Est > 20.00), grep("PRE", names(dat))] <- NA

	# Pick the winner
	dat <- ML.winner(dat, models = mods)
	for (w in c("win_01", "win_05", "win_10")) {
		dat[[w]] <- ifelse(is.na(dat[[w]]), "NA", dat[[w]])
	}

	# Also pick a default Model
	dat$default <- "NA"
	for (def in defaults) {
		dat$default <- ifelse(!is.na(dat[[paste0(def, "_pval")]]), def, dat$default)
	}

	#print("win")
	#dat$win_05 %>% factor %>% summary %>% print
	#print("default")
	#dat$default %>% factor %>% summary %>% print
	#win <- dat %>% select(starts_with("win"), model)
	#win.p <- win %>% filter(model == "PRE") %>% apply(1, factor) %>% t
	#win.e <- win %>% filter(model == "EUT") %>% apply(1, factor) %>% t
	#print(paste("Display winners", inst))
	#print(summary(win.p))
	#print(summary(win.e))

	assign(inst, dat)

	cat(c("Winner - Save:", inst, "\n"))
	save(list = inst, file = paste0(data_dir, inst, save_suffix))
}

win2 <- function(inst) {
	cat(paste("Winner 2:", inst, "\n"))

	load_suffix <- "_est_hess.Rda"

	cat("  Loading", inst, "...\n")
	load(paste0(data_dir, inst, load_suffix))

	oname <- paste0(inst, "_est")

	dat <- get(oname)

	ss <- seq(from = 0.2, to = .7, length.out = 4)
	lb <- 1 - ss
	ub <- 1.3 + ss

	#expnum <- 9
	#lb <- c(lb, exp(hunif(expnum) * -1.2))
	#ub <- c(ub, exp(hunif(expnum) * 1.2))
	
	cat("  Calculating", inst, "...\n")
	tstat <- c.lapply(dat, test0, lb = lb, ub = ub)
	tstat <- do.call(rbind, tstat)

	tlen <- 1:nrow(tstat)

	astat <- tstat[which(tlen %% 2 == 1), ]
	bstat <- tstat[which(tlen %% 2 == 0), ]

	stats <- cbind(astat, bstat) %>% as.data.frame

	stats_names <- c(paste0("PRE_astat_", round(lb, 2), "_", round(ub, 2)),
	                  paste0("PRE_bstat_", round(lb, 2), "_", round(ub, 2)))

	colnames(stats) <- c(paste0("PRE_astat_", round(lb, 2), "_", round(ub, 2)),
	                  paste0("PRE_bstat_", round(lb, 2), "_", round(ub, 2)))

	new_names <- paste0("PRE_dstat_", round(lb, 2), "_", round(ub, 2))

	sname <- paste0(inst, "_stats")
	assign(sname, stats)

	rm(list = c(oname, "dat", "tstat", "astat", "bstat", "stats"))
	gc()

	save(list = c(sname, "new_names"), file = paste0(data_dir, inst, "_default_stats.Rda"))
}

win2_merge <- function(inst, stats) {
	#dbug <- 0
	#print(paste0("here", dbug)) ; dbug <- dbug + 1

	load_suffix <- "-bak.Rda"

	cat("  Loading", inst, "\n")
	load(paste0(data_dir, inst, load_suffix))
	load(paste0(data_dir, inst, "_default_stats.Rda"))

	sname <- paste0(inst, "_stats")
	dat   <- get(inst)
	stats <- get(sname)

	colnum <- ncol(stats)

	stats10  <- ifelse(stats > 0.10, T, F)
	stats10a <- stats10[, 1:(colnum/2)]
	stats10b <- stats10[, ((colnum/2)+1):colnum]
	stats10  <- stats10a | stats10b
	stats10  <- ifelse(stats10, "PRE", NA)

	stats05  <- ifelse(stats > 0.05, T, F)
	stats05a <- stats05[, 1:(colnum/2)]
	stats05b <- stats05[, ((colnum/2)+1):colnum]
	stats05  <- stats05a | stats05b
	stats05  <- ifelse(stats05, "PRE", NA)

	stats <- cbind(stats10, stats05)
	sname <- c(paste0(new_names, "_s10"), paste0(new_names, "_s05"))

	colnames(stats) <- sname

	save(sname, file = paste0(data_dir, "stat_names.Rda"))

	stats    <- stats[dat$ID, ]
	statsEUT <- cbind(dat$EUT_pval, stats)

	stats <- apply(statsEUT, 1, function(row) {
		pval <- row[1]
		tests <- row[2:length(row)]
		out <- ifelse(is.na(tests) & (!is.na(pval)), "EUT", tests)
		ifelse(is.na(out), "NA", out)
	}) %>% t()

	# Add Statse to the rest of the data
	dat[,sname] <- stats

	dat %>% select(starts_with("PRE_dstat")) %>% apply(2, factor) %>% summary %>% print

	assign(inst, dat)

	win_vars <- unique(c(win_vars, sname))

	assign("win_vars", win_vars, envir = .GlobalEnv)

	save(list = inst, file = paste0(data_dir, inst, load_suffix))
}

testit <- function(bvec2, bvec3, lb, ub) {

	blen <- length(bvec2)
	llen <- length(lb)

	lbt <- matrix(lb, nrow = blen, ncol = llen, byrow = T)
	ubt <- matrix(ub, nrow = blen, ncol = llen, byrow = T)

	at <- bvec2 < lbt | ubt < bvec2
	at <- colMeans(at)

	bt <- bvec3 < lbt | ubt < bvec3
	bt <- colMeans(bt)

	return(rbind(at,bt))

}

test0 <- function(sub, N = 1000, lb = 0.7, ub = 2.45) {
	PRE <- sub$PRE
	if (is.na(PRE[1])) return(matrix(NA, nrow = 2, ncol = length(lb)))
	
	est <- PRE$estimates
	hes <- PRE$hessian
	cov <- solve(-hes)

	boot <- mvhnorm(N, est, cov)

	lb <- log(lb)
	ub <- log(ub)

	test <- testit(boot[,2], boot[,3], lb = lb, ub = ub)

	return(test)
}



getEst <-  function(sub) {
	Inst <- sub %>%
		filter(Inst == "HNG")
	out <- lapply(multiples, function(m) {
		data <- Inst[1:(m*80),]
		opt <- "NR"
		# Initialize on the subject's real values
		init <- c(r = data$r[1], alpha = data$alpha[1], beta = data$beta[1], mu = data$mu[1])
		init <- ifelse(is.na(init), 1, init)
		EST <- list()
		EST[["EUT"]]  <- tryCatch(ML.optim(init[c(1,4)], inst = data, pfunc = c("eut"), optimizer = opt, iterations = 150, report = F), error = function(e) {NA})
		EST[["PRE"]]  <- tryCatch(ML.optim(init[c(1,2,3,4)], inst = data, pfunc = c("prelec"), optimizer = opt, iterations = 150, report = F), error = function(e) {NA})
		return(EST)
	})
	mnames <- paste0("HNG_", multiples)
	names(out) <- mnames
	return(out)
}

perFile <- function(fname) {
	print(fname)
	suffix <- sub(".*-", "", fname)
	save_file <- paste0(new_est_dir, "/sub_est_new-", suffix)
	if (file.exists(save_file)) return()
	e <- tryCatch(load(fname), error = function(e) {NA})
	if (is.na(e)) return()

	load(fname)
	sub.est <- c.lapplyLB(persub, getEst)

	real <- lapply(persub, function(x) {
		r     <- x$r[1]
		mu    <- x$mu[1]
		alpha <- x$alpha[1]
		beta  <- x$beta[1]
		mod   <- x$pfunc[1]
		if (mod == "EUT")    mod <- 1
		if (mod == "prelec") mod <- 4
		c(r = r, alpha = alpha, beta = beta, mu = mu, model = mod)
	})
	real <- do.call(rbind, real)
	save(real, sub.est, file = save_file)
}

rr <- c(10, 2:6)

for (i in rr) {
	fpat <- paste0("HNG-subdat-", i)
	files <- list.files(path = new_rawdat_dir, pattern = fpat, full.names = T)
	null <- lapply(files, perFile)

	print(warnings())
}


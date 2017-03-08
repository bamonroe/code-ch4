# Start estimator
library(ctools)
c.library("MSL")

load("subdat.Rda")

sink("estimates_error.Rout")

# Which optimizers
opt <- "CG"
opt <- "Nelder-Mead"
opt <- "DE"
opt <- "BHHH"
opt <- "BFGS"
opt <- "NR"

# Do the estimation for each model
sub.est <- c.lapply(persub, function(sub, opt) {

	insts <- split(sub, sub$Inst)
	# We don't estimate on the ins
	insts$HNG.ins <- NULL
	insts$HNG_1 <- insts$HNG[1:80,]

	lapply(insts, function(data, opt) {
		# Initialize on the subject's real values
		init <- c(r = data$r[1], alpha = data$alpha[1], beta = data$beta[1], mu = data$mu[1])
		init <- ifelse(is.na(init), 1, init)
		EST <- list()
		# There are a lot of error messages printed to the colsole instead of in the log file
		# because of failures to converge. Sink should hopefully put these into a file
		EST[["EUT"]]  <- tryCatch(ML.optim(init[c(1,4)], inst = data, pfunc = c("eut"), optimizer = opt, report = F), error = function(e) {NA})
		EST[["POW"]]  <- tryCatch(ML.optim(init[c(1,2,4)], inst = data, pfunc = c("pow"), optimizer = opt, report = F), error = function(e) {NA})
		EST[["INVS"]] <- tryCatch(ML.optim(init[c(1,2,4)], inst = data, pfunc = c("invs"), optimizer = opt, report = F), error = function(e) {NA})
		EST[["PRE"]]  <- tryCatch(ML.optim(init[c(1,2,3,4)], inst = data, pfunc = c("prelec"), optimizer = opt, report = F), error = function(e) {NA})
		# Stop sinking things
		return(EST)
	}, opt = opt)
}, opt = opt)

models <- lapply(persub, function(x) {x$pfunc[1]})

real <- lapply(persub, function(x) {
	r     <- x$r[1]
	mu    <- x$mu[1]
	alpha <- x$alpha[1]
	beta  <- x$beta[1]
	mod   <- x$pfunc[1]

	if (mod == "EUT")    mod <- 1
	if (mod == "pow")    mod <- 2
	if (mod == "invs")   mod <- 3
	if (mod == "prelec") mod <- 4

	c(r = r, alpha = alpha, beta = beta, mu = mu, model = mod)
})
real <- do.call(rbind, real)

save(real, sub.est, file = "sub_est.Rda")


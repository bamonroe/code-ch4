# Gather the relevent stats into a results dataframe
load("sub_est.Rda")

getres <- function(sub.inst) {

# How many subjects
N <- length(sub.inst)
# Names of the models
m.names <- c("eut", "pow", "inv", "pre")

# Make a dataframe for results from all subjects
results <- data.frame(real)
results$model <- as.character(results$model)
results$model <- ifelse(results$model == "1", "EUT",
                 ifelse(results$model == "2", "pow",
                 ifelse(results$model == "3", "invs",
                 ifelse(results$model == "4", "prelec", "NA"))))

# Some basic descripive stuff
parsed <- lapply(sub.inst, function(x) {
	lapply(x, function(y){
		if (is.na(y[1])) {
			return(NA)
		} else {
			return(cbind(y$parsed, LL = y$likelihood))
		}
	})
})

# get estimates into the results parsed
rescol <- lapply(parsed, function(x) {
	narow <- matrix(rep(NA, 4), ncol = 4)
	if (!is.na(x$EUT[1])) {
		eut <- c(x$EUT[1,1:4],  x$EUT[2,1:4], x$EUT[1,5])
	} else {
		eut <- c(narow, narow, NA)
	}
	if (!is.na(x$POW[1])) {
		pow <- c(x$POW[1,1:4],  x$POW[2,1:4],  x$POW[3,1:4], x$POW[1,5])
	} else {
		pow <- c(narow, narow, narow, NA)
	}
	if (!is.na(x$INVS[1])) {
		inv <- c(x$INVS[1,1:4], x$INVS[2,1:4], x$INVS[3,1:4], x$INVS[1,5])
	} else {
		inv <- c(narow, narow, narow, NA)
	}
	if (!is.na(x$PRE[1])) {
		pre <- c(x$PRE[1,1:4],  x$PRE[2,1:4],  x$PRE[3,1:4], x$PRE[4,1:4], x$PRE[1,5])
	} else {
		pre <- c(narow, narow, narow, narow, NA)
	}
	cnames <- c("Est", "SE", "L95", "U95")
	rnames     <- paste0("r.", cnames)
	munames    <- paste0("mu.", cnames)
	alphanames <- paste0("alpha.", cnames)
	betanames  <- paste0("beta.", cnames)

	names(eut) <- c(paste0("eut.",rnames), paste0("eut.", munames), "eut.LL")
	names(pow) <- c(paste0("pow.",rnames), paste0("pow.", munames), paste0("pow.", alphanames), "pow.LL")
	names(inv) <- c(paste0("inv.",rnames), paste0("inv.", munames), paste0("inv.", alphanames), "inv.LL")
	names(pre) <- c(paste0("pre.",rnames), paste0("pre.", munames), paste0("pre.", alphanames), paste0("pre.", betanames), "pre.LL")

	out <- c(eut, pow, inv, pre)

})
rescol <- do.call(rbind, rescol)

# Filter out results that haven't converged
# convcode 0 - small gradient convergence
# convcode 1 - successive iterations within tolerance limit
# convcode 2 - iteration limit reached
ccode <- 1
converged <- lapply(sub.inst, function(x, ccode) {
	mod <- lapply(x, function(y, ccode) {
		if (is.na(y[1])) {
			out <- NA
		} else if ((y$convcode > ccode)) {
			out <- NA
		} else {
			out <- y
		}
	}, ccode = ccode)
}, ccode = ccode)

concode <- lapply(sub.inst, function(x) {
	mod <- lapply(x, function(y) {
		if (is.na(y[1])) {
			out <- NA
		} else {
			out <- y$convcode
		}
	})
	do.call(cbind, mod)
})
concode <- do.call(rbind, concode)
colnames(concode) <- paste0(m.names, ".concode")


# Grab eut pvalue and log liklihood for every model
is.eut <- lapply(converged, function(x) {
	pl <- lapply(x, function(y) {
		if (!is.na(y[1])) {
			pval <- y$eut.p
			ll   <- y$likelihood
			out <- c(pval, ll)
			names(out) <- c("pval", "ll")
		} else {
			out <- NA
		}
		return(out)
	})
	pl
})

pvals <- lapply(is.eut, function(x) {
	out <- lapply(x, function(y) {
		if (!is.na(y[1])) {
			y[1]
		} else {
			NA
		}
	})
	do.call(cbind, out)
})
pvals <- do.call(rbind, pvals)
colnames(pvals) <- paste0(m.names, ".pval")

# Which model won the comparison. RDU models need to be confirmed as
# not EUT, and then those models + EUT have log-likelihoods compared
# whichever model has the greatest log-likelihood is the winner
winners <- lapply(is.eut, function(x) {
	# Test each model at a variety of significance levels
	sig <- lapply(x, function(y) {
		if (!is.na(y[1])) {
			s10 <- ifelse( y[1] < 0.1,  1, 0)
			s5  <- ifelse( y[1] < 0.05, 1, 0)
			s1  <- ifelse( y[1] < 0.01, 1, 0)
			out <- c(s10, s5, s1)
		} else {
			out <- rep(NA, 3)
		}
		names(out) <- c("s10", "s5", "s1")
		return(out)
	})
	# Matrix of significance by model, (row = mod, col = sig)
	sig <- do.call(rbind, sig)
	#EUT ROW, we're always considering EUT
	sig[1,] <- 1

	# Get the likelihoods, use snum in case I want to add some more significance tests
	ll <- lapply(x, function(y, snum) {
		if (!is.na(y[1])) {
			# The likelihood is in y[2]
			out <- rep(y[2], snum)
		} else {
			out <- rep(NA, snum)
		}
		return(out)
	}, snum = ncol(sig) )
	# Matrix of likelihoods
	ll   <- do.call(rbind, ll)
	# Matrix of the whole thing bound by rows, we'll compare one significance level
	# at a time
	full <- rbind(sig,ll)

	winner <- apply(full, 2, function(x) {
		sig <- x[1:(length(x)/2)]
		ll  <- x[((length(x)/2) + 1):length(x)]
		# Check for models that differ from EUT significantly and also EUT
		# All who pass get into the winner's club
		winner <- ifelse(sig == 1, ll, NA)
		# Rank the winners club on log likelihood
		out <- order(winner, decreasing = T, na.last=T)[1]
		# If our "out" is NA, it means that EUT didn't converge AND none of the RDU models
		# were significantly different from EUT. So now we grab whatever model has the highest 
		# LL, which won't be EUT.
		if (is.na(winner[out])) {
			# If all models show NA, pick on maximum ll, failing that, return NA
			out <- order(ll, decreasing = T, na.last=T)[1]
			if (is.na(ll[out])) out <- NA
		}
		return(out)
	})
	return(winner)
})
# Bind the list, and sum up the winners
winmat <- do.call(rbind, winners)
colnames(winmat) <- c("win.s10", "win.s05", "win.s01")

results <- cbind(rescol, pvals, winmat, concode, results)

}

HNG   <- lapply(sub.est, function(sub) {sub$HNG})
HNG_1 <- lapply(sub.est, function(sub) {sub$HNG_1})
SH    <- lapply(sub.est, function(sub) {sub$SH})
HO    <- lapply(sub.est, function(sub) {sub$HO})
LMS20 <- lapply(sub.est, function(sub) {sub$LMS20})
LMS30 <- lapply(sub.est, function(sub) {sub$LMS30})

HNG.res   <- getres(HNG)
HNG_1.res <- getres(HNG_1)
SH.res    <- getres(SH   )
HO.res    <- getres(HO   )
LMS20.res <- getres(LMS20)
LMS30.res <- getres(LMS30)

save(HNG.res,   file = "HNG_res.Rda")
save(HNG_1.res, file = "HNG_1_res.Rda")
save(SH.res,    file = "SH_res.Rda")
save(HO.res,    file = "HO_res.Rda")
save(LMS20.res, file = "LMS20_res.Rda")
save(LMS30.res, file = "LMS30_res.Rda")


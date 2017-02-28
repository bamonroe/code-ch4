library(ctools)
c.library("dplyr", "ggplot2")

run.wel <- T
wloc  <- "../data/classify/weldat"
wcalc <- "../data/classify/wcalced"
files <- list.files(wloc, pattern="subdat*")

crra <- function(x,r) {x^(1-r)/(1-r)}

welfun <- function(CEA, CEB, choice) {


	CEM <- ifelse(CEA > CEB, CEA, CEB)
	CE0 <- ifelse(choice == 0, CEA, CEB)
	CE1 <- ifelse(choice == 1, CEA, CEB)

	w.m <- sum(CE0 - CEM)
	w.a <- sum(CE0 - CE1)
	out <- c(wm = w.m, wa = w.a)
	out

}
welfun <- compiler::cmpfun(welfun)

r.eut <- function(dat) {
	r     <- dat$r[1]
	alpha <- dat$alpha[1]
	beta  <- dat$beta[1]
	mu    <- dat$mu[1]

	UA <- dat$pA0*crra(dat$A0, r) + dat$pA1*crra(dat$A1, r) + dat$pA2*crra(dat$A2, r)
	UB <- dat$pB0*crra(dat$B0, r) + dat$pB1*crra(dat$B1, r) + dat$pB2*crra(dat$B2, r)

	CEA <- (UA*(1-r))^(1/(1-r))
	CEB <- (UB*(1-r))^(1/(1-r))

	out <- c(welfun(UA, UB, dat$choice), r = r, alpha = alpha, beta = beta, mu = mu)

	out
}
r.eut <- compiler::cmpfun(r.eut)

r.pow <- function(dat) {
	r     <- dat$r[1]
	alpha <- dat$alpha[1]
	beta  <- dat$beta[1]
	mu    <- dat$mu[1]

	apA0 <- dat$pA0 + dat$pA1 + dat$pA2
	apA1 <- dat$pA1 + dat$pA2
	apA2 <- dat$pA2

	apB0 <- dat$pB0 + dat$pB1 + dat$pB2
	apB1 <- dat$pB1 + dat$pB2
	apB2 <- dat$pB2

	pwpA2 <- apA2^dat$alpha
	pwpA1 <- apA1^dat$alpha
	pwpA0 <- apA0^dat$alpha

	pwpB2 <- apB2^dat$alpha
	pwpB1 <- apB1^dat$alpha
	pwpB0 <- apB0^dat$alpha

	dwpA2 <- pwpA2
	dwpA1 <- pwpA1 - pwpA2
	dwpA0 <- pwpA0 - pwpA1 - pwpA2

	dwpB2 <- pwpB2
	dwpB1 <- pwpB1 - pwpB2
	dwpB0 <- pwpB0 - pwpB1 - pwpB2

	UA <- dat$pA0*crra(dat$A0, r) + dat$pA1*crra(dat$A1, r) + dat$pA2*crra(dat$A2, r)
	UB <- dat$pB0*crra(dat$B0, r) + dat$pB1*crra(dat$B1, r) + dat$pB2*crra(dat$B2, r)

	CEA <- (UA*(1-r))^(1/(1-r))
	CEB <- (UB*(1-r))^(1/(1-r))

	out <- c(welfun(UA, UB, dat$choice), r = r, alpha = alpha, beta = beta, mu = mu)
	out

}
r.pow <- compiler::cmpfun(r.pow)

r.inv <- function(dat) {
	r     <- dat$r[1]
	alpha <- dat$alpha[1]
	beta  <- dat$beta[1]
	mu    <- dat$mu[1]

	apA0 <- dat$pA0 + dat$pA1 + dat$pA2
	apA1 <- dat$pA1 + dat$pA2
	apA2 <- dat$pA2

	apB0 <- dat$pB0 + dat$pB1 + dat$pB2
	apB1 <- dat$pB1 + dat$pB2
	apB2 <- dat$pB2

	pwpA0 <- (apA0^alpha) / (((apA0^alpha) + ((1-apA0)^alpha))^(1/alpha));
	pwpA1 <- (apA1^alpha) / (((apA1^alpha) + ((1-apA1)^alpha))^(1/alpha));
	pwpA2 <- (apA2^alpha) / (((apA2^alpha) + ((1-apA2)^alpha))^(1/alpha));

	pwpB0 <- (apB0^alpha) / (((apB0^alpha) + ((1-apB0)^alpha))^(1/alpha));
	pwpB1 <- (apB1^alpha) / (((apB1^alpha) + ((1-apB1)^alpha))^(1/alpha));
	pwpB2 <- (apB2^alpha) / (((apB2^alpha) + ((1-apB2)^alpha))^(1/alpha));

	dwpA2 <- pwpA2
	dwpA1 <- pwpA1 - pwpA2
	dwpA0 <- pwpA0 - pwpA1 - pwpA2

	dwpB2 <- pwpB2
	dwpB1 <- pwpB1 - pwpB2
	dwpB0 <- pwpB0 - pwpB1 - pwpB2

	UA <- dat$pA0*crra(dat$A0, r) + dat$pA1*crra(dat$A1, r) + dat$pA2*crra(dat$A2, r)
	UB <- dat$pB0*crra(dat$B0, r) + dat$pB1*crra(dat$B1, r) + dat$pB2*crra(dat$B2, r)

	CEA <- (UA*(1-r))^(1/(1-r))
	CEB <- (UB*(1-r))^(1/(1-r))

	out <- c(welfun(UA, UB, dat$choice), r = r, alpha = alpha, beta = beta, mu = mu)
	out

}
r.inv <- compiler::cmpfun(r.inv)

r.pre <- function(dat) {
	r     <- dat$r[1]
	alpha <- dat$alpha[1]
	beta  <- dat$beta[1]
	mu    <- dat$mu[1]

	apA0 <- dat$pA0 + dat$pA1 + dat$pA2
	apA1 <- dat$pA1 + dat$pA2
	apA2 <- dat$pA2

	apB0 <- dat$pB0 + dat$pB1 + dat$pB2
	apB1 <- dat$pB1 + dat$pB2
	apB2 <- dat$pB2

	pwpA0  = exp(-beta * ((-log(apA0))^alpha))
	pwpA1  = exp(-beta * ((-log(apA1))^alpha))
	pwpA2  = exp(-beta * ((-log(apA2))^alpha))
	pwpB0  = exp(-beta * ((-log(apB0))^alpha))
	pwpB1  = exp(-beta * ((-log(apB1))^alpha))
	pwpB2  = exp(-beta * ((-log(apB2))^alpha))

	pwpA0 <- ifelse(is.nan(pwpA0), 0, pwpA0)
	pwpA1 <- ifelse(is.nan(pwpA1), 0, pwpA1)
	pwpA2 <- ifelse(is.nan(pwpA2), 0, pwpA2)

	pwpB0 <- ifelse(is.nan(pwpB0), 0, pwpB0)
	pwpB1 <- ifelse(is.nan(pwpB1), 0, pwpB1)
	pwpB2 <- ifelse(is.nan(pwpB2), 0, pwpB2)

	dwpA2 <- pwpA2
	dwpA1 <- pwpA1 - pwpA2
	dwpA0 <- pwpA0 - pwpA1 - pwpA2

	dwpB2 <- pwpB2
	dwpB1 <- pwpB1 - pwpB2
	dwpB0 <- pwpB0 - pwpB1 - pwpB2

	UA <- dat$pA0*crra(dat$A0, r) + dat$pA1*crra(dat$A1, r) + dat$pA2*crra(dat$A2, r)
	UB <- dat$pB0*crra(dat$B0, r) + dat$pB1*crra(dat$B1, r) + dat$pB2*crra(dat$B2, r)

	CEA <- (UA*(1-r))^(1/(1-r))
	CEB <- (UB*(1-r))^(1/(1-r))

	out <- c(welfun(UA, UB, dat$choice), r = r, alpha = alpha, beta = beta, mu = mu)
	out

}
r.pre <- compiler::cmpfun(r.pre)

c.export("crra", "r.eut", "r.pow", "r.inv", "r.pow")

if (run.wel) {
	wel <- c.lapply(files, function(f) {
		print(f)
		load(paste0(wloc, "/", f))
		welres <- lapply(weldat, function(dat) {
			if (dat$pfunc == "EUT")           {
				out <- unlist(c(mod = 1, r.eut(dat)))
			} else if (dat$pfunc == "pow")    {
				out <- unlist(c(mod = 2, r.pow(dat)))
			} else if (dat$pfunc == "invs")   {
				out <- unlist(c(mod = 3, r.inv(dat)))
			} else if (dat$pfunc == "prelec") {
				out <- unlist(c(mod = 4, r.pre(dat)))
			}
		})
		welres <- do.call(rbind, welres)
		save(welres, file = paste0(wcalc, "/wel-", f))

		return(welres)
	})
	wel <- do.call(rbind, wel) %>%
		data.frame() %>%
		sample_frac(0.05)
	colnames(wel) <- c("mod", "wel.M", "wel.A", "r", "alpha", "beta", "mu")
	save(wel, file = "wel.Rda")
} else {
	load("wel.Rda")
}

wel$wel.M <- as.numeric(wel$wel.M)
wel$wel.A <- as.numeric(wel$wel.A)
wel$mod <- factor(wel$mod, levels = 1:4, labels = c("EUT", "pow", "invs", "prelec"))

wel$wel  <- wel$wel.M
wel$type <- 1

wel1 <- wel
wel2 <- wel

wel1$wel <- wel1$wel.M
wel1$type <- 1

wel2$wel <- wel2$wel.A
wel2$type <- 2

wel1 <- rbind(wel1,wel2)

wel1$type <- factor(wel1$type, levels = 1:2, labels = c("Obtained - Max", "Obtained - Alternative"))

# Plot it
p <- ggplot(wel1, aes(x = mu, y = wel))
p <- p + facet_grid(type~mod, scales = "free_y", switch = "both")
p <- p + geom_smooth(method = "loess", span = 0.25)

# configure plot
height <- 6.6
width <- height * 16 / 9

ggsave("wel.pdf", plot = p, width = width, height = height, units = "in")


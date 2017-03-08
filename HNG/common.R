library(dplyr)
hng_res_dir <- "../data/HNG_res/"
load(paste0(hng_res_dir, "filtered.Rda"))

# What are the convergence statistics for both of us
dat.eut <- ifelse(!is.na(hng.df$EUT_LL), 1, 0)
dat.pow <- ifelse(!is.na(hng.df$POW_LL), 1, 0)
dat.inv <- ifelse(!is.na(hng.df$INV_LL), 1, 0)
dat.pre <- ifelse(!is.na(hng.df$PRE_LL), 1, 0)

counts0 <- data.frame(dat.eut, dat.pow, dat.inv, dat.pre)
colnames(counts0) <- c("EUT", "POW", "INV", "PRE")

sub.eut <- ifelse(!is.na(sub.df$EUT_LL), 1, 0)
sub.pow <- ifelse(!is.na(sub.df$POW_LL), 1, 0)
sub.inv <- ifelse(!is.na(sub.df$INV_LL), 1, 0)
sub.pre <- ifelse(!is.na(sub.df$PRE_LL), 1, 0)

counts1 <- data.frame(sub.eut, sub.pow, sub.inv, sub.pre)
colnames(counts1) <- c("EUT", "POW", "INV", "PRE")

# counts is a little 4x2 matrix totalling up converged model numbers - I have more
counts <- rbind(colSums(counts0), colSums(counts1))
rownames(counts) <- c("HN", "Monroe")

same <- ifelse(counts0 == counts1, 1, 0)
same.sum <- colSums(same)

dat.has <- ifelse((counts0 == 1) & (counts1 == 0), 1, 0)
dat.has.sum <- colSums(dat.has)

sub.has <- ifelse((counts1 == 1) & (counts0 == 0), 1, 0)
sub.has.sum <- colSums(sub.has)

# How many winners have we declared

dat.winners <- ifelse(!is.na(hng.df$win_05), 1, 0) %>% sum(na.rm = T)
sub.winners <- ifelse(!is.na(sub.df$win_05), 1, 0) %>% sum(na.rm = T)

dat.eut.win <- ifelse(hng.df$win_05 == "EUT", 1, 0) %>% sum(na.rm = T)
dat.pow.win <- ifelse(hng.df$win_05 == "POW", 1, 0) %>% sum(na.rm = T)
dat.inv.win <- ifelse(hng.df$win_05 == "INV", 1, 0) %>% sum(na.rm = T)
dat.pre.win <- ifelse(hng.df$win_05 == "PRE", 1, 0) %>% sum(na.rm = T)

dat.winners <- cbind(dat.eut.win, dat.pow.win, dat.inv.win, dat.pre.win)

sub.eut.win <- ifelse(sub.df$win_05 == "EUT", 1, 0) %>% sum(na.rm = T)
sub.pow.win <- ifelse(sub.df$win_05 == "POW", 1, 0) %>% sum(na.rm = T)
sub.inv.win <- ifelse(sub.df$win_05 == "INV", 1, 0) %>% sum(na.rm = T)
sub.pre.win <- ifelse(sub.df$win_05 == "PRE", 1, 0) %>% sum(na.rm = T)

sub.winners <- cbind(sub.eut.win, sub.pow.win, sub.inv.win, sub.pre.win)

winners <- rbind(dat.winners, sub.winners)
winners <- cbind(winners, rowSums(winners))

colnames(winners) <- c("EUT", "POW", "INV", "PRE", "Total")
rownames(winners) <- c("HN", "Monroe")


## What are the differences between our estimations

models <- c(eut = "EUT", rduP = "POW", rduI = "INV", rduPR = "PRE")
parameters <- list(r = "r", gamma = "alpha", eta = "beta", phi = "alpha")
suffixes <- c(hi = "U95", low = "L95", pe = "Est", se = "SE", pval = "pval")

DIFF <- list()
for (m in names(models)) {
	for (p in names(parameters)) {
		for (s in names(suffixes)) {

			Bm <- models[[m]]
			Bp <- parameters[[p]]
			Bs <- suffixes[[s]]

			Bname <- paste0(Bm, "_", Bp, "_", Bs)

			if( ! Bname %in% names(sub.df)) {
				Bname <- paste0(Bm, "_", Bs)
				if( ! Bname %in% names(sub.df)) next
			
			}

			DIFF[[Bname]] <- sub.df[[Bname]] - hng.df[[Bname]]
			DIFF[[Bname]] <- ifelse(is.na(sub.df[[Bname]]) & is.nan(hng.df[[Bname]]), NA, DIFF[[Bname]])

		}
	}
}

DIFF <- do.call(cbind, DIFF)
DIFF <- data.frame(DIFF)
# I actually care about absolute differences
DIFF <- abs(DIFF)

# Some metrics
d.sum <- apply(DIFF, 2, summary)
d.sum <- data.frame(d.sum)

# Grab the interesting results
d.interesting <- d.sum %>%
	select(ends_with("Est"), ends_with("SE"), ends_with("pval"))

d.eut <- d.interesting %>%
	select(starts_with("EUT")) %>%
	t()

d.pow <- d.interesting %>%
	select(starts_with("POW")) %>%
	t()

d.inv <- d.interesting %>%
	select(starts_with("INV")) %>%
	t()

d.pre <- d.interesting %>%
	select(starts_with("PRE")) %>%
	t()


View(d.interesting)

vname <- "INV_alpha_Est"

big.five <- lapply(names(d.interesting), function(vname) {
	var  <- DIFF[[vname]]
	oo   <- order(var)
	od   <- var[oo]
	od   <- od[which(!is.na(od))]
	view <- length(od)

	last <- 20

	view <- (view-(last-1)):view
	dat  <- hng.df[[vname]][oo[view]]
	sub  <- sub.df[[vname]][oo[view]]
	rbind(dat, sub)
})

big.diff <- lapply(names(d.interesting), function(vname) {
	var  <- DIFF[[vname]]
	oo   <- order(var)
	od   <- var[oo]
	od   <- od[which(!is.na(od))]
	view <- length(od)

	last <- 20

	view <- (view-(last-1)):view
	out  <- DIFF[[vname]][oo[view]]
	out
})

names(big.five) <- names(d.interesting)
names(big.diff) <- names(d.interesting)

big.five

round(same.sum, 2)

common <- rbind(counts, same.sum, dat.has.sum, sub.has.sum)
common <- data.frame(common)
rownames(common) <- c("HN Converged Models", "Monroe Converged Models", "Number Subjects Same Convergence", "HN Converged but not Monroe", "Monroe Converged but not HN")

d.eut
d.pow
d.inv
d.pre

winners
winners[,1:4] / winners[,5]
common

save(big.five, common, d.eut, d.pow, d.inv, d.pre, file = paste0(hng_res_dir, "common.Rda"))


library(dplyr)
library(ggplot2)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
fit_dir    <- "../data/lo_fits/"
load_suffix <- "-win.Rda"

for (inst in instruments) {

	load(paste0(fit_dir, inst, load_suffix))

	EUT_dat <- winners$predictions[[1]]
	POW_dat <- winners$predictions[[2]]
	INV_dat <- winners$predictions[[3]]
	PRE_dat <- winners$predictions[[4]]

	p <- list()
	p[["EUT"]] <- ggplot(EUT_dat, aes(x = r, y = EUT_EUT)) + 
	              geom_point(alpha = 0.35) + geom_smooth()

	p[["POW"]] <- ggplot(POW_dat, aes(x = alpha, y = POW_POW)) + 
	              geom_point(alpha = 0.35) + geom_smooth()

	#p[["INV"]] <- ggplot(INV_dat, aes(x = alpha, y = INV_INV)) + 
	p[["INV"]] <- ggplot(INV_dat, aes(x = alpha, y = INV_INV)) + 
	              geom_point(alpha = 0.35) + geom_smooth()

	p[["PRE"]] <- ggplot(PRE_dat, aes(x = alpha, y = PRE_PRE)) + 
	              geom_point(alpha = 0.35) + geom_smooth()

}
p


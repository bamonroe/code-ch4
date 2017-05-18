plot_fits <- function(inst) {

	load_suffix <- "-win.Rda"

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

c.lapply(plot_fits, insts)

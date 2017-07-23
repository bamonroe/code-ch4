mkmini_1 <- function(inst, mods) {
	cat(paste("Mini1:", inst, "\n"))

	load_suffix <- "-bak.Rda"
	save_suffix <- "-bak.Rda"

	load(paste0(data_dir, inst, load_suffix))

	dat <- get(inst)

	# Throw an ID variable in there
	dat$ID <- 1:nrow(dat)

	# Reduce the dataset to only include models we care about, given by pop_mods, and sample as necessary
	dat0 <- lapply(pop_mods, function(mod) {
		dat %>%
			filter(model == mod) %>%
			sample_frac(mini_frac)
	})

	dat <- do.call(rbind, dat0)

	assign(inst, dat)

	cat(c("Mini1 - Save:", inst, "\n"))
	save(list = inst, file = paste0(data_dir, inst, save_suffix))
}

mkmini_2 <- function(inst, mods, wel_var) {
	cat(paste("Mini2:", inst, "\n"))
	load_suffix <- "-bak.Rda"
	save_suffix <- "-bak.Rda"
	load(paste0(data_dir, inst, load_suffix))

	dat <- get(inst)
	cn  <- colnames(dat)

	#keep_vars <- c(win_vars, grep("^real", cn, value = T), grep(paste0(".*", wel_var, "$"), cn, value = T), "r", "mu", "alpha", "beta", "model", "ID")
	keep_vars <- c(win_vars, grep("^real", cn, value = T), grep(paste0(".*", wel_var, "$"), cn, value = T), "r", "mu", "alpha", "beta", "model")
	keep_vars <- unique(keep_vars)

	dat <- dat[,keep_vars]

#	dat <- dat %>%
#		select(matches(paste0(win_vars, collapse = "|")), default, starts_with("real"), ends_with(wel_var), r, mu, alpha, beta, model, ID)
		#select(starts_with("win"), default, starts_with("real"), ends_with(wel_var), r, mu, alpha, beta, model)

	assign(inst, dat)

	cat(c("Mini2 - Save:", inst, "\n"))
	save(list = inst, file = paste0(data_dir, inst, save_suffix))
}

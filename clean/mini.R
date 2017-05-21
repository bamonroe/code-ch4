mkmini_1 <- function(inst, mods) {
	cat(paste("Mini1:", inst, "\n"))

	load_suffix <- "-bak.Rda"
	save_suffix <- "-bak.Rda"

	load(paste0(data_dir, inst, load_suffix))

	dat <- get(inst)

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

	dat <- dat %>%
		select(starts_with("win"), default, starts_with("real"), ends_with(wel_var), r, mu, alpha, beta, model)

	assign(inst, dat)

	cat(c("Mini2 - Save:", inst, "\n"))
	save(list = inst, file = paste0(data_dir, inst, save_suffix))
}

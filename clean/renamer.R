mkbak <- function(inst) {
	cat(c("Rename:", inst, "\n"))

	load_suffix <- ".Rda"
	save_suffix <- "-bak.Rda"

	load(paste0(data_dir, inst, ".Rda"))

	dat <- get(inst)

	varnames <- names(dat)

	nnames <- c()

	# Replace variables that have periods with underscores
	for(var in varnames) {
		if (length(grep("\\.", var)) == 1) {
			nvar <- gsub("\\.", "_", var)
		} else {
			nvar <- var
		}
		nnames <- c(nnames, nvar)
	}

	# Make the names uppercase
	for (mod in mods) {
		position <- grep(tolower(mod), nnames)
		rep_name <- sub(tolower(mod), mod, nnames[position])
		nnames[position] <- rep_name
	}
	colnames(dat) <- nnames

	# Just use the same things everywhere
	dat$model <- ifelse(dat$model == "pow", "POW", dat$model)
	dat$model <- ifelse(dat$model == "invs", "INV", dat$model)
	dat$model <- ifelse(dat$model == "prelec", "PRE", dat$model)

	assign(inst, dat)

	cat(c("Rename - Save:", inst, "\n"))
	save(list=inst, file = paste0(data_dir, inst, save_suffix))
}


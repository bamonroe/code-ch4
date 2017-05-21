mkwin <- function(inst){
	cat(paste("Winner:", inst, "\n"))

	load_suffix <- "-bak.Rda"
	save_suffix <- "-bak.Rda"

	load(paste0(data_dir, inst, load_suffix))

	dat <- get(inst)

	# Apply the exclusionary rules
	for (mod in mods) {
		var <- paste0(mod, "_r_Est")
		dat[which((dat[[var]] > 0.99) & (dat[[var]] < 1.01)), grep(mod, names(dat)) ] <- NA
		dat[which(dat[[var]] < -15), grep(mod, names(dat)) ] <- NA
		dat[which(dat[[var]] > 15),  grep(mod, names(dat)) ] <- NA
	}
	dat[which(dat$PRE_beta_Est > 20.00), grep("PRE", names(dat))] <- NA

	# Pick the winner
	dat <- ML.winner(dat, models = mods)
	for (w in c("win_01", "win_05", "win_10")) {
		dat[[w]] <- ifelse(is.na(dat[[w]]), "NA", dat[[w]])
	}

	# Also pick a default Model
	dat$default <- "NA"
	for (def in defaults) {
		dat$default <- ifelse(!is.na(dat[[paste0(def, "_pval")]]), def, dat$default)
	}

#	print("win")
#	dat$win_05 %>% factor %>% summary %>% print
#	print("default")
#	dat$default %>% factor %>% summary %>% print

	#win <- dat %>% select(starts_with("win"), model)
	#win.p <- win %>% filter(model == "PRE") %>% apply(1, factor) %>% t
	#win.e <- win %>% filter(model == "EUT") %>% apply(1, factor) %>% t
	#print(paste("Display winners", inst))
	#print(summary(win.p))
	#print(summary(win.e))

	assign(inst, dat)

	cat(c("Winner - Save:", inst, "\n"))
	save(list = inst, file = paste0(data_dir, inst, save_suffix))
}


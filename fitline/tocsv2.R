csv_class <- function(class_var, inst) {

	load_suffix <- "-table.Rda"
	save_suffix <- "-table.csv"
	mfile       <- paste0(inst, "-", class_var)

	load(paste0(fit_dir, mfile, load_suffix))

	full <- cbind(prob, wel) %>% as.data.frame
	full <- data.frame(pop= c("EUT Subjects", "Prelec Subjects"), round(full, digits = 3))

	print(full)

	#write.csv(round(prob, digits = 3), file = paste0(table_dir, mfile, "_prob", save_suffix), quote = F, row.names = F)
	#write.csv(round(wel,  digits = 3), file = paste0(table_dir, mfile,  "_wel", save_suffix), quote = F, row.names = F)

	write.csv(full, file = paste0(table_dir, mfile, "_full", save_suffix), quote = F, row.names = F)
}

csv_inst <- function(inst) {
	lapply(win_vars, csv_class, inst = inst)
}

lapply(insts, csv_inst)

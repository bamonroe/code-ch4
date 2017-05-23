merge_inst <- function(inst) {

	fname <- paste0(inst, "-")

	mfiles <- list.files(path = merged_dir, pattern = fname, full.names = T)

	res <- list()

	cat(c("Loading", inst, "files", "\n"))
	for (j in 1:length(mfiles)) {
		load(mfiles[j])
		print(mfiles[j])
		res[[j]] <- get(inst)
	}

	cat(c("Row Binding", inst, "\n"))
	res <- do.call(rbind, res)

	assign(inst, res)

	cat(c("Saving", inst, "\n"))
	save(list = inst, file = paste0(data_dir, inst, ".Rda"))

}

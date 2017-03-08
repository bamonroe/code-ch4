
insts <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")


merge_inst <- function(inst) {

	if (inst == "HNG") {
		fname <- "HNG-"
	} else {
		fname <- inst
	}

	mfiles <- list.files(path = "../data/classify/merged/", pattern = fname, full.names = T)

	res <- list()

	print(paste("Loading", inst, "files"))
	for (j in 1:length(mfiles)) {
		load(mfiles[j])
		print(mfiles[j])
		res[[j]] <- get(inst)
	}

	print(paste("Binding", inst))
	res <- do.call(rbind, res)

	assign(inst, res)

	print(paste("Saving", inst))
	save(list = inst, file = paste0("../data/classify/full/", inst, ".Rda"))

}

for (i in insts) {
	merge_inst(i)
}


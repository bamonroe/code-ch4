files <- list.files(path = "../data/classify/results", pattern = "HNG_res.*", full.names = T)

resdat <- list()
for (i in 1:length(files)) {

	print(files[i])
	load(files[i])

	resdat[[i]] <- HNG.res

}

results <- do.call(rbind, resdat)

save(results, file = "results.Rda")

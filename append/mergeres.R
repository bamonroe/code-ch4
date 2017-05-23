rfiles <- list.files(path = "../data/classify/results", pattern = "HNG_res", full.names = T)
wfiles <- list.files(path = "../data/classify/welcalc", pattern = "subdat", full.names = T)

resdat <- list()
for (i in 1:length(rfiles)) {
	print(rfiles[i])
	print(wfiles[i])
	load(rfiles[i])
	load(wfiles[i])
	resdat[[i]] <- cbind(HNG.res, HNG.wel)
}

HNG <- do.call(rbind, resdat)
save(HNG, file = "../data/classify/merged/HNG.Rda")
rm(HNG, resdat)

rfiles <- list.files(path = "../data/classify/results", pattern = "HO_res", full.names = T)
wfiles <- list.files(path = "../data/classify/welcalc", pattern = "subdat", full.names = T)

resdat <- list()
for (i in 1:length(rfiles)) {
	print(rfiles[i])
	print(wfiles[i])
	load(rfiles[i])
	load(wfiles[i])
	resdat[[i]] <- cbind(HO.res, HO.wel)
}

HO <- do.call(rbind, resdat)
save(HO, file = "../data/classify/merged/HO.Rda")
rm(HO, resdat)

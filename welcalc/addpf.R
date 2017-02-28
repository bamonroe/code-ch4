library(ctools)

e.files <- list.files("../data/classify/estimates", pattern = "-1.*.Rda", full.names = T)

c.lapply(e.files, function(f) {
	print(f)
	load(f)
	sub.est <- lapply(sub.est, function(sub) {
		sub <- lapply(sub, function (inst) {
			if (!is.na(inst$EUT[1])) {
				inst$EUT$pfunc <- "EUT"
			}
			if (!is.na(inst$POW[1])) {
				inst$POW$pfunc <- "POW"
			}
			if (!is.na(inst$INVS[1])) {
				inst$INVS$pfunc <- "INVS"
			}
			if (!is.na(inst$PRE[1])) {
				inst$PRE$pfunc <- "PRELEC"
			}
			inst
		})
		sub
	})
	save(sub.est, real, file = f)
})


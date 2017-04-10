library(halton)
library(dplyr)
library(mgcv)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
fit_dir    <- "../data/lo_fits/"
table_dir    <- "../tables/"
load_suffix <- "-win.Rda"
save_suffix <- ".csv"


for (inst in instruments) {

	load(paste0(fit_dir, inst, load_suffix))

	rnames <- c("EUT", "RDU_{Pow}", "RDU_{Inv}", "RDU_{Prelec}")
	rnames <- c("EUT", "Power", "Inverse-S", "Prelec")

	PA  <- round(cprobs$P_A, digits = 2)
	PB  <- round(cprobs$P_B, digits = 3)
	PBA <- round(cprobs$P_BA, digits = 3)
	PAB <- round(cprobs$P_AB, digits = 3)

	PA  <- t(PA)
	PB  <- t(PB)
	PBA <- cbind(rnames = rnames, PBA)
	PAB <- cbind(rnames = rnames, PAB)

	write.csv(PA,  file = paste0(table_dir, inst, "-PA",  save_suffix), quote = F, row.names = F)
	write.csv(PB,  file = paste0(table_dir, inst, "-PB",  save_suffix), quote = F, row.names = F)
	write.csv(PBA, file = paste0(table_dir, inst, "-PBA", save_suffix), quote = F, row.names = F)
	write.csv(PAB, file = paste0(table_dir, inst, "-PAB", save_suffix), quote = F, row.names = F)

	wsum <- round(welprob$wel_sums, digits = 3)
	PBA  <- round(welprob$P_BA,     digits = 3)
	PAB  <- round(welprob$P_AB,     digits = 3)

	wsum <- cbind(rnames = rnames, wsum)
	PBA  <- t(PBA)
	PAB  <- t(PAB)

	write.csv(wsum, file = paste0(table_dir, inst, "-wsum",  save_suffix), quote = F, row.names = F)
	write.csv(PBA,  file = paste0(table_dir, inst, "-PBA-wel", save_suffix), quote = F, row.names = F)
	write.csv(PAB,  file = paste0(table_dir, inst, "-PAB-wel", save_suffix), quote = F, row.names = F)

}

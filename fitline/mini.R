library(dplyr)

instruments <- c("HNG", "HNG_1", "HO", "LMS20", "LMS30", "SH")
instruments <- c("HNG_1")
data_dir    <- "../data/classify/full/"
load_suffix <- "-bak.Rda"
save_suff   <- "-mini.Rda"

wel.vars <- c("WelSurplus", "WelMax", "WelEfficiency", "CEdiff", "Prob")
wel.var  <- wel.vars[1]

for (inst in instruments) {
print(inst)

load(paste0(data_dir, inst, load_suffix))

tmp <- get(inst)

nn <- 100000

nn <- 1

tmp1 <- tmp %>%
	filter(!is.na(win_05)) %>%
	filter(model == "EUT") %>%
	select(starts_with("win"), starts_with("real"), ends_with(wel.var),
				 r, mu, alpha, beta, model) %>%
	sample_frac(nn)
	#sample_n(nn)

tmp2 <- tmp %>%
	filter(!is.na(win_05)) %>%
	filter(model == "pow") %>%
	select(starts_with("win"), starts_with("real"), ends_with(wel.var),
				 r, mu, alpha, beta, model) %>%
	sample_frac(nn)
	#sample_n(nn)

tmp3 <- tmp %>%
	filter(!is.na(win_05)) %>%
	filter(model == "invs") %>%
	select(starts_with("win"), starts_with("real"), ends_with(wel.var),
				 r, mu, alpha, beta, model) %>%
	sample_frac(nn)
	#sample_n(nn)

tmp4 <- tmp %>%
	filter(!is.na(win_05)) %>%
	filter(model == "prelec") %>%
	select(starts_with("win"), starts_with("real"), ends_with(wel.var),
				 r, mu, alpha, beta, model) %>%
	sample_frac(nn)
	#sample_n(nn)

tmp <- rbind(tmp1, tmp2, tmp3, tmp4)

assign(inst, tmp)

save(list = inst, file = paste0(data_dir, inst, save_suff))

rm(tmp, tmp1, tmp2, tmp3, tmp4)
rm(list=inst)

}


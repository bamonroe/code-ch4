library(dplyr)

load("HNG_est.Rda")
load("HNG_reported.Rda")

w10 <- summary(sub.df.report$win_10[which(!is.na(sub.df.report$win_10))])
w05 <- summary(sub.df.report$win_05[which(!is.na(sub.df.report$win_05))])
w01 <- summary(sub.df.report$win_01[which(!is.na(sub.df.report$win_01))])

w10_f <- w10 / sum(w10)
w05_f <- w05 / sum(w05)
w01_f <- w01 / sum(w01)

w10_f
w05_f
w01_f

allna <- sub.df %>%
	select(-starts_with("win"), -ID) %>%
	apply(1, function(r) {all(is.na(r))})

	ifelse(allna, 1,0) %>%
	sum()


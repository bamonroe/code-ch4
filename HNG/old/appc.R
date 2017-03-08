library(dplyr)
load("HNG_est.Rda")

est <- sub.df.t %>%
	select(ID, starts_with("win"), EUT_r_Est, INV_r_Est, INV_alpha_Est, POW_r_Est, POW_alpha_Est, PRE_r_Est, PRE_beta_Est, PRE_alpha_Est)

eut_or <- est$EUT_r_Est[order(est$EUT_r_Est)]
cbind(order(est$EUT_r_Est), eut_or)

old <- c(1:102)
new <- c(25, 14, 32, 20, 22, 19, NA, 16, NA, 21, 28, 23, 26, 9, 17, 31, 1, NA, 60, 15, 2, 11, 12, 10, 6, 30, 7, NA, 18, 3, 62, NA, 63, NA, 71, 58, 70, 60, 53, 34, 61, 68, 67, 59, 40, 57, NA, 64, 72, 56, 49, 46, 38, 50, 41, 47, NA, 33, 44, NA, 37, 45, NA, NA, 35, 54, 95, 104, 101, 109, 102, 99, 106, 107, 108, 80, 74, 111, 98, 97, 96, 76, 86, 91, 110, 82, 90, 78, 100, 93, 79, 88, 81, 73, 83, 85, 77, 87, NA, 75, 94, 89 )

nres <- which(is.na(new))

inv_or <- est$INV_r_Est[order(est$INV_r_Est)]
nres
cbind(order(est$INV_r_Est), inv_or)

# 7 9 18 28 32 34 47 57 60 63 64 99
new[7]  <- 27
new[9]  <- NA
new[18] <- NA
new[28] <- NA
new[32] <- 55
new[34] <- NA
new[47] <- 42
new[57] <- NA
new[60] <- NA
new[63] <- 52
new[64] <- 39
new[99] <- 92

nres <- which(is.na(new))
nres

# 9 18 28 34 57 60
pow_or <- est$POW_r_Est[order(est$POW_r_Est)]
nres
cbind(order(est$POW_r_Est), pow_or)

new[9]  <- NA
new[18] <- 29
new[28] <- 8
new[34] <- 65
new[57] <- NA
new[60] <- NA

# OBS 9 57 and 60 I didn't get convergence for on any model
# Harrison and Ng only got convergences for EUT on these models.
sub.df.report <- sub.df.t[new,]
rownames(sub.df.report) <- 1:102
sub.df.report

# Which obs do we have the same results for
same <- c(T,F,T,F,T,T,F,T,F,T,F,T,T,F,T,T,T,T,F,T,T,F,T,T,T,T,T,T,F,F,F,T,F,F,T,T,F,T,T,T,T,T,F,T,T,T,T,T,T,F,F,T,T,F,T,T,F,T,T,F,T,T,F,T,T,T,T,T,F,T,T,T,T,T,T,T,T,T,T,T,T,F,F,T,T,T,F,T,F,T,T,T,T,T,T,T,T,T,F,T,T,F)
sum(ifelse(same, 1, 0)) / length(same)

# Which of my subjects do I not have any estimates for
allna <- sub.df %>%
	select(-starts_with("win"), -ID) %>%
	apply(1, function(r) {all(is.na(r))})

# How many subjects is  this
ifelse(allna, 1,0) %>%
	sum()

# df of all subjects that we both have estimates for
sub2 <- sub.df[new[which(!is.na(new))], ]

# df of all subjects that I have estimates for
sub2 <- sub.df[new[which(!is.na(new))], ]


save(sub.df.report, file = "HNG_reported.Rda")




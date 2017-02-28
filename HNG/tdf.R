library(MSL)

load("HNG_est.Rda")

sub.df  <- ML.dataframe(sub.est)

print(sub.df$win_05)

w05 <- sub.df$win_05[which(!is.na(sub.df$win_05))]

print(length(w05))

print(summary(w05))
print(summary(w05) / sum(summary(w05)))

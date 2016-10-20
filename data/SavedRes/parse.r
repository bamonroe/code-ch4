library(ctools)
c.library("dplyr")
#res
load("eut_rdu.Rda")

res <- do.call(Map, c(rbind, res ))
res <- do.call(data.frame, res)

er <- (res %>% select(starts_with("reals"))) - (res %>% select(starts_with("estimates")))

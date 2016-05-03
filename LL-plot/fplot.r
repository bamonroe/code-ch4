rm(list=ls())

library(ctools)

c.library("ggplot2","dplyr","reshape2")

load("LL.Rda")

#LL <- LL %>%
#	filter(LL >-5550)

USE <- melt(LL, measure.vars=c("rm","rs","um","us"))
USE <- tbl_df(USE)

pop.line <- data.frame(variable=levels(USE$variable), vl=genpars)
sam.line <- data.frame(variable=levels(USE$variable), vl=sampars)

maxpars <- LL[LL$LL == max(LL$LL),-1]
maxpars <- c(rm=maxpars$rm, rs=maxpars$rs, um=maxpars$um, us=maxpars$us)

max.line <-data.frame(variable=levels(USE$variable), vl=maxpars)

# Alpha level of the points
alph <- .5

p <- ggplot(USE,aes_string(y="LL",x="value",color="variable"))
p <- p + geom_smooth()
p <- p + geom_point(alpha=alph)
p <- p + geom_vline(data=pop.line, aes(xintercept=vl), linetype="dashed")
p <- p + geom_vline(data=sam.line, aes(xintercept=vl), linetype="twodash")
p <- p + geom_vline(data=max.line, aes(xintercept=vl), linetype="dotted")
p <- p + facet_wrap( facets="variable", ncol=2, scale="free_x")

p

print(genpars)
print(maxpars)



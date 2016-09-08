library(ctools)

c.library("microbenchmark","MSL","cgen")

# Define the sim population
sim <- c(rm=.6, rs=.3, um=.15, us=.05, rh=.3)
N <- 100
HH <- 50

# Generate a simulated instrument
D <- genChoice(sim, N, inst = "HL")

head(D, n = 10)

#s1 <- MSL.SLL.setup(pars = sim, inst = D, dempars = list(), HH = HH, model = "EUT", covar.version = "Choice" )
#s2 <- MSL.SLL.setup(pars = sim, inst = D, dempars = list(), HH = HH, model = "EUT", covar.version = "Subject" )
#s3 <- MSL.SLL.setup(pars = sim, inst = D, dempars = list(), HH = HH, model = "EUT", covar.version = "Fixed" )
#
#o1 <- EUT_COV_perChoice(s1[[1]], s1[[2]], s1[[3]], s1[[4]], s1[[5]], s1[[6]], s1[[7]])
#o2 <- EUT_COV_perSub(s2[[1]], s2[[2]], s2[[3]], s2[[4]], s2[[5]], s2[[6]])
#o3 <- EUT_COV_perSub_fixed(s3[[1]], s3[[2]], s3[[3]], s3[[4]], s3[[5]], s3[[6]], s3[[7]], s3[[8]])
#
#o1
#o2
#o3
#
#microbenchmark(
##		EUT_COV_perChoice(s1[[1]], s1[[2]], s1[[3]], s1[[4]], s1[[5]], s1[[6]], s1[[7]]),
#		EUT_COV_perSub(s2[[1]], s2[[2]], s2[[3]], s2[[4]], s2[[5]], s2[[6]]),
#    EUT_COV_perSub_fixed(s3[[1]], s3[[2]], s3[[3]], s3[[4]], s3[[5]], s3[[6]], s3[[7]], s3[[8]]),
#    times = 10
#)

config = list(method="BFGS", itnmax = 1000)
control= list(trace=2, REPORT = 1, kkt = T, usenumDeriv = T, dowarn = F)

m1 <- MSL.optim(pars = sim, inst = D, dempars=list(), HH=HH, model="EUT", covar.version = "Fixed" ,
					try = F, config = config, control = control)

stop()

microbenchmark(
m1 <- MSL.optim(pars = sim, inst = D, dempars=list(), HH=HH, model="EUT", covar.version = "Subject" ,
					try = F, config = config, control = control),
m2 <- MSL.optim(pars = sim, inst = D, dempars=list(), HH=HH, model="EUT", covar.version = "Fixed" ,
					try = F, config = config, control = control),
times=10
)


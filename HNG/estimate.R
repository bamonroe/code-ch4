# Load the HNG cleaned data
load("../data/HNG/HNG.Rda")

HNG <- DAT %>%
	filter(Inst == "HNG")
INS <- DAT %>%
	filter(Inst == "HNG.ins")

# Some configurations
opt  <- "BFGS" # BFGS optimizer Method
opt  <- "NR"   # Newton Rhapson optimizer Method
opt  <- "BHHH" # BHHH optimizer Method
inum <- 500    # Iteration limit

#sub.est.t <- ML.all(HNG, optimizer = opt, iterations = inum, do.par = T)
#sub.df.t <- ML.dataframe(sub.est.t)

HNG$A0 <- ((HNG$A0 - 10)/40)/20
HNG$A1 <- ((HNG$A1 - 10)/40)/20
HNG$A2 <- ((HNG$A2 - 10)/40)/20
HNG$B0 <- ((HNG$B0 - 10)/40)/20
HNG$B1 <- ((HNG$B1 - 10)/40)/20
HNG$B2 <- ((HNG$B2 - 10)/40)/20

sub.est <- ML.all(HNG, optimizer = opt, iterations = inum, do.par = T)
sub.df  <- ML.dataframe(sub.est)


#save(sub.est, sub.est.t, sub.df, sub.df.t, file = "HNG_est.Rda")
save(sub.est, sub.df, file = "HNG_est.Rda")

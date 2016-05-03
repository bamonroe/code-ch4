library(dplyr)
source("geninst.r")

D <- genHL(c(1,.1,.3,.1),1)

D <- D[1:10,]
D <- tbl_df(D)
D <- D %>%
	select(-c, -r, -mu, -ID)

A <- D %>%
	select(starts_with("A"))

B <- D %>%
	select(starts_with("B"))

pA <- D %>%
	select(starts_with("pA"))

pB <- D %>%
	select(starts_with("pB"))

Max <- D$Max
Min <- D$Min

crra <- function(x,r){
	x^(1-r) / (1-r)
}

UU <- function(x,p,r){
	rowSums(crra(x,r)*p)
}

r <- .640388
mu <- .0842498

r <- .813987
mu <- .0350778

r <- 0.359612
mu <- .0842498

choice <- c(0, #1
						1, #2
						0, #3
						1, #4
						1, #5
						0, #6
						1, #7
						1, #8
						1, #9
						1) #10
						
						

UA <- UU(A,pA,r) 
UB <- UU(B,pB,r) 

ctx <- crra(Max,r) - crra(Min,r)

UB1 <- (UB - UA) / ctx / mu
UB1 <- (UB / ctx / mu) - (UA / ctx / mu)

PA <- 1/(1+exp(UB1))
PB <- 1 - PA

print(rbind(r=r, mu=mu))
print(choice)
print(rbind(PA=PA, PB=PB))

ifelse(choice==0,PA,PB)
print(rbind(UB1=UB1, UA=UA, UB=UB, ctx=ctx))

crra <- function(x,r){
	x^(1-r) / (1-r)
}

ml_eut <- function(par, Inst){

	r  <- par[1]
	mu <- exp(par[2])^(1.0/3.0)

	A      <- Inst[["A"]]
	B      <- Inst[["B"]]
	pA     <- Inst[["pA"]]
	pB     <- Inst[["pB"]]
	choice <- Inst[["choice"]]
	Max    <- Inst[["Max"]]
	Min    <- Inst[["Min"]]

	cnum <- ncol(A)

	# Calculate the context

	ctx <- crra(Max, r) - crra(Min, r)
	
	# Calculate the utility of the lotteries

	UA <- rowSums(pA * crra(A,r))
	UB <- rowSums(pB * crra(B,r))

	for(j in 1:cnum ){
		if (j == 1){
			UA <- pA[,j] * crra(A[,j], r)
			UB <- pB[,j] * crra(B[,j], r)
		}
		else{
			UA <- UA + (pA[,j] * crra(A[,j], r))
			UB <- UB + (pB[,j] * crra(B[,j], r))
		}
	}

	# Re-base utility of B and add in context and fechner
	UB1  <- (UB-UA)/ctx/mu

	# If we have no issues, this is the choice probability of A
	Aprob <- (1.0 / (1.0 + exp(UB1)))
	Bprob <- 1.0 - Aprob

	# Grab the choice probability for the chosen option
	like <- ifelse(choice==0, Aprob, Bprob)

	ll <- log(like)

	return(-sum(ll))


}


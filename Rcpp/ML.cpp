#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

NumericVector crra(NumericVector x, double r){
	return(pow(x,r) / r);
}

//NumericVector pw(NumericVector p , NumericVector a, NumericVector b){
//     return( exp(-b * vpow(-log(p), a)) );
//}

// [[Rcpp::export]]
double ML_EUT(NumericVector par, List Inst){

	int dbug=0;

	double r = 1 - par[0];
	double mu =pow(exp(par[1]), (1.0/3.0));

	NumericMatrix A      = Inst["A"];
	NumericMatrix B      = Inst["B"];
	NumericMatrix pA     = Inst["pA"];
	NumericMatrix pB     = Inst["pB"];
	NumericVector choice = Inst["choice"];
	NumericVector Max    = Inst["Max"];
	NumericVector Min    = Inst["Min"];

	double cnum = A.ncol();

	// Calculate the context
	NumericVector ctx = (pow(Max, r) / r) - (pow(Min, r) / r);

	// Calculate the utility of the lotteries
	NumericVector UA;
	NumericVector UB;

	for(int j = 0; j < cnum; j++){
		if (j == 0){
			UA = pA(_,j) * crra(A(_,j), r);
			UB = pB(_,j) * crra(B(_,j), r);
		}
		else{
			UA = UA + (pA(_,j) * crra(A(_,j), r));
			UB = UB + (pB(_,j) * crra(B(_,j), r));
		}
	}
			
	// Re-base utility of B and add in context and fechner
	NumericVector UB1  = (UB-UA)/ctx/mu;

	// If we have no issues, this is the choice probability of A
	NumericVector Aprob = (1.0 / (1.0 + exp(UB1)));
	NumericVector Bprob = 1.0 - Aprob;
	
	// Grab the choice probability for the chosen option
	NumericVector like = ifelse(choice==0, Aprob, Bprob);

	NumericVector ll = log(like);

	return(-sum(ll));

}


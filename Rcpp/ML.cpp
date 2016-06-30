#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

NumericVector crra(NumericVector x, double r){
	return(pow(x,1-r) / (1-r));
	//return(pow(x,r));
}

NumericVector ppow(NumericVector p, NumericVector pweights){
	double gamma = pweights[0];
	return(pow(p,gamma));
}

NumericVector flex(NumericVector p, NumericVector pweights){
	double alpha = pweights[0];
	double beta = pweights[1];
	return(exp(-beta * (pow(-log(p), alpha))));
}

// DBUG line
// Rcout << "here" << dbug << std::endl ; dbug++;

// [[Rcpp::export]]
double ML_EUT(NumericVector par, List Inst){

	int dbug=0;

	double r = par[0];
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
	NumericVector ctx = crra(Max, r) - crra(Min, r);

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

// [[Rcpp::export]]
double ML_RDU(NumericVector par, List Inst, int pweight){

	int dbug=0;

	double r = 1.0 - par[0];
	double mu = pow(exp(par[1]), (1.0/3.0));

	NumericVector pweights(2);

	if (pweight == 0) {
		double alpha = pow(exp(par[2]), (1.0/3.0));
		pweights[0] = alpha;
	}
	else{
		double alpha = pow(exp(par[2]), (1.0/3.0));
		pweights[0] = alpha;
		double beta = pow(exp(par[3]), (1.0/3.0));
		pweights[1] = beta;
	}

	NumericMatrix A      = Inst["A"];
	NumericMatrix B      = Inst["B"];
	NumericMatrix pA     = Inst["pA"];
	NumericMatrix pB     = Inst["pB"];
	IntegerVector choice = Inst["choice"];
	NumericVector Max    = Inst["Max"];
	NumericVector Min    = Inst["Min"];

	double cnum = A.ncol();
	double rnum = A.nrow();

	// Allocate place for probability weights
	NumericMatrix pwA(rnum, cnum);
	NumericMatrix pwB(rnum, cnum);

	// Allocate place for cummulative probabilities
	NumericMatrix cprobA(rnum, cnum);
	NumericMatrix cprobB(rnum, cnum);

	for(int j = cnum - 1 ; j > -1 ; j-- ){

		int k = j + 1;

		if (j == cnum -1 ){
			cprobA(_,j) = pA(_,j);
			cprobB(_,j) = pB(_,j);
			cprobA(_,j) = ifelse( cprobA(_,j) > 1.0, 1.0 , cprobA(_,j));
			cprobB(_,j) = ifelse( cprobB(_,j) > 1.0, 1.0 , cprobB(_,j));

			if (pweight == 0 ){
				pwA(_,j) =  ppow(cprobA(_,j), pweights);
				pwB(_,j) =  ppow(cprobB(_,j), pweights);
			}
			else{
				pwA(_,j) =  flex(cprobA(_,j), pweights);
				pwB(_,j) =  flex(cprobB(_,j), pweights);
			}
		}
		else{
			cprobA(_,j) = pA(_,j) + cprobA(_,k);
			cprobB(_,j) = pB(_,j) + cprobB(_,k);
			cprobA(_,j) = ifelse( cprobA(_,j) > 1.0, 1.0 , cprobA(_,j));
			cprobB(_,j) = ifelse( cprobB(_,j) > 1.0, 1.0 , cprobB(_,j));

			if (pweight == 0 ){
				pwA(_,j) =  ppow(cprobA(_,j), pweights) - ppow(cprobA(_,k), pweights);
				pwB(_,j) =  ppow(cprobB(_,j), pweights) - ppow(cprobB(_,k), pweights);
			}
			else{
				pwA(_,j) =  flex(cprobA(_,j), pweights) - flex(cprobA(_,k), pweights);
				pwB(_,j) =  flex(cprobB(_,j), pweights) - flex(cprobB(_,k), pweights);
			}
		}

	//	Rcout << "pweight" << pwA(0,j) << std::endl ;
	//	Rcout << "pweight" << pwB(0,j) << std::endl ;

	}

	// Calculate the context
	NumericVector ctx = crra(Max, r) - crra(Min, r);

	// Calculate the utility of the lotteries
	NumericVector UA;
	NumericVector UB;

	for(int j = 0; j < cnum; j++){
		if (j == 0){
			UA = pwA(_,j) * crra(A(_,j), r);
			UB = pwB(_,j) * crra(B(_,j), r);
		}
		else{
			UA = UA + (pwA(_,j) * crra(A(_,j), r));
			UB = UB + (pwB(_,j) * crra(B(_,j), r));
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

/*
// [[Rcpp::export]]
double ML_RDU(NumericVector par, List Inst, int pweight){

	int dbug=0;

	double r = 1.0 - par[0];
	double mu = pow(exp(par[1]), (1.0/3.0));

	NumericVector pweights(2);

	if (pweight == 0) {
		double alpha = pow(exp(par[2]), (1.0/3.0));
		pweights[0] = alpha;
	}
	else{
		double alpha = pow(exp(par[2]), (1.0/3.0));
		pweights[0] = alpha;
		double beta = pow(exp(par[3]), (1.0/3.0));
		pweights[1] = beta;
	}

	NumericMatrix A      = Inst["A"];
	NumericMatrix B      = Inst["B"];
	NumericMatrix pA     = Inst["pA"];
	NumericMatrix pB     = Inst["pB"];
	IntegerVector choice = Inst["choice"];
	NumericVector Max    = Inst["Max"];
	NumericVector Min    = Inst["Min"];

	double cnum = A.ncol();
	double rnum = A.nrow();

	// Allocate place for probability weights
	NumericMatrix pwA(rnum, cnum);
	NumericMatrix pwB(rnum, cnum);

	// Allocate place for cummulative probabilities
	NumericMatrix cprobA(rnum, cnum);
	NumericMatrix cprobB(rnum, cnum);

	for(int j = cnum - 1 ; j > -1 ; j-- ){

		int k = j + 1;

		if (j == cnum -1 ){
			cprobA(_,j) = pA(_,j);
			cprobB(_,j) = pB(_,j);
			cprobA(_,j) = ifelse( cprobA(_,j) > 1.0, 1.0 , cprobA(_,j));
			cprobB(_,j) = ifelse( cprobB(_,j) > 1.0, 1.0 , cprobB(_,j));

			if (pweight == 0 ){
				pwA(_,j) =  ppow(cprobA(_,j), pweights);
				pwB(_,j) =  ppow(cprobB(_,j), pweights);
			}
			else{
				pwA(_,j) =  flex(cprobA(_,j), pweights);
				pwB(_,j) =  flex(cprobB(_,j), pweights);
			}
		}
		else{
			cprobA(_,j) = pA(_,j) + cprobA(_,k);
			cprobB(_,j) = pB(_,j) + cprobB(_,k);
			cprobA(_,j) = ifelse( cprobA(_,j) > 1.0, 1.0 , cprobA(_,j));
			cprobB(_,j) = ifelse( cprobB(_,j) > 1.0, 1.0 , cprobB(_,j));

			if (pweight == 0 ){
				pwA(_,j) =  ppow(cprobA(_,j), pweights) - ppow(cprobA(_,k), pweights);
				pwB(_,j) =  ppow(cprobB(_,j), pweights) - ppow(cprobB(_,k), pweights);
			}
			else{
				pwA(_,j) =  flex(cprobA(_,j), pweights) - flex(cprobA(_,k), pweights);
				pwB(_,j) =  flex(cprobB(_,j), pweights) - flex(cprobB(_,k), pweights);
			}
		}

	//	Rcout << "pweight" << pwA(0,j) << std::endl ;
	//	Rcout << "pweight" << pwB(0,j) << std::endl ;

	}

	// Calculate the context
	NumericVector ctx = crra(Max, r) - crra(Min, r);

	// Calculate the utility of the lotteries
	NumericVector UA;
	NumericVector UB;

	for(int j = 0; j < cnum; j++){
		if (j == 0){
			UA = pwA(_,j) * crra(A(_,j), r);
			UB = pwB(_,j) * crra(B(_,j), r);
		}
		else{
			UA = UA + (pwA(_,j) * crra(A(_,j), r));
			UB = UB + (pwB(_,j) * crra(B(_,j), r));
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
*/

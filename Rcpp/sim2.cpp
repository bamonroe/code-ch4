#include <Rcpp.h>
using namespace Rcpp;

double crra(double x,double r){
	return(pow(x,r) / r);
}

// pow takes any base, but only up to double exponent, this function fixes that
NumericVector vpow(const NumericVector base, const NumericVector exp) {
  NumericVector out(base.size());
  std::transform(base.begin(), base.end(),
                 exp.begin(), out.begin(), ::pow);
  return out;
}

NumericVector pw(NumericVector p , NumericVector a, NumericVector b){
     return( exp(-b * vpow(-log(p), a)) );
}

// [[Rcpp::export]]
double MSL_EUT(NumericVector par, NumericMatrix h1, NumericMatrix h2, List Inst){

	int dbugc = 1;

	double rm = par[0];
	double rs =pow(exp(par[1]), (1.0/3.0));
	double um =pow(exp(par[2]), (1.0/3.0));
	double us =pow(exp(par[3]), (1.0/3.0));

	double k = pow(um,2) / pow(us,2);
	double t = pow(us,2) / um;

	int h = h1.ncol();

	NumericVector r;
	NumericVector mu;

	NumericVector ctx;

	NumericVector pzA;
	NumericVector pzB;
	NumericVector prA;
	NumericVector prB;

	NumericVector UA;
	NumericVector UB;
	NumericVector UB1;
	NumericVector PA;

	NumericVector N0;
	NumericVector N1;
	NumericVector Aprob;
	NumericVector Bprob;


	double N = Inst.size();

	NumericVector subprob(N);

	NumericVector simprob(h);

	for(int n = 0; n < N; n++){
		
		List inst = Inst[n];

		NumericMatrix A      = inst["A"];
		NumericMatrix B      = inst["B"];
		NumericMatrix pA     = inst["pA"];
		NumericMatrix pB     = inst["pB"];
		NumericVector choice = inst["choice"];
		NumericVector Max    = inst["Max"];
		NumericVector Min    = inst["Min"];

		int rnum = A.nrow();

		NumericVector UA(rnum);
		NumericVector UB(rnum);

		NumericMatrix sim(rnum, h);
		
		r  = qnorm(h1(n,_),rm,rs);
		r = 1.0 - r;
		mu = qgamma(h2(n,_),k,t);
	
		for(int i = 0; i < h ; i++){

			// Calculate the context
			ctx = pow(Max, r[i]) / r[i] - pow(Min,r[i]) / r[i];

			// Calculate the utility of the lotteries
			UA.fill(0.0);
			UB.fill(0.0);
			
			for(int j = 0; j < A.ncol(); j++){
				pzA = A(_,j);
				pzB = B(_,j);
				prA = pA(_,j);
				prB = pB(_,j);

				UA = UA + (prA * pow(pzA,r[i]) / r[i]);
				UB = UB + (prB * pow(pzB,r[i]) / r[i]);
			}
			
			// Re-base utility of B and add in context and fechner
			UB1  = (UB/ctx/mu) - (UA/ctx/mu);

			// If we have no issues, this is the choice probability of A
			PA = (1.0 / (1.0 + exp(UB1)));

			// Are we dealing with an insane number?
			// yes
			N0 = ifelse( UB > UA , 0.0 , 1.0 ); 
			// no, but are we making an insane number via exp?
			N1 = ifelse( UB1 > 709.0, 0.0 , PA );

			// Check for the 2 issues, and return the probability of A
			Aprob = ifelse( is_na(PA) , N0 , N1);

			// Making pB = 1-pA saves us the exponential calculations - it's faster
			NumericVector Bprob = 1.0 - Aprob;

			// Grab the choice probability for the chosen option
			sim(_,i) = ifelse(choice==0, Aprob, Bprob);
			
		}
		
		simprob.fill(1.0);

		for(int i = 0; i < rnum ; i++){
			simprob = simprob * sim(i,_);
		}

		subprob[n] = mean(simprob);

//		Rcout << "here" << dbugc << " " <<subprob[n] << std::endl; dbugc += 1;

	}

	NumericVector sl = log(subprob);
	return(-sum(sl));

}


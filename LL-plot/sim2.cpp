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
		NumericVector UB1(rnum);

		NumericMatrix sim(rnum, h);
		
		r  = qnorm(h1(n,_),rm,rs);
		mu = qgamma(h2(n,_),k,t);

		double ri;
		
		for(int i = 0; i < h ; i++){

			ri = 1.0 - r[i];

			// Calculate the context
			ctx = (pow(Max, ri) / ri ) - (pow(Min, ri) / ri);

			// Calculate the utility of the lotteries
			for(int j = 0; j < A.ncol(); j++){
				// Read questionmark thingy as an ifelse 
				if(j == 0){
					UA = (pA(_,j) * pow(A(_,j),ri) / ri);
					UB = (pB(_,j) * pow(B(_,j),ri) / ri);
				}
				else{
					UA = UA + (pA(_,j) * pow(A(_,j),ri) / ri) ;
					UB = UB + (pB(_,j) * pow(B(_,j),ri) / ri) ;
				}
			}
			
			// Re-base utility of B and add in context and fechner
			UB1  = (UB - UA) / ctx / mu[i];

			// If we have no issues, this is the choice probability of A
			PA = (1.0 / (1.0 + exp(UB1)));

			// Are we dealing with an insane number?
			// yes
			N0 = ifelse( UB > UA , 0.0 , 1.0 ); 
			// no, but are we making an insane number via exp?
			N1 = ifelse( UB1 > 709.0, 0.0 , PA );

			// Check for the 2 issues, and return the probability of A
			Aprob = ifelse( is_na(UB1) , N0 , N1);

			// Making pB = 1-pA saves us the exponential calculations - it's faster
			NumericVector Bprob = 1.0 - Aprob;

			// Grab the choice probability for the chosen option
			sim(_,i) = ifelse(choice==0, Aprob, Bprob);

			if(n==-1 ){
				NumericVector pp = sim(_,i);
				Rcout << "r: " << r[i] << std::endl;
				Rcout << "mu: " << mu[i] << std::endl;
				Rcout << "choice: " << choice << std::endl;
				Rcout << "Aprob: " << Aprob << std::endl;
				Rcout << "Bprob: " << Bprob << std::endl;
				Rcout << "Choicen:" << pp << std::endl;
				Rcout << "UB1: " << UB1 << std::endl;
				Rcout << "UA: " << UA << std::endl;
				Rcout << "UB: " << UB << std::endl;
				Rcout << "CTX: " << ctx << std::endl;
				Rcout << "" << std::endl;
			}
			
		}
		
		simprob.fill(1.0);

		for(int i = 0; i < rnum ; i++){
			simprob = simprob * sim(i,_);
		}

		subprob[n] = mean(simprob);

	}

	NumericVector sl = log(subprob);
	return(-sum(sl));

}


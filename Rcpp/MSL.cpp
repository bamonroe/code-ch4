#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// Function to add covariates
NumericVector getPars(int csize, List covv, List covi, NumericVector fpar){

	int pnum = 0;
	double ii;

	NumericVector index, var;
	NumericVector par(csize);

	for(int i = 0 ; i < csize ; i++){
		par[i] = fpar[pnum];
		if( covi[i] != R_NilValue){
			index = covi[i];
			for(int j=0; j < index.size(); j++){
				pnum++;
				ii = index[j];
				var = covv[ii];
				par[i] = par[i] + fpar[pnum] * var[0];
			}
		}
		pnum++;
	//	Rcout << pnum << std::endl;
	
	}

	return(par);

}

// Functions needed for correlation
arma::mat fillcorr(arma::vec rho){
	int z = .5* ( sqrt(8*rho.n_elem + 1) + 1);
	arma::mat RHO(z,z,arma::fill::eye);
	int count = -1;
	for(int row = 0; row < (z-1); row++){
		for(int col = row+1; col < z; col++){
			count++;
			RHO(row,col) = rho[count];
		}
	}
	RHO = symmatu(RHO);

	return(RHO);

}

NumericMatrix mvhnormInv( NumericMatrix HH, arma::vec mu, arma::mat sigma) {

	int h = HH.nrow();
	int ncols = sigma.n_cols;
	arma::mat Y(h, ncols);

	NumericVector hcol;

	for(int i = 0; i < ncols ; i++){
		hcol = qnorm(HH(_,i),0.0,1.0 );
		Y.col(i) = Rcpp::as<arma::colvec>(hcol);
	}

	arma::mat RES = arma::repmat(mu, 1, h).t() + Y * arma::chol(sigma);

	NumericMatrix OUT(h, ncols);

	arma::colvec vtemp;
	NumericVector htemp;

	double mean;
	double sd;

	for(int i = 0; i < ncols ; i++){
		vtemp = RES.col(i);
		htemp = as<NumericVector>(wrap(vtemp));

		mean = mu[i];
		sd = sqrt(sigma(i,i));

		OUT(_,i) = pnorm( htemp , mean , sd );
	}

	return(OUT);

}

// Utility functions
double crra(double x,double r){
	return(pow(x,r) / r);
}

// Likelihood Functions

// [[Rcpp::export]]
double MSL_EUT(NumericVector par, NumericMatrix h1, NumericMatrix h2, List Inst){

	// dbug is only used with Rcout to keep trace down errors.
	int dbug = 0;

	double rm = par[0];
	double rs =pow(exp(par[1]), (1.0/3.0));
	double um =pow(exp(par[2]), (1.0/3.0));
	double us =pow(exp(par[3]), (1.0/3.0));

	arma::vec means(2);
	means(0) = rm;
	means(1) = um;
	NumericVector sd    = NumericVector::create(rs, us);

	double k = pow(um,2) / pow(us,2);
	double t = pow(us,2) / um;

	// Bound the correlations between -1,1
	arma::vec rho = NumericVector::create(par[4]);
	rho = exp(rho);
	rho = (rho/(1 + rho))*2 - 1;

	// How big is correlation matrix
	int z = .5* ( sqrt(8*rho.n_elem + 1) + 1);

	arma::mat RHO = fillcorr(rho);

	arma::mat sigma(z,z,arma::fill::eye);

	// Fill out the covariance matrix
	for(int row = 0; row < z ; row++){
		for(int col = 0; col < z ; col++){
			sigma(row,col) = sd[row] * sd[col] * RHO(row,col);
		}
	}

	int h = h1.ncol();

	NumericVector r;
	NumericVector mu;

	NumericVector ctx;

	NumericVector UA;
	NumericVector UB;
	NumericVector UB1;
	NumericVector PA;

	NumericVector N0;
	NumericVector N1;
	NumericVector Aprob;
	NumericVector Bprob;

	NumericMatrix CH(h,2);

	double N = Inst.size();

	NumericVector subprob(N);

	arma::rowvec simprob(h);

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
		int cnum = A.ncol();

		NumericVector UA(rnum);
		NumericVector UB(rnum);

		NumericMatrix sim(rnum, h);

		CH(_,0) = h1(n,_);
		CH(_,1) = h2(n,_);

		// Correlate our random variables
		CH = mvhnormInv(CH, means, sigma ) ;
		
		r  = qnorm(CH(_,0),rm,rs);
		r = 1.0 - r;
		mu = qgamma(CH(_,1),k,t);

		double ri;

		for(int i = 0; i < h ; i++){

			ri = r[i];

			// Calculate the context
			ctx = (pow(Max, ri) / ri) - (pow(Min, ri) / ri);

			// Calculate the utility of the lotteries
			for(int j = 0; j < cnum; j++){
				if (j == 0){
					UA = (pA(_,j) * pow(A(_,j), ri) / ri);
					UB = (pB(_,j) * pow(B(_,j), ri) / ri);
				}
				else{
					UA = UA + (pA(_,j) * pow(A(_,j), ri) / ri);
					UB = UB + (pB(_,j) * pow(B(_,j), ri) / ri);
				}
			}
			
			// Re-base utility of B and add in context and fechner
			UB1  = (UB-UA)/ctx/mu[i];

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
		
		arma::mat simArma = Rcpp::as<arma::mat>(sim);

		simprob = prod(simArma,0);

		double spb = mean(simprob);

		subprob[n] = spb;

	}

	NumericVector sl = log(subprob);
	return(-sum(sl));

}

// [[Rcpp::export]]
double MSL_EUT_cov(NumericVector par, List covars, List covindex, NumericMatrix h1, NumericMatrix h2, List Inst){

	// dbug is only used with Rcout to keep trace down errors.
	int dbug = 0;

	NumericVector r;
	NumericVector mu;

	NumericVector ctx;

	NumericVector UB1;
	NumericVector PA;

	NumericVector N0;
	NumericVector N1;
	NumericVector Aprob;
	NumericVector Bprob;

	int h = h1.ncol();
	double N = Inst.size();

	NumericMatrix CH(h,2);

	NumericVector subprob(N);
	arma::rowvec simprob(h);

	// Loop through all subjects, this is necessary because we have correlation 
	// between the r and mu variables.
	
	double ii;
	NumericVector var;

	for(int n = 0; n < N; n++){
		
		// Here we make the "base" parameters linnear equations of the covariate
		// coefficient and ONLY the first value of the covariate. To allow covariates
		// to change within a subject's choices, we'd need another loop, which means
		// a lot more function calls, and likely a fairly big decrease in speed.
		int csize = covindex.size();
		NumericVector basepar = getPars(csize, covars[n], covindex, par);

		double rm = basepar[0];
		double rs = pow(exp(basepar[1]), (1.0/3.0));
		double um = pow(exp(basepar[2]), (1.0/3.0));
		double us = pow(exp(basepar[3]), (1.0/3.0));

		// Bound the correlations between -1,1
		arma::vec rho = NumericVector::create(basepar[4]);
		rho = exp(rho);
		rho = (rho/(1 + rho))*2 - 1;

		// Translate um and us to shape and scale parameters for gamma distribution
		double k = pow(um,2) / pow(us,2);
		double t = pow(us,2) / um;

		// Now we need to correlate the distributions
		arma::vec means(2);
		means(0) = rm;
		means(1) = um;
		NumericVector sd = NumericVector::create(rs, us);

		// How big is correlation matrix
		int z = .5* ( sqrt(8*rho.n_elem + 1) + 1);

		arma::mat RHO = fillcorr(rho);
		arma::mat sigma(z,z,arma::fill::eye);

		// Fill out the covariance matrix
		for(int row = 0; row < z ; row++){
			for(int col = 0; col < z ; col++){
				sigma(row,col) = sd[row] * sd[col] * RHO(row,col);
			}
		}

		CH(_,0) = h1(n,_);
		CH(_,1) = h2(n,_);

		// Correlate our random uniform variables
		CH = mvhnormInv(CH, means, sigma ) ;
		
		// Generate the real variables by inverting the uniform variables
		r  = qnorm(CH(_,0),rm,rs);
		r = 1.0 - r;
		mu = qgamma(CH(_,1),k,t);

		List inst = Inst[n];

		NumericMatrix A      = inst["A"];
		NumericMatrix B      = inst["B"];
		NumericMatrix pA     = inst["pA"];
		NumericMatrix pB     = inst["pB"];
		NumericVector choice = inst["choice"];
		NumericVector Max    = inst["Max"];
		NumericVector Min    = inst["Min"];

		int rnum = A.nrow();
		int cnum = A.ncol();

		NumericVector UA(rnum);
		NumericVector UB(rnum);

		NumericMatrix sim(rnum, h);

		double ri;
		// Now loop through the halton draws and calculate
		for(int i = 0; i < h ; i++){

			ri = r[i];

			// Calculate the context
			ctx = (pow(Max, ri) / ri) - (pow(Min, ri) / ri);

			// Calculate the utility of the lotteries
			for(int j = 0; j < cnum; j++){
				if (j == 0){
					UA = (pA(_,j) * pow(A(_,j), ri) / ri);
					UB = (pB(_,j) * pow(B(_,j), ri) / ri);
				}
				else{
					UA = UA + (pA(_,j) * pow(A(_,j), ri) / ri);
					UB = UB + (pB(_,j) * pow(B(_,j), ri) / ri);
				}
			}
			
			// Re-base utility of B and add in context and fechner
			UB1  = (UB-UA)/ctx/mu[i];

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
		
		arma::mat simArma = Rcpp::as<arma::mat>(sim);

		simprob = prod(simArma,0);

		double spb = mean(simprob);

		subprob[n] = spb;

	}

	NumericVector sl = log(subprob);
	return(-sum(sl));

}

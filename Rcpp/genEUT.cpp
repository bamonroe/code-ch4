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

// Vector Power
arma::mat vpow(const arma::vec base, const arma::vec exp) {
	arma::vec out(base.size());
	std::transform(base.begin(), base.end(),
                 exp.begin(), out.begin(), ::pow);
  return out;
}

// Multivariate normal inversion.
// Pass in unfiform distributed matrix, vector of means, and covaiance matirx, get back
// correlated normal distirbuted matrix
NumericMatrix mvrnormInv(int n, arma::vec mu, arma::mat sigma) {

	int ncols = sigma.n_cols;
	arma::mat Y = arma::randn(n, ncols);
	arma::mat RES = arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma);

	NumericMatrix OUT(n, ncols);

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
arma::vec crra(arma::vec x, arma::vec r){
	return(vpow(x, 1 - r) / 1- r);
}

arma::mat getPrefs(double rm, double rs, double um, double us, arma::vec rho, int N){

	// set ourselves up to use correlations
	arma::vec means(2);
	means(0) = rm;
	means(1) = um;
	arma::vec sd(2);
	sd(0) = rs;
	sd(1) = us;

	// Translate um and us to shape and scale parameters for gamma distribution
	double k = pow(um,2) / pow(us,2);
	double t = pow(us,2) / um;

	// How big is correlation matrix
	int z = .5* ( sqrt(8*rho.n_elem + 1) + 1);

	arma::mat RHO = fillcorr(rho);
	arma::mat sigma(z,z,arma::fill::eye);

	// Fill out the covariance matrix
	for(int row = 0; row < z ; row++){
		for(int col = 0; col < z ; col++){
			sigma(row,col) = sd(row) * sd(col) * RHO(row,col);
		}
	}

	// Correlate our random uniform variables
	NumericMatrix dists = mvrnormInv(1, means, sigma ) ;
	
	// Generate the real variables by inverting the uniform variables
	NumericVector r  = qnorm(dists(_,0),rm,rs);
	NumericVector mu = qgamma(dists(_,1),k,t);

	arma::mat out(1,2);
	out(0,0) = r(0);
	out(0,1) = mu(0);

	return(out);

}

// [[Rcpp::export]]
arma::mat genEUTcpp( arma::vec par, List dempars, int N, arma::mat A, arma::mat B, 
					     arma::mat pA, arma::mat pB, arma::vec Min, arma::vec Max){

	int dbug=0;

	// How big is the instrument
	int isize = A.n_rows;
	// How many columns of the final matrix are needed
	int cnum = A.n_cols + pA.n_cols + B.n_cols + pB.n_cols + 4 + dempars.size() ;
	// How many rows
	int rnum = isize * N;

	arma::mat out(rnum,cnum);
	// Per subject matix
	//arma::mat per(rnum,cnum);

	double rm  = par[0]; // CRRA Mean
	double rs  = par[1]; // CRRA Standard Deviation
	double um  = par[2]; // Fechner Mean
	double us  = par[3]; // Fechner Standard Deviation
	arma::vec rho(1);
	rho(0) = par(4); // Rho

	// Translate um and us to shape and scale parameters for gamma distribution
	double k = pow(um,2) / pow(us,2);
	double t = pow(us,2) / um;

	// columns containing individual distirbutional parameters
	arma::mat rmmat(N,1);
	arma::mat rsmat(N,1);
	arma::mat ummat(N,1);
	arma::mat usmat(N,1);

	// Make the instrument matrix
	arma::mat inst = join_rows(A, pA);
			  inst = join_rows(inst, B);
			  inst = join_rows(inst, pB);
			  inst = join_rows(inst, Max);
			  inst = join_rows(inst, Min);

	// columns to be bound to the instrument
	arma::mat rmat(isize,1);
	arma::mat mumat(isize,1);
	arma::mat idmat(isize,1);

	arma::mat prefs(1,2);

	// First subject
	prefs = getPrefs(rm, rs, um, us, rho, N);
	rmat.fill(prefs(0,0));
	mumat.fill(prefs(0,1));
	idmat.fill(1);

	arma::mat per = join_rows(inst, join_rows(rmat, mumat));
	per = join_rows(per, idmat);

	// Output matirx
	out = per;

	for(int i =1 ; i < N ; i++){

		prefs = getPrefs(rm, rs, um, us, rho, N);
	
		rmat.fill(prefs(0,0));
		mumat.fill(prefs(0,1));
		idmat.fill(i+1);

		per = join_rows(inst,  join_rows(rmat, mumat));
		per = join_rows(per , idmat);

		out = join_cols(out,per);
		//out.insert_rows( (i +1)*isize - 1, per );
		
	}

	int rpos  = A.n_cols * 4 + 2;
	int mupos  = A.n_cols * 4 + 3;

	arma::vec rcol = out.col(rpos);
	arma::vec MUCOL = out.col(mupos);
	NumericVector mucol = wrap(MUCOL);

	// Get the contextual utility
	arma::vec CTX = crra(out.col(A.n_cols *4), rcol) - crra(out.col(A.n_cols *4 + 1), rcol);

	NumericVector ctx = wrap(CTX);

	arma::vec ua = arma::zeros<arma::vec>(out.n_rows);
	arma::vec ub = arma::zeros<arma::vec>(out.n_rows);

	for(int i = 0 ; i < A.n_cols ; i++ ){

		//A pA B pB Max Min r mu
		//i  j k  l m   n
		
		int j = A.n_cols     + i;
		int k = A.n_cols * 2 + i;
		int l = A.n_cols * 3 + i;
	
		ua = ua + (out.col(j) % crra(out.col(i), rcol));
		ub = ub + (out.col(l) % crra(out.col(k), rcol));
	}

	NumericVector UA = wrap(ua);
	NumericVector UB = wrap(ub);

	// Re-base utility of B and add in context and fechner
	NumericVector UB1  = (UB-UA) / ctx / mucol ;

	// If we have no issues, this is the choice probability of A
	NumericVector PA = (1.0 / (1.0 + exp(UB1)));

	// Are we dealing with an insane number?
	// yes
	NumericVector N0 = ifelse((UB) >(UA) , 0.0 , 1.0 ); 
	// no, but are we making an insane number via exp?
	NumericVector N1 = ifelse( UB1 > 709.0, 0.0 ,(PA) );

	// Check for the 2 issues, and return the probability of A
	NumericVector Aprob = ifelse( is_na(PA) , N0 , N1);

	// Making pB = 1-pA saves us the exponential calculations - it's faster
	NumericVector Bprob = 1.0 - Aprob;

	// Generate a random number for choosing
	NumericVector rand = runif(out.n_rows);

	NumericVector choice = ifelse( Aprob > rand, 0.0, 1.0);

	arma::vec c = as<arma::colvec>(choice);

	out.insert_cols( out.n_cols, c );

	return(out);
}

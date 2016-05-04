#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
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
double MSL_RDU(NumericVector par, NumericMatrix h1, NumericMatrix h2, NumericMatrix h3, NumericMatrix h4,
					NumericVector A0, NumericVector A1, NumericVector B0, NumericVector B1,
					NumericVector pA0, NumericVector pA1, NumericVector pB0, NumericVector pB1,
					NumericVector max, NumericVector min, NumericVector c){
    

	double rm = par[0];
	double rs = exp(par[1]);
	double um = exp(par[2]);
	double us = exp(par[3]);

	double am = exp(par[4]);
	double as = exp(par[5]);

	double bm = exp(par[6]);
	double bs = exp(par[7]);

	double uk = pow(um,2) / pow(us,2);
	double ut = pow(us,2) / um;

	double ak = pow(am,2) / pow(as,2);
	double at = pow(as,2) / am;

	double bk = pow(bm,2) / pow(bs,2);
	double bt = pow(bs,2) / bm;

	int rnum = h1.nrow();
	int h = h1.ncol();

	NumericMatrix r(rnum,h);
	NumericMatrix mu(rnum,h);
	NumericMatrix alpha(rnum,h);
	NumericMatrix beta(rnum,h);

	NumericVector wA1(rnum);
	NumericVector wA0(rnum);
	NumericVector wB1(rnum);
	NumericVector wB0(rnum);

	NumericVector ctx(rnum);
	NumericVector UA(rnum);
	NumericVector UB(rnum);
	NumericVector UB1(rnum);
	NumericVector PA(rnum);

	NumericVector N0(rnum);
	NumericVector N1(rnum);
	NumericVector pA(rnum);

	NumericMatrix sim(rnum,h);

	for(int i = 0; i < h ; i++){

		r(_,i)  = qnorm(h1(_,i),rm,rs);
		mu(_,i) = qgamma(h2(_,i),uk,ut);
		alpha(_,i) = qgamma(h3(_,i),ak,at);
		beta (_,i) = qgamma(h4(_,i),bk,bt);

		wA1 = pw(pA1,alpha(_,i),beta(_,i));
		wA0 = pw(pA0 + pA1,alpha(_,i),beta(_,i)) - pw(pA1,alpha(_,i),beta(_,i));

		wB1 = pw(pB1,alpha(_,i),beta(_,i));
		wB0 = pw(pB0 + pB1,alpha(_,i),beta(_,i)) - pw(pB1,alpha(_,i),beta(_,i));

		// Calculate the context
		ctx = vpow(max,(1-r(_,i)))/(1-r(_,i)) - vpow(min,(1-r(_,i)))/(1-r(_,i));

		// Calculate the utility of the lotteries
		UA = (wA0 * vpow(A0,(1-r(_,i)))/(1-r(_,i))) + (wA1 * vpow(A1,(1-r(_,i)))/(1-r(_,i)));
		UB = (wB0 * vpow(B0,(1-r(_,i)))/(1-r(_,i))) + (wB1 * vpow(B1,(1-r(_,i)))/(1-r(_,i)));

		// Re-base utility of B and add in context and fechner
		UB1  = (UB/ctx/mu(_,i)) - (UA/ctx/mu(_,i));

		// If we have no issues, this is the choice probability of A
		PA = (1 / (1 + exp(UB1)));

		// Are we dealing with an insane number?
		// yes
		N0 = ifelse( UB > UA , 0 , 1 ); 
		// no, but are we making an insane number via exp?
		N1 = ifelse( UB1 > 709 , 0 , PA );

		// Check for the 2 issues, and return the probability of A
		pA = ifelse( is_na(UB1) , N0 , N1);

		// Making pB = 1-pA saves us the exponential calculations - it's faster
		//pB = 1 - pA;

		// Grab the choice probability for the chosen option
		sim(_,i) = ifelse(c==0,pA,(1-pA));

	}

	NumericVector sl(rnum);

	for(int i = 0; i < rnum ; i++){
		sl[i] = mean(sim(i,_));
	}

	sl = log(sl);
	return(-sum(sl));

}

// [[Rcpp::export]]
double MSL_EUT_LN(NumericVector par, NumericMatrix h1, NumericMatrix h2,
					NumericMatrix A, NumericMatrix B,
					NumericMatrix pA,NumericMatrix pB,
					NumericVector max, NumericVector min, NumericVector c){
    
	double rm = par[0];
	double rs = exp(par[1]);
	double r_stretch = exp(par[2]);
	double r_shift = par[3];

	double um = par[4];
	double us = exp(par[5]);
	double mu_stretch = exp(par[6]);

	int rnum = h1.nrow();
	int h = h1.ncol();

	int pnum = A.ncol();

	NumericMatrix r(rnum,h);
	NumericMatrix mu(rnum,h);

	NumericVector ctx(rnum);
	NumericVector UA(rnum);
	NumericVector UB(rnum);
	NumericVector UB1(rnum);
	NumericVector PA(rnum);

	NumericVector N0(rnum);
	NumericVector N1(rnum);
	NumericVector prA(rnum);

	NumericMatrix sim(rnum,h);

	for(int i = 0; i < h ; i++){

		// Construct logit-normal distributions
		r(_,i)  = exp(qnorm(h1(_,i),rm,rs));
		mu(_,i) = exp(qnorm(h2(_,i),um,us));

		r(_,i)  = r(_,i) / (1 + r(_,i));
		mu(_,i) = mu(_,i) / (1 + mu(_,i));

		r(_,i)  = r(_,i)*r_stretch + r_shift;
		mu(_,i) = mu(_,i)*mu_stretch;

		// Calculate the context
		ctx = vpow(max,(1-r(_,i)))/(1-r(_,i)) - vpow(min,(1-r(_,i)))/(1-r(_,i));

		// Calculate the utility of the lotteries
		UA = (pA(_,0) * vpow(A(_,0),(1-r(_,i)))/(1-r(_,i))) ;
		UB = (pB(_,0) * vpow(B(_,0),(1-r(_,i)))/(1-r(_,i))) ;

		for(int j = 1; j < pnum ; j++ ){
			UA = UA + (pA(_,j) * vpow(A(_,j),(1-r(_,i)))/(1-r(_,i))) ;
			UB = UB + (pB(_,j) * vpow(B(_,j),(1-r(_,i)))/(1-r(_,i))) ;
		}

		// Re-base utility of B and add in context and fechner
		UB1  = (UB/ctx/mu(_,i)) - (UA/ctx/mu(_,i));

		// If we have no issues, this is the choice probability of A
		PA = (1 / (1 + exp(UB1)));

		// Are we dealing with an insane number?
		// yes
		N0 = ifelse( UB > UA , 0 , 1 ); 
		// no, but are we making an insane number via exp?
		N1 = ifelse( UB1 > 709 , 0 , PA );

		// Check for the 2 issues, and return the probability of A
		prA = ifelse( is_na(UB1) , N0 , N1);

		// Making pB = 1-pA saves us the exponential calculations - it's faster
		NumericVector pB = 1 - prA;

		// Grab the choice probability for the chosen option
		sim(_,i) = ifelse(c==0,prA,pB);

	}

	NumericVector sl(rnum);

	for(int i = 0; i < rnum ; i++){
		sl[i] = mean(sim(i,_));
	}

	sl = log(sl);
	return(-sum(sl));

}

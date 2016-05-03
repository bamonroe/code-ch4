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
double MSL_EUT(NumericVector par, NumericMatrix h1, NumericMatrix h2,
					NumericMatrix A, NumericMatrix B,
					NumericMatrix pA, NumericMatrix pB,
					NumericVector Max, NumericVector Min, NumericVector choice){
    

	double rm = par[0];
	//double rs = std::abs(par[1]);
	//double um = std::abs(par[2]);
	//double us = std::abs(par[3]);
	double rs =pow(exp(par[1]), (1.0/3.0));
	double um =pow(exp(par[2]), (1.0/3.0));
	double us =pow(exp(par[3]), (1.0/3.0));

	double k = pow(um,2) / pow(us,2);
	double t = pow(us,2) / um;

	int rnum = h1.nrow();
	int h = h1.ncol();

	NumericMatrix r(rnum,h);
	NumericMatrix mu(rnum,h);

	NumericVector ctx(rnum);

	NumericVector pzA(A.nrow());
	NumericVector pzB(A.nrow());
	NumericVector prA(A.nrow());
	NumericVector prB(A.nrow());

	NumericVector UA(rnum);
	NumericVector UB(rnum);
	NumericVector UB1(rnum);
	NumericVector PA(rnum);

	NumericVector N0(rnum);
	NumericVector N1(rnum);
	NumericVector Aprob(rnum);
	NumericVector Bprob(rnum);

	NumericMatrix sim(rnum,h);

	for(int i = 0; i < h ; i++){

		r(_,i)  = qnorm(h1(_,i),rm,rs);
		mu(_,i) = qgamma(h2(_,i),k,t);

		// Calculate the context
		ctx = vpow(Max,(1-r(_,i)))/(1-r(_,i)) - vpow(Min,(1-r(_,i)))/(1-r(_,i));

		// Calculate the utility of the lotteries
		UA.fill(0);
		UB.fill(0);

		for(int j = 0; j < A.ncol(); j++){
			pzA = A(_,j);
			pzB = B(_,j);
			prA = pA(_,j);
			prB = pB(_,j);

			UA = UA + (prA * vpow(pzA,(1-r(_,i)))/(1-r(_,i)));
			UB = UB + (prB * vpow(pzB,(1-r(_,i)))/(1-r(_,i)));
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
		Aprob = ifelse( is_na(UB1) , N0 , N1);

		// Making pB = 1-pA saves us the exponential calculations - it's faster
		NumericVector Bprob = 1 - Aprob;

		// Grab the choice probability for the chosen option
		sim(_,i) = ifelse(choice==0, Aprob, Bprob);

	}

	NumericVector sl(rnum);

	for(int i = 0; i < rnum ; i++){
		sl[i] = mean(sim(i,_));
	}

	sl = log(sl);
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

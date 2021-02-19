data{
	int<lower=1> D;						    
  int<lower=1> Im;
  int<lower=1> Jm;
  int<lower=1> Nm;
  int<lower=1> iim[Nm];
  int<lower=1> jjm[Nm];
  int<lower=0,upper=1>Ym[Nm];
  
  int<lower=1> It;
  int<lower=1> Jt;
  int<lower=1> Nt;
  int<lower=1> iit[Nt];
  int<lower=1> jjt[Nt];
  int<lower=0,upper=1>Yt[Nt];
  
  int<lower=1> Ie;
  int<lower=1> Je;
  int<lower=1> Ne;
  int<lower=1> iie[Ne];
  int<lower=1> jje[Ne];
  int<lower=0,upper=1>Ye[Ne];
  
  real lkj_theta;
  real a_mu;
  real a_sd;
  real d_mu;
  real d_sd;
}

transformed data{       
  row_vector [D] mu_theta = rep_row_vector(0.0, D);
  row_vector [D] sigma_theta = rep_row_vector(1.0, D);
}

parameters{
  vector [D] theta [Im];
  corr_matrix[D] theta_corr;

  vector<lower=0>[Jm] am;
  vector<lower=0>[Jt] at;
  vector<lower=0>[Je] ae;
  vector [Jm] dm;
  vector [Jt] dt;
  vector [Je] de;
} 

model{
    target += cauchy_lpdf(am | a_mu, a_sd);
    target += cauchy_lpdf(at | a_mu, a_sd);
    target += cauchy_lpdf(ae | a_mu, a_sd);
    target += cauchy_lpdf(dm | d_mu, d_sd);
    target += cauchy_lpdf(dt | d_mu, d_sd);
    target += cauchy_lpdf(de | d_mu, d_sd);

    target += lkj_corr_lpdf(theta_corr | lkj_theta);
    target += multi_normal_lpdf(theta | mu_theta, quad_form_diag(theta_corr, sigma_theta));
    
  for(nm in 1:Nm){
	  	target += bernoulli_logit_lpmf(Ym[nm] | am[jjm[nm]] * theta[iim[nm],1] - dm[jjm[nm]]); 
  }
  
  for(nt in 1:Nt){
	  	target += bernoulli_logit_lpmf(Yt[nt] | at[jjt[nt]] * theta[iit[nt],2] - dt[jjt[nt]]); 
  }
  
  for(ne in 1:Ne){
	  	target += bernoulli_logit_lpmf(Ye[ne] | ae[jje[ne]] * theta[iie[ne],3] - de[jje[ne]]); 
  }
}


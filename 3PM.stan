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
  cholesky_factor_corr [D] lower_chol;

  vector<lower=0>[Jm] am;
  vector<lower=0>[Jt] at;
  vector<lower=0>[Je] ae;
  vector [Jm] dm;
  vector [Jt] dt;
  vector [Je] de;
} 

model{
    am ~ cauchy(a_mu, a_sd);
    at ~ cauchy(a_mu, a_sd);
    ae ~ cauchy(a_mu, a_sd);
    dm ~ cauchy(d_mu, d_sd);
    dt ~ cauchy(d_mu, d_sd);
    de ~ cauchy(d_mu, d_sd);

    lower_chol ~ lkj_corr_cholesky(lkj_theta);
    theta ~ multi_normal_cholesky(mu_theta, diag_pre_multiply(sigma_theta, lower_chol));
    
    for(nm in 1:Nm){
      Ym[nm] ~ bernoulli_logit(am[jjm[nm]] * theta[iim[nm],1] - dm[jjm[nm]]);
    }
      
    for(nt in 1:Nt){
      Yt[nt] ~ bernoulli_logit(at[jjt[nt]] * theta[iit[nt],2] - dt[jjt[nt]]);
    }
    
    for(ne in 1:Ne){
      Ye[ne] ~ bernoulli_logit(ae[jje[ne]] * theta[iie[ne],3] - de[jje[ne]]);
    }
}

generated quantities{
  corr_matrix [D] theta_corr;
//  vector [Jm] bm;
//  vector [Jt] bt;
//  vector [Je] be;
  
  theta_corr = multiply_lower_tri_self_transpose(lower_chol);

//  bm[] = -dm[]./am[];
//  bt[] = -dt[]./at[];
//  be[] = -de[]./ae[];
}


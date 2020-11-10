
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=T)
library(mvtnorm)
library(bayesplot)
setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20")
#setwd("C:\\Users\\ajm045\\Dropbox\\6553_ESRM-AdvancedMultivariate\\Project\\Code_Final")
ThreePM=stan_model(file="3PM2.stan")

# N=150
# J=10
# seed=11
# n=1
#                                                        3000x3x2x2=36000  + 120x3x2x2+50x3x2x2=2040x3=6120
#                                                        1500x3x2x2=18000
#                                                         750x3x2x2= 9000                                   69156
nitems_list = list(20)
npers_list = list(3000)

##############################################################################
###### Data Generation ######
### returns list for Stan ###

sim_Bayes=function(nsim){
  for(J in nitems_list){
    for(N in npers_list){
      for(n in 1:nsim){
        t_start=proc.time()
        set.seed(n)
        theta=rmvnorm(N,mean=c(0,0,0),sigma=matrix(c( 1.00,0.50,-0.50,                # MOD CORRELATION
                                                      0.50,1.00, 0.50,
                                                     -0.50,0.50, 1.00),
                                                   ncol=3,nrow=3,byrow=T))  
        am=runif(J,0.50,3.0)
        at=runif(J,0.50,3.0)
        ae=runif(J,0.50,3.0)
        
        min=-2.5; max=2.5;                  # truncation of b_t and b_e
#       min_m=-0.3; max_m=1.2; mean_m=0.45                                      # 38.0% - High PROPORTION MRS (approximately normal)
#       min_m=0.6; max_m=2.1; mean_m=1.35                                       # 20.0% - Mid PROPORTION MRS (approximately uniform)
        min_m=1.25; max_m=2.75; mean_m=2.0                                      # 11.0% - Low PROPORTION MRS (MACH-V)
        bm=qnorm(pnorm(min_m,mean_m,sqrt(0.35))+runif(J)*(pnorm(max_m,mean_m,sqrt(0.35))-pnorm(min_m,mean_m,sqrt(0.35))),mean_m,sqrt(0.35))
        bt=qnorm(pnorm(min,0,sqrt(1.5))+runif(J)*(pnorm(max,0,sqrt(1.5))-pnorm(min,0,sqrt(1.5))),0,sqrt(1.5))  # N(0,sqrt(1.5)) trunc[-2.5,2.5]
        be=qnorm(pnorm(min,0,sqrt(1.5))+runif(J)*(pnorm(max,0,sqrt(1.5))-pnorm(min,0,sqrt(1.5))),0,sqrt(1.5))

        dm = bm*am
        dt = bt*at
        de = be*ae
        
        mprob = matrix(0,nrow=N,ncol=J)
        tprob = matrix(0,nrow=N,ncol=J)
        eprob = matrix(0,nrow=N,ncol=J)
        for (i in 1:N){
          for (j in 1:J){
            mprob[i,j] = plogis(am[j] * theta[i,1] - dm[j])
            tprob[i,j] = plogis(at[j] * theta[i,2] - dt[j])
            eprob[i,j] = plogis(ae[j] * theta[i,3] - de[j])
          }
        }
        
        mthresh=matrix(runif(N*J),N,J); tthresh=matrix(runif(N*J),N,J); ethresh=matrix(runif(N*J),N,J)
        ym=as.data.frame(ifelse(mprob[,1:J] > mthresh[,1:J],1,0))
        yt=as.data.frame(ifelse(tprob[,1:J] > tthresh[,1:J],1,0))
        ye=as.data.frame(ifelse(eprob[,1:J] > ethresh[,1:J],1,0))
        m=reshape(ym, idvar = "mid", timevar = "mitem", varying = list(names(ym)[1:J]), v.names = "y_m", direction = "long")
        t=reshape(yt, idvar = "tid", timevar = "titem", varying = list(names(yt)[1:J]), v.names = "y_t", direction = "long")
        e=reshape(ye, idvar = "eid", timevar = "eitem", varying = list(names(ye)[1:J]), v.names = "y_e", direction = "long")
        t$y_t = as.numeric(ifelse(m$y_m == 1, NA, t$y_t))
        e$y_e = as.numeric(ifelse(m$y_m == 1, NA, e$y_e))
        t = na.omit(t); e = na.omit(e)
      
        d1=list(D=3,
                Jm=max(m$mitem), Im=max(m$mid), Nm=max(m$mid)*max(m$mitem),
                iim=m$mid, jjm=m$mitem, Ym=m$y_m,
                Jt=max(t$titem), It=max(t$tid), Nt=nrow(t), 
                iit=t$tid, jjt=t$titem, Yt=t$y_t,
                Je=max(e$eitem), Ie=max(e$eid), Ne=nrow(e),
                iie=e$eid, jje=e$eitem, Ye=e$y_e,
                lkj_theta=1, a_mu=0, a_sd=2.5, d_mu=0, d_sd=2.5)

        fit=sampling(ThreePM,
                     seed=11,
                     data=d1,
                     chains=4,
                     cores=4,
                     warmup=150,
                     iter=1000,
                     pars=c("lower_chol"),
                     include=F,
                     refresh=-1)

        sum=summary(fit, pars=(c(paste0("theta[",1:N,",1]"), paste0("theta[",1:N,",2]"), paste0("theta[",1:N,",3]"),
                                 paste0("am[",1:J,"]"), paste0("at[",1:J,"]"), paste0("ae[",1:J,"]"),
                                 paste0("dm[",1:J,"]"), paste0("dt[",1:J,"]"), paste0("de[",1:J,"]"),
                                 "theta_corr[1,2]", "theta_corr[1,3]", "theta_corr[2,3]")),
                               probs=c(.025,.20,.80,.975))$summary
        rownames(sum) <- c()
        tru=c(mrs=theta[,1], toi=theta[,2], ers=theta[,3],
              a_m=am[1:J], a_t=at[1:J], a_e=ae[1:J],
              d_m=bm[1:J], d_t=bt[1:J], d_e=be[1:J],
              r_mt=0.20, r_me=-0.20, r_te=0.20)                                   # SPECIFY TRUE CORRELATIONS (LOW)
#             r_mt=0.50, r_me=-0.50, r_mt=0.50)                                   # SPECIFY TRUE CORRELATIONS (HIGH)

        result=data.frame(sum,tru,nsim=n,pers=N,items=J,MRS_prop="low",theta_corr="mod",est="Bayes")
        rownames(result) <- c(paste0("mrs",1:N),paste0("toi",1:N),paste0("ers",1:N),
                              paste0("a_m",1:J),paste0("a_t",1:J),paste0("a_e",1:J),
                              paste0("d_m",1:J),paste0("d_t",1:J),paste0("d_e",1:J),"r_mt","r_me","r_te")

        if(n == 1 & J==nitems_list[1] & N==npers_list[1]){
          results=result
        } else {
          results=rbind(results,result)
        }
        time=proc.time()-t_start
        writeLines(paste("        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        writeLines(" ")
        writeLines(paste("               ~~~~~ N =", N, ", J =", J, "~~~~~"))
        writeLines(" ")
        writeLines(paste("             ~~~~~ ITERATION =", n, "COMPLETE ~~~~~"))
        writeLines(" ")
        writeLines(paste("                  ~~~~~ TIME", time, "~~~~~"))
        writeLines(paste("        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        }
      }
    }
  return(results)
}


sim_out = sim_Bayes(nsim=200)
colnames(sim_out) <- c("mean","se_mean","sd","p025","p20","p80","p975","ESS","Rhat","true","nsim","pers","items","MRS_prop","theta_corr","est")
write.csv(sim_out,"mod-corr_low-MRS_3000_20.csv")

hist(bm)
mean(m$y_m)
min(bm)
max(bm)
mean(bm)
sd(bm)

plogis(-2)

head(sum)
table(sim_out$nsim)
table(sim_out$items)
table(sim_out$pers)

results$bias=results$mean-results$true
results$bias2=results$bias^2
results$abs_bias=abs(results$bias)
results$abs_relbias=abs(results$bias)/results$tru

item_parms=results[(N*3+1):nrow(results),]

print(fit, pars=c(paste0("am")), digits=3)  
print(fit, pars=c(paste0("at")), digits=3)  
print(fit, pars=c(paste0("ae")), digits=3)  
print(fit, pars=c(paste0("dm")), digits=3)  
print(fit, pars=c(paste0("dt")), digits=3)  
print(fit, pars=c(paste0("de")), digits=3) 
print(fit, pars=c("theta_corr[1,2]","theta_corr[1,3]","theta_corr[2,3]")) # MRS-TOI; MRS-ERS; TOI-ERS

plot(fit, plotfun = "rhat", pars = c(paste0("theta[",1:N,",1]"))) + ggtitle("MRS Theta Rhat")
plot(fit, plotfun = "rhat", pars = c(paste0("theta[",1:N,",2]"))) + ggtitle("TOI Theta Rhat")
plot(fit, plotfun = "rhat", pars = c(paste0("theta[",1:N,",3]"))) + ggtitle("ERS Theta Rhat")

plot(fit, plotfun = "mcse", pars = c("am","at","ae","dm","dt","dm"))
plot(fit, plotfun = "mcse", pars = c("theta"))

plot(fit, plotfun = "dens", pars = c(paste0("am")), separate_chains = T, nrow = 2) + ggtitle("MRS Discrimination")
plot(fit, plotfun = "dens", pars = c(paste0("at")), separate_chains = T, nrow = 2) + ggtitle("TOI Discrimination")
plot(fit, plotfun = "dens", pars = c(paste0("ae")), separate_chains = T, nrow = 2) + ggtitle("ERS Discrimination")
plot(fit, plotfun = "dens", pars = c(paste0("dm")), separate_chains = T, nrow = 2) + ggtitle("MRS Difficulty")
plot(fit, plotfun = "dens", pars = c(paste0("de")), separate_chains = T, nrow = 2) + ggtitle("TOI Difficulty")
plot(fit, plotfun = "dens", pars = c(paste0("dm")), separate_chains = T, nrow = 2) + ggtitle("ERS Difficulty")

plot(fit, plotfun = "trace",pars = c(paste0("am")), inc_warmup = T, nrow = 5) + ggtitle("MRS Discrimination")
plot(fit, plotfun = "trace",pars = c(paste0("at")), inc_warmup = T, nrow = 5) + ggtitle("TOI Discrimination")
plot(fit, plotfun = "trace",pars = c(paste0("ae")), inc_warmup = T, nrow = 5) + ggtitle("ERS Discrimination")
plot(fit, plotfun = "trace",pars = c(paste0("dm")), inc_warmup = T, nrow = 5) + ggtitle("MRS Difficulty")
plot(fit, plotfun = "trace",pars = c(paste0("dt")), inc_warmup = T, nrow = 5) + ggtitle("TOI Difficulty")
plot(fit, plotfun = "trace",pars = c(paste0("de")), inc_warmup = T, nrow = 5) + ggtitle("ERS Difficulty")

plot(fit, plotfun = "ac", pars = c(paste0("am")), separate_chains = F, nrow = 5) + ggtitle("MRS Discrimination")
plot(fit, plotfun = "ac", pars = c(paste0("at")), separate_chains = F, nrow = 5) + ggtitle("TOI Discrimination")
plot(fit, plotfun = "ac", pars = c(paste0("ae")), separate_chains = F, nrow = 5) + ggtitle("ERS Discrimination")
plot(fit, plotfun = "ac", pars = c(paste0("dm")), separate_chains = F, nrow = 5) + ggtitle("MRS Difficulty")
plot(fit, plotfun = "ac", pars = c(paste0("dt")), separate_chains = F, nrow = 5) + ggtitle("TOI Difficulty")
plot(fit, plotfun = "ac", pars = c(paste0("de")), separate_chains = F, nrow = 5) + ggtitle("ERS Difficulty")

mcmc_rank_hist(fit, pars = c(paste0("am[",1:(J/2),"]")), ref_line = T) + ggtitle("MRS Discrimination")
mcmc_rank_hist(fit, pars = c(paste0("am[",(J/2+1):J,"]")), ref_line = T) + ggtitle("MRS Discrimination")
mcmc_rank_hist(fit, pars = c(paste0("at[",1:(J/2),"]")), ref_line = T) + ggtitle("TOI Discrimination")
mcmc_rank_hist(fit, pars = c(paste0("at[",(J/2+1):J,"]")), ref_line = T) + ggtitle("TOI Discrimination")
mcmc_rank_hist(fit, pars = c(paste0("ae[",1:(J/2),"]")), ref_line = T) + ggtitle("ERS Discrimination")
mcmc_rank_hist(fit, pars = c(paste0("ae[",11:20,"]")), ref_line = T) + ggtitle("ERS Discrimination")
mcmc_rank_hist(fit, pars = c(paste0("dm[",(J/2+1):J,"]")), ref_line = T) + ggtitle("MRS Difficulty")
mcmc_rank_hist(fit, pars = c(paste0("dm[",11:20,"]")), ref_line = T) + ggtitle("MRS Difficulty")
mcmc_rank_hist(fit, pars = c(paste0("dt[",(J/2+1):J,"]")), ref_line = T) + ggtitle("TOI Difficulty")
mcmc_rank_hist(fit, pars = c(paste0("dt[",11:20,"]")), ref_line = T) + ggtitle("TOI Difficulty")
mcmc_rank_hist(fit, pars = c(paste0("de[",(J/2+1):J,"]")), ref_line = T) + ggtitle("ERS Difficulty")
mcmc_rank_hist(fit, pars = c(paste0("de[",11:20,"]")), ref_line = T) + ggtitle("ERS Difficulty")

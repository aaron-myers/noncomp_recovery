
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write=T)
library(mvtnorm)

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20")
ThreePM=stan_model(file="3PM2.stan")

##############################################################################
###### Simulation Function ######

sim_Bayes=function(nsim, r_, mrs_prop){
  for(J in nitems_list){
    for(N in npers_list){
      for(n in 1:nsim){
        
        if(r_=="low"){
          r_mt <- r_te <- 0.20
          r_me <- -1*r_mt
        }else{
          r_mt <- r_te <- 0.50
          r_me <- -1*r_mt
        }
        
        t_start=proc.time()
        set.seed(n)
        theta=rmvnorm(N,mean=c(0,0,0),sigma=matrix(c( 1.00,r_mt,r_me,                # MOD CORRELATION
                                                      r_mt,1.00,r_te,
                                                      r_me,r_te,1.00),
                                                   ncol=3,nrow=3,byrow=T))  
        am=runif(J,0.50,3.0)
        at=runif(J,0.50,3.0)
        ae=runif(J,0.50,3.0)
        
        min=-2.5; max=2.5;                  # truncation of b_t and b_e
        
        if(mrs_prop=="low"){
          min_m=1.25; max_m=2.75; mean_m=2.0                                      # 11.0% - Low PROPORTION MRS (MACH-V)
        }else if(mrs_prop=="mid"){
          min_m=0.6; max_m=2.1; mean_m=1.35                                       # 20.0% - Mid PROPORTION MRS (approximately uniform)
        }else{
          min_m=-0.3; max_m=1.2; mean_m=0.45                                      # 38.0% - High PROPORTION MRS (approximately normal)
        }

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
                     chains=1,#4,
                     cores=4,
                     warmup=50,#150,
                     iter=100,#1000,
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
              r_mt=r_mt, r_me=r_me, r_te=r_te)

        result=data.frame(sum,tru,nsim=n,pers=N,items=J,MRS_prop=mrs_prop,theta_corr=r_,est="Bayes")
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

R="low"                  # low = .20, mod = .50 
M_prop="low"             # low, mid, high
nitems_list = list(5)
npers_list = list(100)

sim_out = sim_Bayes(nsim=2, r_=R , mrs_prop=M_prop)
colnames(sim_out) <- c("mean","se_mean","sd","p025","p20","p80","p975","ESS","Rhat","true","nsim","pers","items","MRS_prop","theta_corr","est")

path = "C:/Users/Aaron/Dropbox/NonComp/New_10-20/Output"

write.csv(sim_out,paste0(path,"/",R,"-corr_",M_prop,"-MRS_",npers_list,"_",nitems_list,".csv"))

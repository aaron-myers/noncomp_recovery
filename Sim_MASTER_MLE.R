
options(mc.cores = parallel::detectCores())

library(mvtnorm)
library(sirt)
setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20")

##############################################################################
###### Simulate MLE ######

nitems_list <- list(20)
npers_list <- list(3000)

sim_MLE <- function(nsim){
  for(J in nitems_list){
    for(N in npers_list){
      for(n in 1:nsim){
        t_start <- proc.time()
        set.seed(n)
        theta <- rmvnorm(N,mean=c(0,0,0),sigma=matrix(c( 1.00,0.20,-0.20,                # LOW CORRELATION
                                                         0.20,1.00, 0.20,
                                                        -0.20,0.20, 1.00),
                                                      ncol=3,nrow=3,byrow=T))  
        am <- runif(J,0.50,3.0)
        at <- runif(J,0.50,3.0)
        ae <- runif(J,0.50,3.0)

        min <- -2.5; max <- 2.5;                  # truncation of b_t and b_e
#       min_m <- -0.3; max_m <- 1.2; mean_m <- 0.45                                      # 38.0% - High PROPORTION MRS (approximately normal)
#       min_m <- 0.6; max_m <- 2.1; mean_m <- 1.35                                       # 20.0% - Mid PROPORTION MRS (approximately uniform)
        min_m <- 1.25; max_m <- 2.75; mean_m <- 2.0                                      # 11.0% - Low PROPORTION MRS (MACH-V)
        bm <- qnorm(pnorm(min_m,mean_m,sqrt(0.35))+runif(J)*(pnorm(max_m,mean_m,sqrt(0.35))-pnorm(min_m,mean_m,sqrt(0.35))),mean_m,sqrt(0.35))
        bt <- qnorm(pnorm(min,0,sqrt(1.5))+runif(J)*(pnorm(max,0,sqrt(1.5))-pnorm(min,0,sqrt(1.5))),0,sqrt(1.5))     # N(0,sqrt(1.5)) trunc[-3,3]
        be <- qnorm(pnorm(min,0,sqrt(1.5))+runif(J)*(pnorm(max,0,sqrt(1.5))-pnorm(min,0,sqrt(1.5))),0,sqrt(1.5))

        dm <- bm*am
        dt <- bt*at
        de <- be*ae

        mprob <- matrix(0,nrow=N,ncol=J)
        tprob <- matrix(0,nrow=N,ncol=J)
        eprob <- matrix(0,nrow=N,ncol=J)
        for (i in 1:N){
          for (j in 1:J){
            mprob[i,j] <- plogis(am[j] * theta[i,1] - dm[j])
            tprob[i,j] <- plogis(at[j] * theta[i,2] - dt[j])
            eprob[i,j] <- plogis(ae[j] * theta[i,3] - de[j])
          }
        }

        mthresh <- matrix(runif(N*J),N,J); tthresh <- matrix(runif(N*J),N,J); ethresh <- matrix(runif(N*J),N,J)
        ym <- as.data.frame(ifelse(mprob[,1:J] > mthresh[,1:J],1,0)); colnames(ym) <- paste0("m",1:J)
        yt <- as.data.frame(ifelse(tprob[,1:J] > tthresh[,1:J],1,0)); colnames(yt) <- paste0("t",1:J)
        ye <- as.data.frame(ifelse(eprob[,1:J] > ethresh[,1:J],1,0)); colnames(ye) <- paste0("e",1:J)

        for (i in 1:N){
          for (j in 1:J){
            if (ym[i,j]==1){
              yt[i,j] <- NA
              ye[i,j] <- NA
            }
          }
        }

        Q <- matrix(0,nrow=J*3,ncol=3); Q[1:J,1] <- 1; Q[(J+1):(J*2),2] <- 1; Q[(2*J+1):(J*3),3] <- 1

        d1 <- cbind(ym,yt,ye)

        fit <- smirt(d1, Q, irtmodel="noncomp", est.a="2PL", a.lower=0, 
                  theta.k=seq(-6,6,len=20), est.corr=T,
                  max.increment=1, increment.factor=1, numdiff.parm=0.0001,
                  maxdevchange=0.1, globconv=0.001, maxiter=1000, msteps=4,
                  mstepconv=0.001)

        sum <- rbind(cbind(fit[["person"]][,3],fit[["person"]][,4]),
                     cbind(fit[["person"]][,5],fit[["person"]][,6]),
                     cbind(fit[["person"]][,7],fit[["person"]][,8]),
                     cbind(fit[["item"]][1:J,7], fit[["se.a"]][1:J,1]),
                     cbind(fit[["item"]][(J+1):(J*2),8], fit[["se.a"]][(J+1):(J*2),2]),
                     cbind(fit[["item"]][(J*2+1):(J*3),9], fit[["se.a"]][(J*2+1):(J*3),3]),
                     cbind((fit[["item"]][1:J,4]), fit[["se.b"]][1:J,1]),
                     cbind((fit[["item"]][(J+1):(J*2),5]), fit[["se.b"]][(J+1):(J*2),2]),
                     cbind((fit[["item"]][(J*2+1):(J*3),6]), fit[["se.b"]][(J*2+1):(J*3),3]),
                     cbind(c(fit[["cor.trait"]][1,2], fit[["cor.trait"]][1,3], fit[["cor.trait"]][2,3]),c(NA,NA,NA)))

        tru <- c(mrs=theta[,1], toi=theta[,2], ers=theta[,3],
                 a_m=am[1:J], a_t=at[1:J], a_e=ae[1:J],
                 d_m=bm[1:J], d_t=bt[1:J], d_e=be[1:J],
                 r_mt=0.20, r_me=-0.20, r_te=0.20)                                   # SPECIFY TRUE CORRELATIONS (LOW)
#                r_mt=0.50, r_me=-0.50, r_te=0.50)                                   # SPECIFY TRUE CORRELATIONS (HIGH)
        
        result <- data.frame(cbind(sum,p025=sum[,1]-1.96*sum[,2],p975=sum[,1]+1.96*sum[,2],tru,nsim=n,
                                   pers=N,items=J,MRS_prop="low",theta_corr="low",est="MLE"))
        if(n==1 & J==nitems_list[1] & N==npers_list[1]){
          results <- result
        } else {
          results <- rbind(results,result)
        }
        time <- proc.time()-t_start
        writeLines(paste("        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        writeLines(" ")
        writeLines(paste("                ~~~~~ N =", N, ", J =", J, "~~~~~"))
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

sim_out <- sim_MLE(nsim=2)
colnames(sim_out) <- c("mean","sd","p025","p975","true","nsim","pers","items","MRS_prop","theta_corr","est")

write.csv(sim_out,"low-MRS_3000_20.csv")




(prop=round(colMeans(ym),3))
mean(prop)
mean(bm)
min(bm)
max(bm)
sd(bm)
hist(bm)

x3=bm

x=c(x1,x2,x3)
hist(x)

(low=pnorm(-1.5,0,1)-pnorm(-Inf,0,1))     # 7%
(midlow=pnorm(-.5,0,1)-pnorm(-1.5,0,1))   # 24%
(mid=pnorm(.5,0,1)-pnorm(-.5,0,1))        # 38%
(midhigh=pnorm(1.5,0,1)-pnorm(.5,0,1))    # 24%
(high=pnorm(Inf,0,1)-pnorm(1.5,0,1))      # 7%

x=c(rep(1,7),rep(5,7),rep(2,24),rep(4,24),rep(3,38))
hist(x)



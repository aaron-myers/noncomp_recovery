
library(stringr)
library(xlsx)
library(plyr)
library(data.table)

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/Output/MLE_OUTPUT")

####################################################################################
## OBTAIN FILE NAMES
# BAYES
M_files <- list.files(pattern="*.csv", full.names=T)

# READ FILES
# B_list <- sapply(B_files, fread, simplify=F) 
M_list <- lapply(M_files, fread, sep=",")

# CONVERT TO DATATABLE
# b1 <- do.call(rbind.data.frame, B_list); row.names(b1) <- NULL
m1 <- rbindlist(M_list, use.names=T, fill=T); row.names(m1) <- NULL
head(m1)

rm(M_files,M_list)

####################################################################################
## READ TRUE INTERCEPTS  (low-corr_low-MRS_750-1500-3000_10-20_Bayes.csv)
setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/Output/Intercepts")
int_files <- list.files(pattern="*.csv", full.names=T)
int_list <- lapply(int_files, fread, sep=",")
i1 <- rbindlist(int_list, use.names=T, fill=T); row.names(i1) <- NULL
head(i1); colnames(i1) <- c("V1","true2","nsim","pers","items","MRS_prop","theta_corr","est")

## MERGE TRUE INTERCEPTS WITH RESULTS
i1 <- i1[order(i1$pers,i1$items,i1$MRS_prop,i1$theta_corr,i1$nsim,i1$V1), ]
m1 <- m1[order(m1$pers,m1$items,m1$MRS_prop,m1$theta_corr,m1$nsim,m1$V1), ]
head(i1); head(m1)

####################################################################################
## RECOVERY INDICES

m1$bias <- m1$mean-m1$true
m1$absbias <- abs(m1$bias)
m1$bias2 <- m1$bias^2
m1$cov <- fifelse(m1$p025 < m1$true & m1$true < m1$p975,1,0)

## CORRELATION FUNCTION USED BELOW
corr_func <- function(X){
  return(data.frame(cor=cor(X$mean, X$true)))
}

## CORRELATION
# b1$r <- ifelse(str_detect(b1$V1,"r_m"),1,0)

####################################################################################
## SUMMARIZE THETA by SUBSET
mrs <- m1[str_detect(m1$V1,"mrs"), ]
toi <- m1[str_detect(m1$V1,"toi"), ]
ers <- m1[str_detect(m1$V1,"ers"), ]

########################################################################################################################################################################
#####  UNCONDITIONAL THETA  ############################################################################################################################################
########################################################################################################################################################################
####################################################################################
## MRS THETA
mrs_avgbias <- aggregate.data.frame(mrs$bias, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), 
                                    FUN=mean); colnames(mrs_avgbias) <- c("pers","items","prop","corr","avgbias")
mrs_avgabsbias <- aggregate.data.frame(mrs$absbias, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), 
                                       FUN=mean); colnames(mrs_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
mrs_avgbias2 <- aggregate.data.frame(mrs$bias2, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), 
                                     FUN=mean); colnames(mrs_avgbias2) <- c("pers","items","prop","corr","avgbias2")
mrs_cov <- aggregate.data.frame(mrs$cov, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), 
                                FUN=mean); colnames(mrs_cov) <- c("pers","items","prop","corr","cov")
mrs_se <- aggregate.data.frame(mrs$mean, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), 
                               FUN=sd); colnames(mrs_se) <- c("pers","items","prop","corr","se")
mrs_corr <- ddply(mrs, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(mrs_corr) <- c("pers","items","prop","corr","cor")

mrs2 <- merge(mrs_avgbias, mrs_avgabsbias, by=c("pers","items","prop","corr")); 
mrs2 <- merge(mrs2, mrs_avgbias2, by=c("pers","items","prop","corr")); mrs2$rmse <- mrs2$avgbias2^.5
mrs2 <- merge(mrs2, mrs_cov, by=c("pers","items","prop","corr"))
mrs2 <- merge(mrs2, mrs_se, by=c("pers","items","prop","corr"))
mrs2 <- merge(mrs2, mrs_corr, by=c("pers","items","prop","corr"))

####################################################################################
## TOI THETA
toi_avgbias <- aggregate.data.frame(toi$bias, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), 
                                    FUN=mean); colnames(toi_avgbias) <- c("pers","items","prop","corr","avgbias")
toi_avgabsbias <- aggregate.data.frame(toi$absbias, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), 
                                       FUN=mean); colnames(toi_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
toi_avgbias2 <- aggregate.data.frame(toi$bias2, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), 
                                     FUN=mean); colnames(toi_avgbias2) <- c("pers","items","prop","corr","avgbias2")
toi_cov <- aggregate.data.frame(toi$cov, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), 
                                FUN=mean); colnames(toi_cov) <- c("pers","items","prop","corr","cov")
toi_se <- aggregate.data.frame(toi$mean, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), 
                                FUN=sd); colnames(toi_se) <- c("pers","items","prop","corr","se")
toi_corr <- ddply(toi, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(toi_corr) <- c("pers","items","prop","corr","cor")

toi2 <- merge(toi_avgbias, toi_avgabsbias, by=c("pers","items","prop","corr")); 
toi2 <- merge(toi2, toi_avgbias2, by=c("pers","items","prop","corr")); toi2$rmse <- toi2$avgbias2^.5
toi2 <- merge(toi2, toi_cov, by=c("pers","items","prop","corr"))
toi2 <- merge(toi2, toi_se, by=c("pers","items","prop","corr"))
toi2 <- merge(toi2, toi_corr, by=c("pers","items","prop","corr"))

####################################################################################
## ERS THETA
ers_avgbias <- aggregate.data.frame(ers$bias, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), 
                                    FUN=mean); colnames(ers_avgbias) <- c("pers","items","prop","corr","avgbias")
ers_avgabsbias <- aggregate.data.frame(ers$absbias, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), 
                                       FUN=mean); colnames(ers_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
ers_avgbias2 <- aggregate.data.frame(ers$bias2, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), 
                                     FUN=mean); colnames(ers_avgbias2) <- c("pers","items","prop","corr","avgbias2")
ers_cov <- aggregate.data.frame(ers$cov, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), 
                                    FUN=mean); colnames(ers_cov) <- c("pers","items","prop","corr","cov")
ers_se <- aggregate.data.frame(ers$mean, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), 
                               FUN=sd); colnames(ers_se) <- c("pers","items","prop","corr","se")
ers_corr <- ddply(ers, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(ers_corr) <- c("pers","items","prop","corr","cor")

ers2 <- merge(ers_avgbias, ers_avgabsbias, by=c("pers","items","prop","corr")); 
ers2 <- merge(ers2, ers_avgbias2, by=c("pers","items","prop","corr")); ers2$rmse <- ers2$avgbias2^.5
ers2 <- merge(ers2, ers_cov, by=c("pers","items","prop","corr"))
ers2 <- merge(ers2, ers_se, by=c("pers","items","prop","corr"))
ers2 <- merge(ers2, ers_corr, by=c("pers","items","prop","corr"))

## MERGE and OUTPUT THETA RESULTS
mrs2 <- mrs2[order(mrs2$pers,mrs2$items,mrs2$prop,mrs2$corr), ]
toi2 <- toi2[order(toi2$pers,toi2$items,toi2$prop,toi2$corr), ]
ers2 <- ers2[order(ers2$pers,ers2$items,ers2$prop,ers2$corr), ]

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/Results")
write.xlsx(mrs2, "noncomp_MLE_results.xlsx", sheetName="MRS", col.names=T, row.names=F, append=T)
write.xlsx(toi2, "noncomp_MLE_results.xlsx", sheetName="TOI", col.names=T, row.names=F, append=T)
write.xlsx(ers2, "noncomp_MLE_results.xlsx", sheetName="ERS", col.names=T, row.names=F, append=T)

########################################################################################################################################################################
#####  CONDITIONAL THETA  ##############################################################################################################################################
########################################################################################################################################################################

mrs$cat <- cut(mrs$true, 
               breaks=c(-Inf,-1.75,-1.25,-0.75,-0.25,0.25,0.75,1.25,1.75,Inf),
               labels=c(  -2.0, -1.5, -1.0, -0.5,  0.0, 0.5, 1.0, 1.5, 2.0))

toi$cat <- cut(toi$true, 
               breaks=c(-Inf,-1.75,-1.25,-0.75,-0.25,0.25,0.75,1.25,1.75,Inf),
               labels=c(-2,-1.5,-1,-.5,0,.5,1,1.5,2))

ers$cat <- cut(ers$true, 
               breaks=c(-Inf,-1.75,-1.25,-0.75,-0.25,0.25,0.75,1.25,1.75,Inf),
               labels=c(-2,-1.5,-1,-.5,0,.5,1,1.5,2))

####################################################################################
## MRS THETA
mrsc_avgbias <- aggregate.data.frame(mrs$bias, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr,mrs$cat), 
                                     FUN=mean); colnames(mrsc_avgbias) <- c("pers","items","prop","corr","cat","avgbias")
mrsc_avgabsbias <- aggregate.data.frame(mrs$absbias, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr,mrs$cat), 
                                        FUN=mean); colnames(mrsc_avgabsbias) <- c("pers","items","prop","corr","cat","avgabsbias")
mrsc_avgbias2 <- aggregate.data.frame(mrs$bias2, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr,mrs$cat), 
                                      FUN=mean); colnames(mrsc_avgbias2) <- c("pers","items","prop","corr","cat","avgbias2")
mrsc_cov <- aggregate.data.frame(mrs$cov, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr,mrs$cat), 
                                 FUN=mean); colnames(mrsc_cov) <- c("pers","items","prop","corr","cat","cov")
mrsc_se <- aggregate.data.frame(mrs$mean, by=list(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr,mrs$cat), 
                                FUN=sd); colnames(mrsc_se) <- c("pers","items","prop","corr","cat","se")

mrsc2 <- merge(mrsc_avgbias, mrsc_avgabsbias, by=c("pers","items","prop","corr","cat")); 
mrsc2 <- merge(mrsc2, mrsc_avgbias2, by=c("pers","items","prop","corr","cat")); mrsc2$rmse <- mrsc2$avgbias2^.5
mrsc2 <- merge(mrsc2, mrsc_cov, by=c("pers","items","prop","corr","cat"))
mrsc2 <- merge(mrsc2, mrsc_se, by=c("pers","items","prop","corr","cat"))

####################################################################################
## TOI THETA
toic_avgbias <- aggregate.data.frame(toi$bias, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr,toi$cat), 
                                     FUN=mean); colnames(toic_avgbias) <- c("pers","items","prop","corr","cat","avgbias")
toic_avgabsbias <- aggregate.data.frame(toi$absbias, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr,toi$cat), 
                                        FUN=mean); colnames(toic_avgabsbias) <- c("pers","items","prop","corr","cat","avgabsbias")
toic_avgbias2 <- aggregate.data.frame(toi$bias2, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr,toi$cat), 
                                      FUN=mean); colnames(toic_avgbias2) <- c("pers","items","prop","corr","cat","avgbias2")
toic_cov <- aggregate.data.frame(toi$cov, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr,toi$cat), 
                                 FUN=mean); colnames(toic_cov) <- c("pers","items","prop","corr","cat","cov")
toic_se <- aggregate.data.frame(toi$mean, by=list(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr,toi$cat), 
                                FUN=sd); colnames(toic_se) <- c("pers","items","prop","corr","cat","se")

toic2 <- merge(toic_avgbias, toic_avgabsbias, by=c("pers","items","prop","corr","cat")); 
toic2 <- merge(toic2, toic_avgbias2, by=c("pers","items","prop","corr","cat")); toic2$rmse <- toic2$avgbias2^.5
toic2 <- merge(toic2, toic_cov, by=c("pers","items","prop","corr","cat"))
toic2 <- merge(toic2, toic_se, by=c("pers","items","prop","corr","cat"))

####################################################################################
## ERS THETA
ersc_avgbias <- aggregate.data.frame(ers$bias, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr,ers$cat), 
                                     FUN=mean); colnames(ersc_avgbias) <- c("pers","items","prop","corr","cat","avgbias")
ersc_avgabsbias <- aggregate.data.frame(ers$absbias, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr,ers$cat), 
                                        FUN=mean); colnames(ersc_avgabsbias) <- c("pers","items","prop","corr","cat","avgabsbias")
ersc_avgbias2 <- aggregate.data.frame(ers$bias2, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr,ers$cat), 
                                      FUN=mean); colnames(ersc_avgbias2) <- c("pers","items","prop","corr","cat","avgbias2")
ersc_cov <- aggregate.data.frame(ers$cov, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr,ers$cat), 
                                 FUN=mean); colnames(ersc_cov) <- c("pers","items","prop","corr","cat","cov")
ersc_se <- aggregate.data.frame(ers$mean, by=list(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr,ers$cat), 
                                FUN=sd); colnames(ersc_se) <- c("pers","items","prop","corr","cat","se")

ersc2 <- merge(ersc_avgbias, ersc_avgabsbias, by=c("pers","items","prop","corr","cat")); 
ersc2 <- merge(ersc2, ersc_avgbias2, by=c("pers","items","prop","corr","cat")); ersc2$rmse <- ersc2$avgbias2^.5
ersc2 <- merge(ersc2, ersc_cov, by=c("pers","items","prop","corr","cat"))
ersc2 <- merge(ersc2, ersc_se, by=c("pers","items","prop","corr","cat"))

####################################################################################
## MERGE and OUTPUT CONDITIONAL THETA RESULTS

mrsc2 <- mrsc2[order(mrsc2$pers,mrsc2$items,mrsc2$prop,mrsc2$corr,mrsc2$cat), ]
toic2 <- toic2[order(toic2$pers,toic2$items,toic2$prop,toic2$corr,toic2$cat), ]
ersc2 <- ersc2[order(ersc2$pers,ersc2$items,ersc2$prop,ersc2$corr,ersc2$cat), ]

write.xlsx(mrsc2, "noncomp_MLE_results.xlsx", sheetName="cond_MRS", col.names=T, row.names=F, append=T)
write.xlsx(toic2, "noncomp_MLE_results.xlsx", sheetName="cond_TOI", col.names=T, row.names=F, append=T)
write.xlsx(ersc2, "noncomp_MLE_results.xlsx", sheetName="cond_ERS", col.names=T, row.names=F, append=T)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
#####  ITEM PARAMETER  #################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
## SUMMARIZE ITEM PARAMETER by SUBSET

am <- m1[str_detect(m1$V1,"a_m"), ]
at <- m1[str_detect(m1$V1,"a_t"), ]
ae <- m1[str_detect(m1$V1,"a_e"), ]

dm <- m1[str_detect(m1$V1,"d_m"), ]
dt <- m1[str_detect(m1$V1,"d_t"), ]
de <- m1[str_detect(m1$V1,"d_e"), ]

dm_t <- i1[str_detect(i1$V1,"d_m"), ]; colnames(dm_t) <- c("V1","true_d","nsim","pers","items","MRS_prop","theta_corr","est")
dt_t <- i1[str_detect(i1$V1,"d_t"), ]; colnames(dt_t) <- c("V1","true_d","nsim","pers","items","MRS_prop","theta_corr","est")
de_t <- i1[str_detect(i1$V1,"d_e"), ]; colnames(de_t) <- c("V1","true_d","nsim","pers","items","MRS_prop","theta_corr","est")

bm_t <- i1[str_detect(i1$V1,"b_m"), ]; colnames(bm_t) <- c("V1","true_b","nsim","pers","items","MRS_prop","theta_corr","est")
bt_t <- i1[str_detect(i1$V1,"b_t"), ]; colnames(bt_t) <- c("V1","true_b","nsim","pers","items","MRS_prop","theta_corr","est")
be_t <- i1[str_detect(i1$V1,"b_e"), ]; colnames(be_t) <- c("V1","true_b","nsim","pers","items","MRS_prop","theta_corr","est")

dm_t <- cbind(dm_t,bm_t)
dt_t <- cbind(dt_t,bt_t)
de_t <- cbind(de_t,be_t)

dm <- dm[order(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr,dm$nsim,dm$true), ]
dt <- dt[order(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr,dt$nsim,dt$true), ]
de <- de[order(de$pers,de$items,de$MRS_prop,de$theta_corr,de$nsim,de$true), ]

dm_t <- dm_t[order(dm_t$pers,dm_t$items,dm_t$MRS_prop,dm_t$theta_corr,dm_t$nsim,dm_t$true_b), ]
dt_t <- dt_t[order(dt_t$pers,dt_t$items,dt_t$MRS_prop,dt_t$theta_corr,dt_t$nsim,dt_t$true_b), ]
de_t <- de_t[order(de_t$pers,de_t$items,de_t$MRS_prop,de_t$theta_corr,de_t$nsim,de_t$true_b), ]

## VERIFYING CORRECT MERGER
head(dm); head(dm_t)
tail(dm); tail(dm_t)
dm <- cbind(dm,dm_t[,c("true_d","true_b")])
diff <- dm$true-dm$true_b
max(diff); min(diff); mean(diff)

dt <- cbind(dt,dt_t[,c("true_d","true_b")])
diff <- dt$true-dt$true_b
max(diff); min(diff); mean(diff)

de <- cbind(de,de_t[,c("true_d","true_b")])
diff <- de$true-de$true_b
max(diff); min(diff); mean(diff)

## RECALCULATING RECOVERY INDICES
dm$bias <- dm$mean-dm$true_d; dm$absbias <- abs(dm$bias)
dm$bias2 <- dm$bias^2; dm$cov <- fifelse(dm$p025 < dm$true_d & dm$true_d < dm$p975,1,0)

dt$bias <- dt$mean-dt$true_d; dt$absbias <- abs(dt$bias)
dt$bias2 <- dt$bias^2; dt$cov <- fifelse(dt$p025 < dt$true_d & dt$true_d < dt$p975,1,0)

de$bias <- de$mean-de$true_d; de$absbias <- abs(de$bias)
de$bias2 <- de$bias^2; de$cov <- fifelse(de$p025 < de$true_d & de$true_d < de$p975,1,0)

####################################################################################
## MRS ITEM PARAMETER
# DISCRIMINATION
am_avgbias <- aggregate.data.frame(am$bias, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr), 
                                   FUN=mean); colnames(am_avgbias) <- c("pers","items","prop","corr","avgbias")
am_avgabsbias <- aggregate.data.frame(am$absbias, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr), 
                                      FUN=mean); colnames(am_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
am_avgbias2 <- aggregate.data.frame(am$bias2, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr), 
                                    FUN=mean); colnames(am_avgbias2) <- c("pers","items","prop","corr","avgbias2")
am_cov <- aggregate.data.frame(am$cov, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr), 
                               FUN=mean); colnames(am_cov) <- c("pers","items","prop","corr","cov")
am_se <- aggregate.data.frame(am$mean, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr), 
                              FUN=sd); colnames(am_se) <- c("pers","items","prop","corr","se")
am_corr <- ddply(am, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(am_corr) <- c("pers","items","prop","corr","cor")

am2 <- merge(am_avgbias, am_avgabsbias, by=c("pers","items","prop","corr")); 
am2 <- merge(am2, am_avgbias2, by=c("pers","items","prop","corr")); am2$rmse <- am2$avgbias2^.5
am2 <- merge(am2, am_cov, by=c("pers","items","prop","corr"))
am2 <- merge(am2, am_se, by=c("pers","items","prop","corr"))
am2 <- merge(am2, am_corr, by=c("pers","items","prop","corr"))

# DIFFICULTY
dm_avgbias <- aggregate.data.frame(dm$bias, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), 
                                   FUN=mean); colnames(dm_avgbias) <- c("pers","items","prop","corr","avgbias")
dm_avgabsbias <- aggregate.data.frame(dm$absbias, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), 
                                      FUN=mean); colnames(dm_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
dm_avgbias2 <- aggregate.data.frame(dm$bias2, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), 
                                    FUN=mean); colnames(dm_avgbias2) <- c("pers","items","prop","corr","avgbias2")
dm_cov <- aggregate.data.frame(dm$cov, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), 
                               FUN=mean); colnames(dm_cov) <- c("pers","items","prop","corr","cov")
dm_se <- aggregate.data.frame(dm$mean, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), 
                              FUN=sd); colnames(dm_se) <- c("pers","items","prop","corr","se")
dm_corr <- ddply(dm, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(dm_corr) <- c("pers","items","prop","corr","cor")

dm2 <- merge(dm_avgbias, dm_avgabsbias, by=c("pers","items","prop","corr")); 
dm2 <- merge(dm2, dm_avgbias2, by=c("pers","items","prop","corr")); dm2$rmse <- dm2$avgbias2^.5
dm2 <- merge(dm2, dm_cov, by=c("pers","items","prop","corr"))
dm2 <- merge(dm2, dm_se, by=c("pers","items","prop","corr"))
dm2 <- merge(dm2, dm_corr, by=c("pers","items","prop","corr"))

####################################################################################
## TOI ITEM PARAMETER
# DISCRIMINATION
at_avgbias <- aggregate.data.frame(at$bias, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr), 
                                   FUN=mean); colnames(at_avgbias) <- c("pers","items","prop","corr","avgbias")
at_avgabsbias <- aggregate.data.frame(at$absbias, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr), 
                                      FUN=mean); colnames(at_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
at_avgbias2 <- aggregate.data.frame(at$bias2, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr), 
                                    FUN=mean); colnames(at_avgbias2) <- c("pers","items","prop","corr","avgbias2")
at_cov <- aggregate.data.frame(at$cov, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr), 
                               FUN=mean); colnames(at_cov) <- c("pers","items","prop","corr","cov")
at_se <- aggregate.data.frame(at$mean, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr), 
                              FUN=sd); colnames(at_se) <- c("pers","items","prop","corr","se")
at_corr <- ddply(at, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(at_corr) <- c("pers","items","prop","corr","cor")

at2 <- merge(at_avgbias, at_avgabsbias, by=c("pers","items","prop","corr")); 
at2 <- merge(at2, at_avgbias2, by=c("pers","items","prop","corr")); at2$rmse <- at2$avgbias2^.5
at2 <- merge(at2, at_cov, by=c("pers","items","prop","corr"))
at2 <- merge(at2, at_se, by=c("pers","items","prop","corr"))
at2 <- merge(at2, at_corr, by=c("pers","items","prop","corr"))

# DIFFICULTY
dt_avgbias <- aggregate.data.frame(dt$bias, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), 
                                   FUN=mean); colnames(dt_avgbias) <- c("pers","items","prop","corr","avgbias")
dt_avgabsbias <- aggregate.data.frame(dt$absbias, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), 
                                      FUN=mean); colnames(dt_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
dt_avgbias2 <- aggregate.data.frame(dt$bias2, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), 
                                    FUN=mean); colnames(dt_avgbias2) <- c("pers","items","prop","corr","avgbias2")
dt_cov <- aggregate.data.frame(dt$cov, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), 
                               FUN=mean); colnames(dt_cov) <- c("pers","items","prop","corr","cov")
dt_se <- aggregate.data.frame(dt$mean, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), 
                              FUN=sd); colnames(dt_se) <- c("pers","items","prop","corr","se")
dt_corr <- ddply(dt, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(dt_corr) <- c("pers","items","prop","corr","cor")

dt2 <- merge(dt_avgbias, dt_avgabsbias, by=c("pers","items","prop","corr")); 
dt2 <- merge(dt2, dt_avgbias2, by=c("pers","items","prop","corr")); dt2$rmse <- dt2$avgbias2^.5
dt2 <- merge(dt2, dt_cov, by=c("pers","items","prop","corr"))
dt2 <- merge(dt2, dt_se, by=c("pers","items","prop","corr"))
dt2 <- merge(dt2, dt_corr, by=c("pers","items","prop","corr"))

####################################################################################
## ERS ITEM PARAMETER
# DISCRIMINATION
ae_avgbias <- aggregate.data.frame(ae$bias, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), 
                                   FUN=mean); colnames(ae_avgbias) <- c("pers","items","prop","corr","avgbias")
ae_avgabsbias <- aggregate.data.frame(ae$absbias, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), 
                                      FUN=mean); colnames(ae_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
ae_avgbias2 <- aggregate.data.frame(ae$bias2, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), 
                                    FUN=mean); colnames(ae_avgbias2) <- c("pers","items","prop","corr","avgbias2")
ae_cov <- aggregate.data.frame(ae$cov, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), 
                               FUN=mean); colnames(ae_cov) <- c("pers","items","prop","corr","cov")
ae_se <- aggregate.data.frame(ae$mean, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), 
                              FUN=sd); colnames(ae_se) <- c("pers","items","prop","corr","se")
ae_corr <- ddply(ae, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(ae_corr) <- c("pers","items","prop","corr","cor")

ae2 <- merge(ae_avgbias, ae_avgabsbias, by=c("pers","items","prop","corr")); 
ae2 <- merge(ae2, ae_avgbias2, by=c("pers","items","prop","corr")); ae2$rmse <- ae2$avgbias2^.5
ae2 <- merge(ae2, ae_cov, by=c("pers","items","prop","corr"))
ae2 <- merge(ae2, ae_se, by=c("pers","items","prop","corr"))
ae2 <- merge(ae2, ae_corr, by=c("pers","items","prop","corr"))

# DIFFICULTY
de_avgbias <- aggregate.data.frame(de$bias, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr), 
                                   FUN=mean); colnames(de_avgbias) <- c("pers","items","prop","corr","avgbias")
de_avgabsbias <- aggregate.data.frame(de$absbias, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr), 
                                      FUN=mean); colnames(de_avgabsbias) <- c("pers","items","prop","corr","avgabsbias")
de_avgbias2 <- aggregate.data.frame(de$bias2, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr), 
                                    FUN=mean); colnames(de_avgbias2) <- c("pers","items","prop","corr","avgbias2")
de_cov <- aggregate.data.frame(de$cov, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr), 
                               FUN=mean); colnames(de_cov) <- c("pers","items","prop","corr","cov")
de_se <- aggregate.data.frame(de$mean, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr), 
                              FUN=sd); colnames(de_se) <- c("pers","items","prop","corr","se")
de_corr <- ddply(de, .(pers,items,MRS_prop,theta_corr), corr_func); colnames(de_corr) <- c("pers","items","prop","corr","cor")

de2 <- merge(de_avgbias, de_avgabsbias, by=c("pers","items","prop","corr")); 
de2 <- merge(de2, de_avgbias2, by=c("pers","items","prop","corr")); de2$rmse <- de2$avgbias2^.5
de2 <- merge(de2, de_cov, by=c("pers","items","prop","corr"))
de2 <- merge(de2, de_se, by=c("pers","items","prop","corr"))
de2 <- merge(de2, de_corr, by=c("pers","items","prop","corr"))

####################################################################################
## MERGE and OUTPUT ITEM PARAMETER RESULTS
am2 <- am2[order(am2$pers,am2$items,am2$prop,am2$corr), ]
at2 <- at2[order(at2$pers,at2$items,at2$prop,at2$corr), ]
ae2 <- ae2[order(ae2$pers,ae2$items,ae2$prop,ae2$corr), ]

write.xlsx(am2, "noncomp_MLE_results.xlsx", sheetName="am", col.names=T, row.names=F, append=T)
write.xlsx(at2, "noncomp_MLE_results.xlsx", sheetName="at", col.names=T, row.names=F, append=T)
write.xlsx(ae2, "noncomp_MLE_results.xlsx", sheetName="ae", col.names=T, row.names=F, append=T)

dm2 <- dm2[order(dm2$pers,dm2$items,dm2$prop,dm2$corr), ]
dt2 <- dt2[order(dt2$pers,dt2$items,dt2$prop,dt2$corr), ]
de2 <- de2[order(de2$pers,de2$items,de2$prop,de2$corr), ]

write.xlsx(dm2, "noncomp_MLE_results.xlsx", sheetName="dm", col.names=T, row.names=F, append=T)
write.xlsx(dt2, "noncomp_MLE_results.xlsx", sheetName="dt", col.names=T, row.names=F, append=T)
write.xlsx(de2, "noncomp_MLE_results.xlsx", sheetName="de", col.names=T, row.names=F, append=T)

####################################################################################
## MERGE and OUTPUT ITEM PARAMETER BIAS
names(am)

am <- am[order(am$pers,am$items,am$MRS_prop,am$theta_corr), ]
at <- at[order(at$pers,at$items,at$MRS_prop,at$theta_corr), ]
ae <- ae[order(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), ]
dm <- dm[order(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), ]
dt <- dt[order(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), ]
de <- de[order(de$pers,de$items,de$MRS_prop,de$theta_corr), ]

mrs <- mrs[order(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), ]
toi <- toi[order(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), ]
ers <- ers[order(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), ]

fwrite(am, "noncomp_MLE_am_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(at, "noncomp_MLE_at_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(ae, "noncomp_MLE_ae_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(dm, "noncomp_MLE_dm_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(dt, "noncomp_MLE_dt_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(de, "noncomp_MLE_de_bias.csv", sep=",", col.names=T, row.names=F)

fwrite(mrs, "noncomp_MLE_mrs_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(toi, "noncomp_MLE_toi_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(ers, "noncomp_MLE_ers_bias.csv", sep=",", col.names=T, row.names=F)

# am <- as.data.frame(am)[,!(names(am) %in% drop)]
# at <- as.data.frame(at)[,!(names(at) %in% drop)]
# ae <- as.data.frame(ae)[,!(names(ae) %in% drop)]
# dm <- as.data.frame(dm)[,!(names(dm) %in% drop)]
# dt <- as.data.frame(dt)[,!(names(dt) %in% drop)]
# de <- as.data.frame(de)[,!(names(de) %in% drop)]
# am <- am[order(am$pers,am$items,am$MRS_prop,am$theta_corr), ]
# at <- at[order(at$pers,at$items,at$MRS_prop,at$theta_corr), ]
# ae <- ae[order(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), ]
# dm <- dm[order(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), ]
# dt <- dt[order(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), ]
# de <- de[order(de$pers,de$items,de$MRS_prop,de$theta_corr), ]
# 
# mrs <- as.data.frame(mrs)[,!(names(mrs) %in% drop)]
# toi <- as.data.frame(toi)[,!(names(toi) %in% drop)]
# ers <- as.data.frame(ers)[,!(names(ers) %in% drop)]
# mrs <- mrs[order(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), ]
# toi <- toi[order(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), ]
# ers <- ers[order(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), ]

####################################################################################
####################################################################################
## ITEM CATEGORY
head(am)

min(am$true); max(am$true)
min(at$true); max(at$true)
min(ae$true); max(ae$true)

am$cat <- cut(am$true, 
              breaks=c(0.50,1.00,1.50,2.00,2.50,3.00),
              labels=c( 0.75, 1.25, 1.75,2.25,2.75))
at$cat <- cut(at$true, 
              breaks=c(0.50,1.00,1.50,2.00,2.50,3.00),
              labels=c( 0.75, 1.25, 1.75,2.25,2.75))
ae$cat <- cut(ae$true, 
              breaks=c(0.50,1.00,1.50,2.00,2.50,3.00),
              labels=c( 0.75, 1.25, 1.75,2.25,2.75))

min(dm$true); max(dm$true); hist(dm$true)
min(dt$true); max(dt$true); hist(dt$true)
min(de$true); max(de$true); hist(de$true)

dm$cat <- cut(dm$true, 
              breaks=c(-.50, 0.00, 0.50, 1.00, 1.50, 2.00, Inf),
              labels=c( -0.25,  0.25, 0.75, 1.25, 1.75, 2.25))
dt$cat <- cut(dt$true, 
              breaks=c(-2.50, -1.50, -0.75, -0.25, 0.25, 0.75, 1.50, 2.50),
              labels=c(   -2.00, -1.25, -0.50,  0.00, 0.50, 1.25, 2.00))
de$cat <- cut(de$true, 
              breaks=c(-2.50, -1.50, -0.75, -0.25, 0.25, 0.75, 1.50, 2.50),
              labels=c(   -2.00, -1.25, -0.50,  0.00, 0.50, 1.25, 2.00))

####################################################################################
## MRS ITEM PARAMETERS
## DISCRIMINATION
amc_avgbias <- aggregate.data.frame(am$bias, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr,am$cat), 
                                    FUN=mean); colnames(amc_avgbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias")
amc_avgabsbias <- aggregate.data.frame(am$absbias, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr,am$cat), 
                                       FUN=mean); colnames(amc_avgabsbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgabsbias")
amc_avgbias2 <- aggregate.data.frame(am$bias2, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr,am$cat), 
                                     FUN=mean); colnames(amc_avgbias2) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias2")
amc_cov <- aggregate.data.frame(am$cov, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr,am$cat), 
                                FUN=mean); colnames(amc_cov) <- c("pers","items","MRS_prop","theta_corr","cat","cov")
amc_se <- aggregate.data.frame(am$mean, by=list(am$pers,am$items,am$MRS_prop,am$theta_corr,am$cat), 
                               FUN=sd); colnames(amc_se) <- c("pers","items","MRS_prop","theta_corr","cat","se")

amc2 <- merge(amc_avgbias, amc_avgabsbias, by=c("pers","items","MRS_prop","theta_corr","cat")); 
amc2 <- merge(amc2, amc_avgbias2, by=c("pers","items","MRS_prop","theta_corr","cat")); amc2$rmse <- amc2$avgbias2^.5
amc2 <- merge(amc2, amc_cov, by=c("pers","items","MRS_prop","theta_corr","cat"))
amc2 <- merge(amc2, amc_se, by=c("pers","items","MRS_prop","theta_corr","cat"))

## INTERCEPT
dmc_avgbias <- aggregate.data.frame(dm$bias, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr,dm$cat), 
                                    FUN=mean); colnames(dmc_avgbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias")
dmc_avgabsbias <- aggregate.data.frame(dm$absbias, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr,dm$cat), 
                                       FUN=mean); colnames(dmc_avgabsbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgabsbias")
dmc_avgbias2 <- aggregate.data.frame(dm$bias2, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr,dm$cat), 
                                     FUN=mean); colnames(dmc_avgbias2) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias2")
dmc_cov <- aggregate.data.frame(dm$cov, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr,dm$cat), 
                                FUN=mean); colnames(dmc_cov) <- c("pers","items","MRS_prop","theta_corr","cat","cov")
dmc_se <- aggregate.data.frame(dm$mean, by=list(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr,dm$cat), 
                               FUN=sd); colnames(dmc_se) <- c("pers","items","MRS_prop","theta_corr","cat","se")

dmc2 <- merge(dmc_avgbias, dmc_avgabsbias, by=c("pers","items","MRS_prop","theta_corr","cat")); 
dmc2 <- merge(dmc2, dmc_avgbias2, by=c("pers","items","MRS_prop","theta_corr","cat")); dmc2$rmse <- dmc2$avgbias2^.5
dmc2 <- merge(dmc2, dmc_cov, by=c("pers","items","MRS_prop","theta_corr","cat"))
dmc2 <- merge(dmc2, dmc_se, by=c("pers","items","MRS_prop","theta_corr","cat"))

## TOI ITEM PARAMETERS
atc_avgbias <- aggregate.data.frame(at$bias, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr,at$cat), 
                                    FUN=mean); colnames(atc_avgbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias")
atc_avgabsbias <- aggregate.data.frame(at$absbias, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr,at$cat), 
                                       FUN=mean); colnames(atc_avgabsbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgabsbias")
atc_avgbias2 <- aggregate.data.frame(at$bias2, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr,at$cat), 
                                     FUN=mean); colnames(atc_avgbias2) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias2")
atc_cov <- aggregate.data.frame(at$cov, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr,at$cat), 
                                FUN=mean); colnames(atc_cov) <- c("pers","items","MRS_prop","theta_corr","cat","cov")
atc_se <- aggregate.data.frame(at$mean, by=list(at$pers,at$items,at$MRS_prop,at$theta_corr,at$cat), 
                               FUN=sd); colnames(atc_se) <- c("pers","items","MRS_prop","theta_corr","cat","se")

atc2 <- merge(atc_avgbias, atc_avgabsbias, by=c("pers","items","MRS_prop","theta_corr","cat")); 
atc2 <- merge(atc2, atc_avgbias2, by=c("pers","items","MRS_prop","theta_corr","cat")); atc2$rmse <- atc2$avgbias2^.5
atc2 <- merge(atc2, atc_cov, by=c("pers","items","MRS_prop","theta_corr","cat"))
atc2 <- merge(atc2, atc_se, by=c("pers","items","MRS_prop","theta_corr","cat"))

## INTERCEPT
dtc_avgbias <- aggregate.data.frame(dt$bias, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr,dt$cat), 
                                    FUN=mean); colnames(dtc_avgbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias")
dtc_avgabsbias <- aggregate.data.frame(dt$absbias, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr,dt$cat), 
                                       FUN=mean); colnames(dtc_avgabsbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgabsbias")
dtc_avgbias2 <- aggregate.data.frame(dt$bias2, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr,dt$cat), 
                                     FUN=mean); colnames(dtc_avgbias2) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias2")
dtc_cov <- aggregate.data.frame(dt$cov, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr,dt$cat), 
                                FUN=mean); colnames(dtc_cov) <- c("pers","items","MRS_prop","theta_corr","cat","cov")
dtc_se <- aggregate.data.frame(dt$mean, by=list(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr,dt$cat), 
                               FUN=sd); colnames(dtc_se) <- c("pers","items","MRS_prop","theta_corr","cat","se")

dtc2 <- merge(dtc_avgbias, dtc_avgabsbias, by=c("pers","items","MRS_prop","theta_corr","cat")); 
dtc2 <- merge(dtc2, dtc_avgbias2, by=c("pers","items","MRS_prop","theta_corr","cat")); dtc2$rmse <- dtc2$avgbias2^.5
dtc2 <- merge(dtc2, dtc_cov, by=c("pers","items","MRS_prop","theta_corr","cat"))
dtc2 <- merge(dtc2, dtc_se, by=c("pers","items","MRS_prop","theta_corr","cat"))

## ERS ITEM PARAMETERS
aec_avgbias <- aggregate.data.frame(ae$bias, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr,ae$cat), 
                                    FUN=mean); colnames(aec_avgbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias")
aec_avgabsbias <- aggregate.data.frame(ae$absbias, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr,ae$cat), 
                                       FUN=mean); colnames(aec_avgabsbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgabsbias")
aec_avgbias2 <- aggregate.data.frame(ae$bias2, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr,ae$cat), 
                                     FUN=mean); colnames(aec_avgbias2) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias2")
aec_cov <- aggregate.data.frame(ae$cov, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr,ae$cat), 
                                FUN=mean); colnames(aec_cov) <- c("pers","items","MRS_prop","theta_corr","cat","cov")
aec_se <- aggregate.data.frame(ae$mean, by=list(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr,ae$cat), 
                               FUN=sd); colnames(aec_se) <- c("pers","items","MRS_prop","theta_corr","cat","se")

aec2 <- merge(aec_avgbias, aec_avgabsbias, by=c("pers","items","MRS_prop","theta_corr","cat")); 
aec2 <- merge(aec2, aec_avgbias2, by=c("pers","items","MRS_prop","theta_corr","cat")); aec2$rmse <- aec2$avgbias2^.5
aec2 <- merge(aec2, aec_cov, by=c("pers","items","MRS_prop","theta_corr","cat"))
aec2 <- merge(aec2, aec_se, by=c("pers","items","MRS_prop","theta_corr","cat"))

## INTERCEPT
dec_avgbias <- aggregate.data.frame(de$bias, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr,de$cat), 
                                    FUN=mean); colnames(dec_avgbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias")
dec_avgabsbias <- aggregate.data.frame(de$absbias, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr,de$cat), 
                                       FUN=mean); colnames(dec_avgabsbias) <- c("pers","items","MRS_prop","theta_corr","cat","avgabsbias")
dec_avgbias2 <- aggregate.data.frame(de$bias2, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr,de$cat), 
                                     FUN=mean); colnames(dec_avgbias2) <- c("pers","items","MRS_prop","theta_corr","cat","avgbias2")
dec_cov <- aggregate.data.frame(de$cov, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr,de$cat), 
                                FUN=mean); colnames(dec_cov) <- c("pers","items","MRS_prop","theta_corr","cat","cov")
dec_se <- aggregate.data.frame(de$mean, by=list(de$pers,de$items,de$MRS_prop,de$theta_corr,de$cat), 
                               FUN=sd); colnames(dec_se) <- c("pers","items","MRS_prop","theta_corr","cat","se")

dec2 <- merge(dec_avgbias, dec_avgabsbias, by=c("pers","items","MRS_prop","theta_corr","cat")); 
dec2 <- merge(dec2, dec_avgbias2, by=c("pers","items","MRS_prop","theta_corr","cat")); dec2$rmse <- dec2$avgbias2^.5
dec2 <- merge(dec2, dec_cov, by=c("pers","items","MRS_prop","theta_corr","cat"))
dec2 <- merge(dec2, dec_se, by=c("pers","items","MRS_prop","theta_corr","cat"))

####################################################################################
## OUTPUT CONDITIONAL ITEM PARAMETER RESULTS
amc2 <- amc2[order(amc2$pers,amc2$items,amc2$MRS_prop,amc2$theta_corr,amc2$cat), ]
atc2 <- atc2[order(atc2$pers,atc2$items,atc2$MRS_prop,atc2$theta_corr,atc2$cat), ]
aec2 <- aec2[order(aec2$pers,aec2$items,aec2$MRS_prop,aec2$theta_corr,aec2$cat), ]

dmc2 <- dmc2[order(dmc2$pers,dmc2$items,dmc2$MRS_prop,dmc2$theta_corr,dmc2$cat), ]
dtc2 <- dtc2[order(dtc2$pers,dtc2$items,dtc2$MRS_prop,dtc2$theta_corr,dtc2$cat), ]
dec2 <- dec2[order(dec2$pers,dec2$items,dec2$MRS_prop,dec2$theta_corr,dec2$cat), ]

write.xlsx(amc2, "noncomp_MLE_results.xlsx", sheetName="cond_am", col.names=T, row.names=F, append=T)
write.xlsx(atc2, "noncomp_MLE_results.xlsx", sheetName="cond_at", col.names=T, row.names=F, append=T)
write.xlsx(aec2, "noncomp_MLE_results.xlsx", sheetName="cond_ae", col.names=T, row.names=F, append=T)

write.xlsx(dmc2, "noncomp_MLE_results.xlsx", sheetName="cond_dm", col.names=T, row.names=F, append=T)
write.xlsx(dtc2, "noncomp_MLE_results.xlsx", sheetName="cond_dt", col.names=T, row.names=F, append=T)
write.xlsx(dec2, "noncomp_MLE_results.xlsx", sheetName="cond_de", col.names=T, row.names=F, append=T)


library(stringr)
library(xlsx)
library(plyr)
library(data.table)

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/Output")

####################################################################################
## OBTAIN FILE NAMES
# BAYES
B_files <- list.files(pattern="*.csv", full.names=T)

# READ FILES
# B_list <- sapply(B_files, fread, simplify=F) 
B_list <- lapply(B_files, fread, sep=",")

# CONVERT TO DATAFRAME
# b1 <- do.call(rbind.data.frame, B_list); row.names(b1) <- NULL
b1 <- rbindlist(B_list, use.names=T, fill=T); row.names(b1) <- NULL
head(b1)

rm(B_files,B_list)
 
## RECOVERY INDICES

b1$bias <- b1$mean-b1$true
b1$absbias <- abs(b1$bias)
b1$bias2 <- b1$bias^2
b1$cov <- fifelse(b1$p025 < b1$true & b1$true < b1$p975,1,0)

## CORRELATION FUNCTION USED BELOW
corr_func <- function(X){
  return(data.frame(cor=cor(X$mean, X$true)))
}

## THETA PARMS
# b1$mrs <- ifelse(str_detect(b1$V1,"mrs"),1,0)
# b1$toi <- ifelse(str_detect(b1$V1,"toi"),1,0)
# b1$ers <- ifelse(str_detect(b1$V1,"ers"),1,0)

## ITEM PARMS
# b1$am <- fifelse(str_detect(b1$V1,"a_m"),1,0)
# b1$at <- ifelse(str_detect(b1$V1,"a_t"),1,0)
# b1$ae <- ifelse(str_detect(b1$V1,"a_e"),1,0)
# b1$dm <- ifelse(str_detect(b1$V1,"d_m"),1,0)
# b1$dt <- ifelse(str_detect(b1$V1,"d_t"),1,0)
# b1$de <- ifelse(str_detect(b1$V1,"d_e"),1,0)

## CORRELATION
# b1$r <- ifelse(str_detect(b1$V1,"r_m"),1,0)

####################################################################################
## SUMMARIZE THETA by SUBSET
mrs <- b1[which(str_detect(b1$V1,"mrs")), ]
toi <- b1[which(str_detect(b1$V1,"toi")), ]
ers <- b1[which(str_detect(b1$V1,"ers")), ]

# mrs <- b1[which(b1$mrs==1), ]
# toi <- b1[which(b1$toi==1), ]
# ers <- b1[which(b1$ers==1), ]

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
write.xlsx(mrs2, "noncomp_Bayes_results.xlsx", sheetName="MRS", col.names=T, row.names=F, append=T)
write.xlsx(toi2, "noncomp_Bayes_results.xlsx", sheetName="TOI", col.names=T, row.names=F, append=T)
write.xlsx(ers2, "noncomp_Bayes_results.xlsx", sheetName="ERS", col.names=T, row.names=F, append=T)

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

write.xlsx(mrsc2, "noncomp_Bayes_results.xlsx", sheetName="cond_MRS", col.names=T, row.names=F, append=T)
write.xlsx(toic2, "noncomp_Bayes_results.xlsx", sheetName="cond_TOI", col.names=T, row.names=F, append=T)
write.xlsx(ersc2, "noncomp_Bayes_results.xlsx", sheetName="cond_ERS", col.names=T, row.names=F, append=T)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
#####  ITEM PARAMETER  #################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################
## SUMMARIZE ITEM PARAMETER by SUBSET

am <- b1[which(str_detect(b1$V1,"a_m")), ]
at <- b1[which(str_detect(b1$V1,"a_t")), ]
ae <- b1[which(str_detect(b1$V1,"a_e")), ]

dm <- b1[which(str_detect(b1$V1,"d_m")), ]
dt <- b1[which(str_detect(b1$V1,"d_t")), ]
de <- b1[which(str_detect(b1$V1,"d_e")), ]

dm$mean <- dm$mean*-1
dt$mean <- dt$mean*-1
de$mean <- de$mean*-1

dm$p025 <- dm$p025*-1; dm$p975 <- dm$p975*-1 
dt$p025 <- dt$p025*-1; dt$p975 <- dt$p975*-1 
de$p025 <- de$p025*-1; de$p975 <- de$p975*-1 

dm$bias <- dm$mean-dm$true; dm$absbias <- abs(dm$bias)
dm$bias2 <- dm$bias^2; dm$cov <- fifelse(dm$p025 < dm$true & dm$true < dm$p975,1,0)

dt$bias <- dt$mean-dt$true; dt$absbias <- abs(dt$bias)
dt$bias2 <- dt$bias^2; dt$cov <- fifelse(dt$p025 < dt$true & dt$true < dt$p975,1,0)

de$bias <- de$mean-de$true; de$absbias <- abs(de$bias)
de$bias2 <- de$bias^2; de$cov <- fifelse(de$p025 < de$true & de$true < de$p975,1,0)

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

write.xlsx(am2, "noncomp_Bayes_results.xlsx", sheetName="am", col.names=T, row.names=F, append=T)
write.xlsx(at2, "noncomp_Bayes_results.xlsx", sheetName="at", col.names=T, row.names=F, append=T)
write.xlsx(ae2, "noncomp_Bayes_results.xlsx", sheetName="ae", col.names=T, row.names=F, append=T)

dm2 <- dm2[order(dm2$pers,dm2$items,dm2$prop,dm2$corr), ]
dt2 <- dt2[order(dt2$pers,dt2$items,dt2$prop,dt2$corr), ]
de2 <- de2[order(de2$pers,de2$items,de2$prop,de2$corr), ]

write.xlsx(dm2, "noncomp_Bayes_results.xlsx", sheetName="dm", col.names=T, row.names=F, append=T)
write.xlsx(dt2, "noncomp_Bayes_results.xlsx", sheetName="dt", col.names=T, row.names=F, append=T)
write.xlsx(de2, "noncomp_Bayes_results.xlsx", sheetName="de", col.names=T, row.names=F, append=T)

####################################################################################
## MERGE and OUTPUT ITEM PARAMETER BIAS

drop <- c("se_mean","p20","p80","ESS","Rhat","mrs","toi","ers","am","at","ae","dm","dt","de","r")

am <- am[,(drop):=NULL]
at <- at[,(drop):=NULL]
ae <- ae[,(drop):=NULL]
dm <- dm[,(drop):=NULL]
dt <- dt[,(drop):=NULL]
de <- de[,(drop):=NULL]
am <- am[order(am$pers,am$items,am$MRS_prop,am$theta_corr), ]
at <- at[order(at$pers,at$items,at$MRS_prop,at$theta_corr), ]
ae <- ae[order(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), ]
dm <- dm[order(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), ]
dt <- dt[order(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), ]
de <- de[order(de$pers,de$items,de$MRS_prop,de$theta_corr), ]

mrs <- mrs[,(drop):=NULL]
toi <- toi[,(drop):=NULL]
ers <- ers[,(drop):=NULL]
mrs <- mrs[order(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), ]
toi <- toi[order(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), ]
ers <- ers[order(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), ]

fwrite(am, "noncomp_Bayes_am_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(at, "noncomp_Bayes_at_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(ae, "noncomp_Bayes_ae_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(dm, "noncomp_Bayes_dm_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(dt, "noncomp_Bayes_dt_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(de, "noncomp_Bayes_de_bias.csv", sep=",", col.names=T, row.names=F)

fwrite(mrs, "noncomp_Bayes_mrs_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(toi, "noncomp_Bayes_toi_bias.csv", sep=",", col.names=T, row.names=F)
fwrite(ers, "noncomp_Bayes_ers_bias.csv", sep=",", col.names=T, row.names=F)




am <- as.data.frame(am)[,!(names(am) %in% drop)]
at <- as.data.frame(at)[,!(names(at) %in% drop)]
ae <- as.data.frame(ae)[,!(names(ae) %in% drop)]
dm <- as.data.frame(dm)[,!(names(dm) %in% drop)]
dt <- as.data.frame(dt)[,!(names(dt) %in% drop)]
de <- as.data.frame(de)[,!(names(de) %in% drop)]
am <- am[order(am$pers,am$items,am$MRS_prop,am$theta_corr), ]
at <- at[order(at$pers,at$items,at$MRS_prop,at$theta_corr), ]
ae <- ae[order(ae$pers,ae$items,ae$MRS_prop,ae$theta_corr), ]
dm <- dm[order(dm$pers,dm$items,dm$MRS_prop,dm$theta_corr), ]
dt <- dt[order(dt$pers,dt$items,dt$MRS_prop,dt$theta_corr), ]
de <- de[order(de$pers,de$items,de$MRS_prop,de$theta_corr), ]

mrs <- as.data.frame(mrs)[,!(names(mrs) %in% drop)]
toi <- as.data.frame(toi)[,!(names(toi) %in% drop)]
ers <- as.data.frame(ers)[,!(names(ers) %in% drop)]
mrs <- mrs[order(mrs$pers,mrs$items,mrs$MRS_prop,mrs$theta_corr), ]
toi <- toi[order(toi$pers,toi$items,toi$MRS_prop,toi$theta_corr), ]
ers <- ers[order(ers$pers,ers$items,ers$MRS_prop,ers$theta_corr), ]

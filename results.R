
library(data.table)
library(xlsx)
library(ggplot2)
library(heplots)
library(lsr)

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/Results")

####################################################################################
####################################################################################
## READ ITEM PARAMETERS
am <- rbind(fread("noncomp_MLE_am_bias.csv",sep=",",header=T),
            fread("noncomp_Bayes_am_bias.csv",sep=",",header=T))
at <- rbind(fread("noncomp_MLE_at_bias.csv",sep=",",header=T),
            fread("noncomp_Bayes_at_bias.csv",sep=",",header=T))
ae <- rbind(fread("noncomp_MLE_ae_bias.csv",sep=",",header=T),
            fread("noncomp_Bayes_ae_bias.csv",sep=",",header=T))

dm <- rbind(fread("noncomp_MLE_dm_bias.csv",sep=",",header=T),
            fread("noncomp_Bayes_dm_bias.csv",sep=",",header=T)[,-c(20:21)])
dt <- rbind(fread("noncomp_MLE_dt_bias.csv",sep=",",header=T),
            fread("noncomp_Bayes_dt_bias.csv",sep=",",header=T)[,-c(20:21)])
de <- rbind(fread("noncomp_MLE_de_bias.csv",sep=",",header=T),
            fread("noncomp_Bayes_de_bias.csv",sep=",",header=T)[,-c(20:21)])

## ANOVA WITH PARTIAL ETA-SQUARED EFFECT SIZES
## ITEM DISCRIMINATION
## a_m
am_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop +
                         est*pers + est*items + est*MRS_prop, 
                       am), anova=T, type=3)
## a_t
at_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       at), anova=T, type=3)
## a_e
ae_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr +
                         pers*items + pers*MRS_prop + items*MRS_prop +
                         est*pers + est*items + est*MRS_prop, 
                       ae), anova=T, type=3)
## ITEM INTERCEPTS
## d_m
dm_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr +
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       dm), anova=T, type=3)
## d_t
dt_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       dt), anova=T, type=3)
## d_e
de_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       de), anova=T, type=3)

## CONDITIONAL ITEM BIAS MEANS
## DISCRIMINATION
## MRS
am_est <- am[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
am_est_pers <- am[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
am_est_pers_items <- am[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
am_est_pers_items_prop <- am[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

am_means <- merge(am_est,am_est_pers, all.y=T, by=c("est")); colnames(am_means) <- c("est","bias_est","pers","bias_est*pers")
am_means <- merge(am_means,am_est_pers_items, all.y=T, by=c("est","pers")); colnames(am_means)[6] <- c("bias_est*pers*items")
am_means <- merge(am_means,am_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(am_means)[8] <- c("bias_est*pers*items")
am_means <- am_means[,c(1,2,3,7,4,5,6,8)]

## TOI
at_est <- at[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
at_est_pers <- at[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
at_est_pers_items <- at[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
at_est_pers_items_prop <- at[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

at_means <- merge(at_est,at_est_pers, all.y=T, by=c("est")); colnames(at_means) <- c("est","bias_est","pers","bias_est*pers")
at_means <- merge(at_means,at_est_pers_items, all.y=T, by=c("est","pers")); colnames(at_means)[6] <- c("bias_est*pers*items")
at_means <- merge(at_means,at_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(at_means)[8] <- c("bias_est*pers*items")
at_means <- at_means[,c(1,2,3,7,4,5,6,8)]

## ERS
ae_est <- ae[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
ae_est_pers <- ae[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
ae_est_pers_items <- ae[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
ae_est_pers_items_prop <- ae[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

ae_means <- merge(ae_est,ae_est_pers, all.y=T, by=c("est")); colnames(ae_means) <- c("est","bias_est","pers","bias_est*pers")
ae_means <- merge(ae_means,ae_est_pers_items, all.y=T, by=c("est","pers")); colnames(ae_means)[6] <- c("bias_est*pers*items")
ae_means <- merge(ae_means,ae_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(ae_means)[8] <- c("bias_est*pers*items")
ae_means <- ae_means[,c(1,2,3,7,4,5,6,8)]

## INTERCEPTS
## MRS
dm_est <- dm[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
dm_est_pers <- dm[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
dm_est_pers_items <- dm[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
dm_est_pers_items_prop <- dm[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

dm_means <- merge(dm_est,dm_est_pers, all.y=T, by=c("est")); colnames(dm_means) <- c("est","bias_est","pers","bias_est*pers")
dm_means <- merge(dm_means,dm_est_pers_items, all.y=T, by=c("est","pers")); colnames(dm_means)[6] <- c("bias_est*pers*items")
dm_means <- merge(dm_means,dm_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(dm_means)[8] <- c("bias_est*pers*items")
dm_means <- dm_means[,c(1,2,3,7,4,5,6,8)]

## TOI
dt_est <- dt[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
dt_est_pers <- dt[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
dt_est_pers_items <- dt[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
dt_est_pers_items_prop <- dt[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

dt_means <- merge(dt_est,dt_est_pers, all.y=T, by=c("est")); colnames(dt_means) <- c("est","bias_est","pers","bias_est*pers")
dt_means <- merge(dt_means,dt_est_pers_items, all.y=T, by=c("est","pers")); colnames(dt_means)[6] <- c("bias_est*pers*items")
dt_means <- merge(dt_means,dt_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(dt_means)[8] <- c("bias_est*pers*items")
dt_means <- dt_means[,c(1,2,3,7,4,5,6,8)]

## ERS
de_est <- de[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
de_est_pers <- de[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
de_est_pers_items <- de[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
de_est_pers_items_prop <- de[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

de_means <- merge(de_est,de_est_pers, all.y=T, by=c("est")); colnames(de_means) <- c("est","bias_est","pers","bias_est*pers")
de_means <- merge(de_means,de_est_pers_items, all.y=T, by=c("est","pers")); colnames(de_means)[6] <- c("bias_est*pers*items")
de_means <- merge(de_means,de_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(de_means)[8] <- c("bias_est*pers*items")
de_means <- de_means[,c(1,2,3,7,4,5,6,8)]

####################################################################################
####################################################################################
## READ PERSON PARAMETERS
mrs <- rbind(fread("noncomp_MLE_mrs_bias.csv",sep=",",header=T),
             fread("noncomp_Bayes_mrs_bias.csv",sep=",",header=T))
toi <- rbind(fread("noncomp_MLE_toi_bias.csv",sep=",",header=T),
             fread("noncomp_Bayes_toi_bias.csv",sep=",",header=T))
ers <- rbind(fread("noncomp_MLE_ers_bias.csv",sep=",",header=T),
             fread("noncomp_Bayes_ers_bias.csv",sep=",",header=T))


## ANOVA WITH PARTIAL ETA-SQUARED EFFECT SIZES
## MRS
mrs_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr +
                          pers*items + pers*MRS_prop + items*MRS_prop + 
                          est*pers + est*items + est*MRS_prop, 
                        mrs), anova=T, type=3)
toi_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr +
                          pers*items + pers*MRS_prop + items*MRS_prop + 
                          est*pers + est*items + est*MRS_prop, 
                        toi), anova=T, type=3)
ers_F <- etaSquared(aov(bias ~ est + pers + items + MRS_prop + theta_corr +
                          pers*items + pers*MRS_prop + items*MRS_prop + 
                          est*pers + est*items + est*MRS_prop, 
                        ers), anova=T, type=3)
round(ers_F,5)
## CONDITIONAL PERSON PARAMETER BIAS MEANS
## MRS
mrs_est <- mrs[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
mrs_est_pers <- mrs[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
mrs_est_pers_items <- mrs[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
mrs_est_pers_items_prop <- mrs[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

mrs_means <- merge(mrs_est,mrs_est_pers, all.y=T, by=c("est")); colnames(mrs_means) <- c("est","bias_est","pers","bias_est*pers")
mrs_means <- merge(mrs_means,mrs_est_pers_items, all.y=T, by=c("est","pers")); colnames(mrs_means)[6] <- c("bias_est*pers*items")
mrs_means <- merge(mrs_means,mrs_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(mrs_means)[8] <- c("bias_est*pers*items")
mrs_means <- mrs_means[,c(1,2,3,7,4,5,6,8)]

## TOI
toi_est <- toi[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
toi_est_pers <- toi[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
toi_est_pers_items <- toi[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
toi_est_pers_items_prop <- toi[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

toi_means <- merge(toi_est,toi_est_pers, all.y=T, by=c("est")); colnames(toi_means) <- c("est","bias_est","pers","bias_est*pers")
toi_means <- merge(toi_means,toi_est_pers_items, all.y=T, by=c("est","pers")); colnames(toi_means)[6] <- c("bias_est*pers*items")
toi_means <- merge(toi_means,toi_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(toi_means)[8] <- c("bias_est*pers*items")
toi_means <- toi_means[,c(1,2,3,7,4,5,6,8)]

## ERS
ers_est <- ers[,lapply(.SD, mean), by=list(est), .SDcols="bias"]
ers_est_pers <- ers[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias"]
ers_est_pers_items <- ers[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias"]
ers_est_pers_items_prop <- ers[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias"]

ers_means <- merge(ers_est,ers_est_pers, all.y=T, by=c("est")); colnames(ers_means) <- c("est","bias_est","pers","bias_est*pers")
ers_means <- merge(ers_means,ers_est_pers_items, all.y=T, by=c("est","pers")); colnames(ers_means)[6] <- c("bias_est*pers*items")
ers_means <- merge(ers_means,ers_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(ers_means)[8] <- c("bias_est*pers*items")
ers_means <- ers_means[,c(1,2,3,7,4,5,6,8)]

write.xlsx(round(mrs_F,6), "noncomp_ANOVA_results.xlsx", sheetName="mrs", col.names=T, row.names=T, append=T)
write.xlsx(round(toi_F,6), "noncomp_ANOVA_results.xlsx", sheetName="toi", col.names=T, row.names=T, append=T)
write.xlsx(round(ers_F,6), "noncomp_ANOVA_results.xlsx", sheetName="ers", col.names=T, row.names=T, append=T)

write.xlsx(round(am_F,6), "noncomp_ANOVA_results.xlsx", sheetName="am", col.names=T, row.names=T, append=T)
write.xlsx(round(at_F,6), "noncomp_ANOVA_results.xlsx", sheetName="at", col.names=T, row.names=T, append=T)
write.xlsx(round(ae_F,6), "noncomp_ANOVA_results.xlsx", sheetName="ae", col.names=T, row.names=T, append=T)

write.xlsx(round(dm_F,6), "noncomp_ANOVA_results.xlsx", sheetName="dm", col.names=T, row.names=T, append=T)
write.xlsx(round(dt_F,6), "noncomp_ANOVA_results.xlsx", sheetName="dt", col.names=T, row.names=T, append=T)
write.xlsx(round(de_F,6), "noncomp_ANOVA_results.xlsx", sheetName="de", col.names=T, row.names=T, append=T)

write.xlsx(mrs_means, "noncomp_ANOVA_results.xlsx", sheetName="mrs_mean", col.names=T, row.names=F, append=T)
write.xlsx(toi_means, "noncomp_ANOVA_results.xlsx", sheetName="toi_mean", col.names=T, row.names=F, append=T)
write.xlsx(ers_means, "noncomp_ANOVA_results.xlsx", sheetName="ers_mean", col.names=T, row.names=F, append=T)

write.xlsx(am_means, "noncomp_ANOVA_results.xlsx", sheetName="am_mean", col.names=T, row.names=F, append=T)
write.xlsx(at_means, "noncomp_ANOVA_results.xlsx", sheetName="at_mean", col.names=T, row.names=F, append=T)
write.xlsx(ae_means, "noncomp_ANOVA_results.xlsx", sheetName="ae_mean", col.names=T, row.names=F, append=T)

write.xlsx(dm_means, "noncomp_ANOVA_results.xlsx", sheetName="dm_mean", col.names=T, row.names=F, append=T)
write.xlsx(dt_means, "noncomp_ANOVA_results.xlsx", sheetName="dt_mean", col.names=T, row.names=F, append=T)
write.xlsx(de_means, "noncomp_ANOVA_results.xlsx", sheetName="de_mean", col.names=T, row.names=F, append=T)

###############################################################################################################################
###############################################################################################################################
## BIAS^2
###############################################################################################################################
###############################################################################################################################

## ANOVA WITH PARTIAL ETA-SQUARED EFFECT SIZES
## ITEM DISCRIMINATION
## a_m
am_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop +
                         est*pers + est*items + est*MRS_prop, 
                       am), anova=T, type=3)
## a_t
at_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       at), anova=T, type=3)
## a_e
ae_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr +
                         pers*items + pers*MRS_prop + items*MRS_prop +
                         est*pers + est*items + est*MRS_prop, 
                       ae), anova=T, type=3)
## ITEM INTERCEPTS
## d_m
dm_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr +
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       dm), anova=T, type=3)
## d_t
dt_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       dt), anova=T, type=3)
## d_e
de_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr + 
                         pers*items + pers*MRS_prop + items*MRS_prop + 
                         est*pers + est*items + est*MRS_prop, 
                       de), anova=T, type=3)

## CONDITIONAL ITEM BIAS MEANS
## DISCRIMINATION
## MRS
am_est <- am[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
am_est_pers <- am[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
am_est_pers_items <- am[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
am_est_pers_items_prop <- am[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

am_means <- merge(am_est,am_est_pers, all.y=T, by=c("est")); colnames(am_means) <- c("est","bias2_est","pers","bias2_est*pers")
am_means <- merge(am_means,am_est_pers_items, all.y=T, by=c("est","pers")); colnames(am_means)[6] <- c("bias2_est*pers*items")
am_means <- merge(am_means,am_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(am_means)[8] <- c("bias2_est*pers*items")
am_means <- am_means[,c(1,2,3,7,4,5,6,8)]

## TOI
at_est <- at[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
at_est_pers <- at[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
at_est_pers_items <- at[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
at_est_pers_items_prop <- at[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

at_means <- merge(at_est,at_est_pers, all.y=T, by=c("est")); colnames(at_means) <- c("est","bias2_est","pers","bias2_est*pers")
at_means <- merge(at_means,at_est_pers_items, all.y=T, by=c("est","pers")); colnames(at_means)[6] <- c("bias2_est*pers*items")
at_means <- merge(at_means,at_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(at_means)[8] <- c("bias2_est*pers*items")
at_means <- at_means[,c(1,2,3,7,4,5,6,8)]

## ERS
ae_est <- ae[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
ae_est_pers <- ae[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
ae_est_pers_items <- ae[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
ae_est_pers_items_prop <- ae[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

ae_means <- merge(ae_est,ae_est_pers, all.y=T, by=c("est")); colnames(ae_means) <- c("est","bias2_est","pers","bias2_est*pers")
ae_means <- merge(ae_means,ae_est_pers_items, all.y=T, by=c("est","pers")); colnames(ae_means)[6] <- c("bias2_est*pers*items")
ae_means <- merge(ae_means,ae_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(ae_means)[8] <- c("bias2_est*pers*items")
ae_means <- ae_means[,c(1,2,3,7,4,5,6,8)]

## INTERCEPTS
## MRS
dm_est <- dm[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
dm_est_pers <- dm[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
dm_est_pers_items <- dm[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
dm_est_pers_items_prop <- dm[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

dm_means <- merge(dm_est,dm_est_pers, all.y=T, by=c("est")); colnames(dm_means) <- c("est","bias2_est","pers","bias2_est*pers")
dm_means <- merge(dm_means,dm_est_pers_items, all.y=T, by=c("est","pers")); colnames(dm_means)[6] <- c("bias2_est*pers*items")
dm_means <- merge(dm_means,dm_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(dm_means)[8] <- c("bias2_est*pers*items")
dm_means <- dm_means[,c(1,2,3,7,4,5,6,8)]

## TOI
dt_est <- dt[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
dt_est_pers <- dt[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
dt_est_pers_items <- dt[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
dt_est_pers_items_prop <- dt[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

dt_means <- merge(dt_est,dt_est_pers, all.y=T, by=c("est")); colnames(dt_means) <- c("est","bias2_est","pers","bias2_est*pers")
dt_means <- merge(dt_means,dt_est_pers_items, all.y=T, by=c("est","pers")); colnames(dt_means)[6] <- c("bias2_est*pers*items")
dt_means <- merge(dt_means,dt_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(dt_means)[8] <- c("bias2_est*pers*items")
dt_means <- dt_means[,c(1,2,3,7,4,5,6,8)]

## ERS
de_est <- de[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
de_est_pers <- de[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
de_est_pers_items <- de[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
de_est_pers_items_prop <- de[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

de_means <- merge(de_est,de_est_pers, all.y=T, by=c("est")); colnames(de_means) <- c("est","bias2_est","pers","bias2_est*pers")
de_means <- merge(de_means,de_est_pers_items, all.y=T, by=c("est","pers")); colnames(de_means)[6] <- c("bias2_est*pers*items")
de_means <- merge(de_means,de_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(de_means)[8] <- c("bias2_est*pers*items")
de_means <- de_means[,c(1,2,3,7,4,5,6,8)]

####################################################################################
####################################################################################
## PERSON PARAMETERS
## ANOVA WITH PARTIAL ETA-SQUARED EFFECT SIZES
## MRS
mrs_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr +
                          pers*items + pers*MRS_prop + items*MRS_prop + 
                          est*pers + est*items + est*MRS_prop, 
                        mrs), anova=T, type=3)
toi_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr +
                          pers*items + pers*MRS_prop + items*MRS_prop + 
                          est*pers + est*items + est*MRS_prop, 
                        toi), anova=T, type=3)
ers_F <- etaSquared(aov(bias2 ~ est + pers + items + MRS_prop + theta_corr +
                          pers*items + pers*MRS_prop + items*MRS_prop + 
                          est*pers + est*items + est*MRS_prop, 
                        ers), anova=T, type=3)

## CONDITIONAL PERSON PARAMETER BIAS MEANS
## MRS
mrs_est <- mrs[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
mrs_est_pers <- mrs[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
mrs_est_pers_items <- mrs[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
mrs_est_pers_items_prop <- mrs[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

mrs_means <- merge(mrs_est,mrs_est_pers, all.y=T, by=c("est")); colnames(mrs_means) <- c("est","bias2_est","pers","bias2_est*pers")
mrs_means <- merge(mrs_means,mrs_est_pers_items, all.y=T, by=c("est","pers")); colnames(mrs_means)[6] <- c("bias2_est*pers*items")
mrs_means <- merge(mrs_means,mrs_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(mrs_means)[8] <- c("bias2_est*pers*items")
mrs_means <- mrs_means[,c(1,2,3,7,4,5,6,8)]

## TOI
toi_est <- toi[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
toi_est_pers <- toi[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
toi_est_pers_items <- toi[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
toi_est_pers_items_prop <- toi[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

toi_means <- merge(toi_est,toi_est_pers, all.y=T, by=c("est")); colnames(toi_means) <- c("est","bias2_est","pers","bias2_est*pers")
toi_means <- merge(toi_means,toi_est_pers_items, all.y=T, by=c("est","pers")); colnames(toi_means)[6] <- c("bias2_est*pers*items")
toi_means <- merge(toi_means,toi_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(toi_means)[8] <- c("bias2_est*pers*items")
toi_means <- toi_means[,c(1,2,3,7,4,5,6,8)]

## ERS
ers_est <- ers[,lapply(.SD, mean), by=list(est), .SDcols="bias2"]
ers_est_pers <- ers[ ,lapply(.SD, mean), by=list(est,pers), .SDcols="bias2"]
ers_est_pers_items <- ers[ ,lapply(.SD, mean), by=list(est,pers,items), .SDcols="bias2"]
ers_est_pers_items_prop <- ers[ ,lapply(.SD, mean), by=list(est,pers,items,MRS_prop), .SDcols="bias2"]

ers_means <- merge(ers_est,ers_est_pers, all.y=T, by=c("est")); colnames(ers_means) <- c("est","bias2_est","pers","bias2_est*pers")
ers_means <- merge(ers_means,ers_est_pers_items, all.y=T, by=c("est","pers")); colnames(ers_means)[6] <- c("bias2_est*pers*items")
ers_means <- merge(ers_means,ers_est_pers_items_prop, all.y=T, by=c("est","pers","items")); colnames(ers_means)[8] <- c("bias2_est*pers*items")
ers_means <- ers_means[,c(1,2,3,7,4,5,6,8)]

write.xlsx(round(mrs_F,6), "noncomp_ANOVA_results.xlsx", sheetName="mrs_bias2", col.names=T, row.names=T, append=T)
write.xlsx(round(toi_F,6), "noncomp_ANOVA_results.xlsx", sheetName="toi_bias2", col.names=T, row.names=T, append=T)
write.xlsx(round(ers_F,6), "noncomp_ANOVA_results.xlsx", sheetName="ers_bias2", col.names=T, row.names=T, append=T)

write.xlsx(round(am_F,6), "noncomp_ANOVA_results.xlsx", sheetName="am_bias2", col.names=T, row.names=T, append=T)
write.xlsx(round(at_F,6), "noncomp_ANOVA_results.xlsx", sheetName="at_bias2", col.names=T, row.names=T, append=T)
write.xlsx(round(ae_F,6), "noncomp_ANOVA_results.xlsx", sheetName="ae_bias2", col.names=T, row.names=T, append=T)

write.xlsx(round(dm_F,6), "noncomp_ANOVA_results.xlsx", sheetName="dm_bias2", col.names=T, row.names=T, append=T)
write.xlsx(round(dt_F,6), "noncomp_ANOVA_results.xlsx", sheetName="dt_bias2", col.names=T, row.names=T, append=T)
write.xlsx(round(de_F,6), "noncomp_ANOVA_results.xlsx", sheetName="de_bias2", col.names=T, row.names=T, append=T)

write.xlsx(mrs_means, "noncomp_ANOVA_results.xlsx", sheetName="mrs_mean_bias2", col.names=T, row.names=F, append=T)
write.xlsx(toi_means, "noncomp_ANOVA_results.xlsx", sheetName="toi_mean_bias2", col.names=T, row.names=F, append=T)
write.xlsx(ers_means, "noncomp_ANOVA_results.xlsx", sheetName="ers_mean_bias2", col.names=T, row.names=F, append=T)

write.xlsx(am_means, "noncomp_ANOVA_results.xlsx", sheetName="am_mean_bias2", col.names=T, row.names=F, append=T)
write.xlsx(at_means, "noncomp_ANOVA_results.xlsx", sheetName="at_mean_bias2", col.names=T, row.names=F, append=T)
write.xlsx(ae_means, "noncomp_ANOVA_results.xlsx", sheetName="ae_mean_bias2", col.names=T, row.names=F, append=T)

write.xlsx(dm_means, "noncomp_ANOVA_results.xlsx", sheetName="dm_mean_bias2", col.names=T, row.names=F, append=T)
write.xlsx(dt_means, "noncomp_ANOVA_results.xlsx", sheetName="dt_mean_bias2", col.names=T, row.names=F, append=T)
write.xlsx(de_means, "noncomp_ANOVA_results.xlsx", sheetName="de_mean_bias2", col.names=T, row.names=F, append=T)

######################################################################################################################
######################################################################################################################
## REGRESSION WITH PARTIAL AND SEMI-PARTIAL R2 (NOT USED)

library(lmSupport)

am_R <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            am)
at_R <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            at)
ae_R <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            ae)

dm_R <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            dm)
dt_R <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            dt)
de_R <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            de)

mrs_R <- lm(bias ~ true + est + pers + items + MRS_prop + theta_corr +
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            mrs)
toi_R <- lm(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            toi)
ers_R <- lm(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop, 
            ers)

print("#####  a_m #####"); modelEffectSizes(am_R)
print("#####  a_t #####"); modelEffectSizes(at_R)
print("#####  a_e #####"); modelEffectSizes(ae_R)

print("#####  d_m #####"); modelEffectSizes(dm_R)
print("#####  d_t #####"); modelEffectSizes(dt_R)
print("#####  d_e #####"); modelEffectSizes(de_R)

print("#####  MRS #####"); modelEffectSizes(mrs_R)
print("#####  TOI #####"); modelEffectSizes(toi_R)
print("#####  ERS #####"); modelEffectSizes(ers_R)

head(mrs)

library(rstatix)

amR <- anova_test(data=am, 
                  dv=bias, 
                  between=c(pers,items,MRS_prop,theta_corr), 
                  within=est, 
                  covariate=true, 
                  effect.size="pes",
                  wid=V1)
print(atR)     
get_anova_table(am_R, correction = c("none"))
get_anova_table(at_R, correction = c("none"))
get_anova_table(ae_R, correction = c("none"))



table(am$pers);table(am$items);table(am$MRS_prop);table(am$theta_corr);


id <- data.table(rep(seq(1,(nrow(am)/2)),2))
am$V1 <- id
at$V1 <- id
ae$V1 <- id
dm$V1 <- id
dt$V1 <- id
de$V1 <- id

head(am)
amR <- aov(bias ~ true + as.factor(est) + as.factor(pers) + as.factor(items) + MRS_prop + theta_corr + 
             as.factor(pers)*as.factor(items) + as.factor(pers)*MRS_prop + as.factor(items)*MRS_prop +
             est*as.factor(pers) + est*as.factor(items) + est*MRS_prop + Error(V1),
           am)
atR <- aov(bias ~ true + as.factor(est) + as.factor(pers) + as.factor(items) + MRS_prop + theta_corr + 
             as.factor(pers)*as.factor(items) + as.factor(pers)*MRS_prop + as.factor(items)*MRS_prop +
             est*as.factor(pers) + est*as.factor(items) + est*MRS_prop + Error(V1),
           at)
aeR <- aov(bias ~ true + as.factor(est) + as.factor(pers) + as.factor(items) + MRS_prop + theta_corr + 
             as.factor(pers)*as.factor(items) + as.factor(pers)*MRS_prop + as.factor(items)*MRS_prop +
             est*as.factor(pers) + est*as.factor(items) + est*MRS_prop + Error(V1),
           ae)
dmR <- aov(bias ~ true + as.factor(est) + as.factor(pers) + as.factor(items) + MRS_prop + theta_corr + 
             as.factor(pers)*as.factor(items) + as.factor(pers)*MRS_prop + as.factor(items)*MRS_prop +
             est*as.factor(pers) + est*as.factor(items) + est*MRS_prop + Error(V1),
           dm)
dtR <- aov(bias ~ true + as.factor(est) + as.factor(pers) + as.factor(items) + MRS_prop + theta_corr + 
             as.factor(pers)*as.factor(items) + as.factor(pers)*MRS_prop + as.factor(items)*MRS_prop +
             est*as.factor(pers) + est*as.factor(items) + est*MRS_prop + Error(V1),
           dt)
deR <- aov(bias ~ true + as.factor(est) + as.factor(pers) + as.factor(items) + MRS_prop + theta_corr + 
             as.factor(pers)*as.factor(items) + as.factor(pers)*MRS_prop + as.factor(items)*MRS_prop +
             est*as.factor(pers) + est*as.factor(items) + est*MRS_prop + Error(V1),
           de)

head(mrs)
id <- data.table(rep(seq(1,(nrow(mrs)/2)),2))
mrs$V1 <- id
toi$V1 <- id
ers$V1 <- id

mrsR <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr +
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop + Error(V1), 
            mrs)
toiR <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop + Error(V1), 
            toi)
ersR <- aov(bias ~ true + est + pers + items + MRS_prop + theta_corr + 
              pers*items + pers*MRS_prop + items*MRS_prop +
              est*pers + est*items + est*MRS_prop + Error(V1), 
            ers)
rm(mrs,toi,ers)


fwrite(am, "am_SAS.csv", sep=",", col.names=T, row.names=F)


##############################################################################################
##############################################################################################
## ANOVA on Summary Statistics
##############################################################################################
##############################################################################################

am_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="am"),est="MLE")
am_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="am"),est="Bayes")
am <- cbind(id=rep(seq(1,nrow(am_M)),2),rbind(am_M,am_B))
am$pers <- as.factor(am$pers)
am$items <- as.factor(am$items)

at_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="at"),est="MLE")
at_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="at"),est="Bayes")
at <- cbind(id=rep(seq(1,nrow(at_M)),2),rbind(at_M,at_B))
at$pers <- as.factor(at$pers)
at$items <- as.factor(at$items)

ae_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="ae"),est="MLE")
ae_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="ae"),est="Bayes")
ae <- cbind(id=rep(seq(1,nrow(ae_M)),2),rbind(ae_M,ae_B))
ae$pers <- as.factor(ae$pers)
ae$items <- as.factor(ae$items)
ae$prop <- as.factor(ae$prop)
ae$corr <- as.factor(ae$corr)

dm_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="dm"),est="MLE")
dm_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="dm"),est="Bayes")
dm <- cbind(id=rep(seq(1,nrow(dm_M)),2),rbind(dm_M,dm_B))
dm$pers <- as.factor(dm$pers)
dm$items <- as.factor(dm$items)

dt_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="dt"),est="MLE")
dt_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="dt"),est="Bayes")
dt <- cbind(id=rep(seq(1,nrow(dt_M)),2),rbind(dt_M,dt_B))
dt$pers <- as.factor(dt$pers)
dt$items <- as.factor(dt$items)

de_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="de"),est="MLE")
de_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="de"),est="Bayes")
de <- cbind(id=rep(seq(1,nrow(de_M)),2),rbind(de_M,de_B))
de$pers <- as.factor(de$pers)
de$items <- as.factor(de$items)

head(am)
am_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), am)
at_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), at)
ae_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), ae)
print(am_R)
print(at_R)
print(ae_R)

dm_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), dm)
dt_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), dt)
de_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), de)
print(dm_R)
print(dt_R)
print(de_R)


mrs_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="MRS"),est="MLE")
mrs_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="MRS"),est="Bayes")
mrs <- cbind(id=rep(seq(1,nrow(mrs_M)),2),rbind(mrs_M,mrs_B))
mrs$pers <- as.factor(mrs$pers)
mrs$items <- as.factor(mrs$items)

toi_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="TOI"),est="MLE")
toi_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="TOI"),est="Bayes")
toi <- cbind(id=rep(seq(1,nrow(toi_M)),2),rbind(toi_M,toi_B))
toi$pers <- as.factor(toi$pers)
toi$items <- as.factor(toi$items)

ers_M <- cbind(read.xlsx("noncomp_MLE_results.xlsx", sheetName="ERS"),est="MLE")
ers_B <- cbind(read.xlsx("noncomp_Bayes_results.xlsx", sheetName="ERS"),est="Bayes")
ers <- cbind(id=rep(seq(1,nrow(ers_M)),2),rbind(ers_M,ers_B))
ers$pers <- as.factor(ers$pers)
ers$items <- as.factor(ers$items)


mrs_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr +
              Error(id), mrs)
toi_R <- aov(rmse ~ est + pers + items + prop + corr + 
               pers*items + pers*prop + items*prop +
               est*pers + est*items + est*prop + est*corr + 
               est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
               pers*items*prop + pers*items*corr + pers*prop*corr +
               Error(id), toi)
ers_R <- aov(rmse ~ est + pers + items + prop + corr + 
               pers*items + pers*prop + items*prop +
               est*pers + est*items + est*prop + est*corr + 
               est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
               pers*items*prop + pers*items*corr + pers*prop*corr +
               Error(id), ers)


print(mrs_R)
print(toi_R)
print(ers_R) 


ae_R <- aov(rmse ~ est + pers + items + prop + corr + 
              pers*items + pers*prop + items*prop +
              est*pers + est*items + est*prop + est*corr + 
              est*pers*items + est*pers*prop + est*pers*corr + est*items*prop + est*items*corr + est*prop*corr +
              pers*items*prop + pers*items*corr + pers*prop*corr 
              #Error(id)
              , ae)

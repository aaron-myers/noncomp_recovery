
library(data.table)
library(xlsx)
library(ggplot2)
library(viridis)
# library(heplots)
# library(lsr)

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/Results")

####################################################################################
####################################################################################
####################################################################################
## THETA - CONDITIONAL RMSE PLOTS                       ## VERIFIED NEGLIGIBLE DIFFERENCES IN ESTIMATOR

mrs <- cbind(read.xlsx("noncomp_Bayes_results.xlsx",
                         sheetName="cond_MRS",
                         as.data.frame=T),dim=1)
toi <- cbind(read.xlsx("noncomp_Bayes_results.xlsx",
                         sheetName="cond_TOI",
                         as.data.frame=T),dim=2)
ers <- cbind(read.xlsx("noncomp_Bayes_results.xlsx",
                         sheetName="cond_ERS",
                         as.data.frame=T),dim=3)
theta <- rbind(mrs,toi,ers)
head(theta)

t1 <- theta[which(theta$pers==3000 & theta$prop=="low"), ] # & theta$items==20
t1$corr2 <- ifelse(t1$corr=="low",".20",".50")
t1$dim <- factor(t1$dim, 
               levels=c(1,2,3),
               labels=c("paste(theta^{(MRS)})",
                        "paste(theta^{(TOI)})",
                        "paste(theta^{(ERS)})"))

ggplot(t1, aes(x=as.numeric(cat), y=rmse,                                  ## NEGLIGIBLE DIFFERENCES AMONG SAMPLE SIZES
               color=as.factor(items),                                     ## PLOT HIGHLIGHTS DIFFERENCES IN THETA CORRELATION 
               linetype=as.factor(corr2),                                       ##- HIGHER r --> BETTER RECOVERY
               group=interaction(corr2, items))) +
  geom_smooth(method="loess",  se=F) + 
  facet_wrap(~dim, ncol=1, scales="free_y", labeller=labeller(dim=label_parsed)) +
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 0)) +
  theme(panel.grid.major = element_line(color="white"),
        panel.grid.major.y = element_line(color="grey92"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA),
        axis.line = element_line(color="black"),
        strip.text.x = element_text(size=12, face="italic"),
        strip.background = element_rect(color="black", fill="grey92"),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key = element_rect(fill=NA),
        legend.title.align = .5,                 #      legend.box = "vertical",
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_blank()) +
  scale_color_manual(name="# Items:", values=c("black","grey72"),
                     labels=c("10","20")) +
  guides(linetype=guide_legend(override.aes=list(color="grey42"))) +
  scale_linetype_manual(name=expression(paste(italic(corr),"(",theta,"):")), values=c(1,4),
                        labels=c(".20",".50")) +
  scale_x_continuous(breaks=seq(-2,2,1)) +
  scale_y_continuous("RMSE") #, breaks=seq(0,2,.25))  

ggsave("cond_theta_item-corr_RMSE.png", units="in", width=6.5, height=6.5, dpi=600)

t2 <- theta[which(theta$pers==3000 & theta$corr=="mod" & theta$prop != "low"), ] # & theta$items==20
t2$prop2 <- ifelse(t2$prop=="low","11%",
                   ifelse(t2$prop=="mid","20%","38%"))
t2$dim <- factor(t2$dim, 
                 levels=c(1,2,3),
                 labels=c("paste(theta^{(MRS)})",
                          "paste(theta^{(TOI)})",
                          "paste(theta^{(ERS)})"))

ggplot(t2, aes(x=as.numeric(cat), y=rmse,                                  ## NEGLIGIBLE DIFFERENCES AMONG SAMPLE SIZES
               color=as.factor(items),                                     ## PLOT HIGHLIGHTS DIFFERENCES IN PROPORTION OF MIDPOINT RESPONSES
               linetype=as.factor(prop2),                                       ##- MORE MIDPOINT RESPONSES --> POORER RECOVERY OF TOI AND ERS
               group=interaction(prop2, items))) +
  geom_smooth(method="loess",  se=F) + 
  facet_wrap(~dim, ncol=1, scales="free_y", labeller=labeller(dim=label_parsed)) +
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 0)) +
  theme(panel.grid.major = element_line(color="white"),
        panel.grid.major.y = element_line(color="grey92"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA),
        axis.line = element_line(color="black"),
        strip.text.x = element_text(size=12, face="italic"),
        strip.background = element_rect(color="black", fill="grey92"),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key = element_rect(fill=NA),
        legend.title.align = .5,                 #      legend.box = "vertical",
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim=c(0,1)) +
  scale_color_manual(name="# Items:", values=c("black","grey72"),
                     labels=c("10","20")) +
  guides(linetype=guide_legend(override.aes=list(color="grey42"))) +
  scale_linetype_manual(name=expression(paste(italic(P),"(MRS):")), values=c(1,2,3)) +
  scale_x_continuous(breaks=seq(-2,2,1)) +
  scale_y_continuous("RMSE", breaks=seq(0,1,.25))  

ggsave("cond_theta_item-prop_RMSE.png", units="in", width=6.5, height=6.5, dpi=600)

####################################################################################
####################################################################################
####################################################################################
## ITEM DIFFICULTY - CONDITIONAL RMSE PLOTS (BAYES)

#mrs <- cbind(read.xlsx("noncomp_Bayes_results.xlsx",
#                       sheetName="cond_dm",
#                       as.data.frame=T),dim=1)
toi <- cbind(read.xlsx("noncomp_Bayes_results.xlsx",
                       sheetName="cond_dt",
                       as.data.frame=T),dim=2)
ers <- cbind(read.xlsx("noncomp_Bayes_results.xlsx",
                       sheetName="cond_de",
                       as.data.frame=T),dim=3)
diff <- rbind(toi,ers)
head(diff)

#diff <- diff[which(diff$dim==3), ] # & diff$items==20
diff$corr2 <- ifelse(diff$theta_corr=="low",".20",".50")
diff$dim <- factor(diff$dim, 
                   levels=c(2,3),
                   labels=c(#"paste(italic(d)^{(MRS)})",
                            "paste(italic(d)^{(TOI)})",
                            "paste(italic(d)^{(ERS)})"))
diff$prop <- ifelse(diff$MRS_prop=="low",1,
                    ifelse(diff$MRS_prop=="mid",2,3))
diff$prop <- factor(diff$prop, 
                    levels=c(1,2,3),
                    labels=c("11%",
                             "20%",
                             "38%"))

ggplot(diff, aes(x=as.numeric(cat), y=rmse,
               color=as.factor(items),
               linetype=as.factor(corr2), 
               group=interaction(corr2, items))) +
  geom_smooth(method="loess",  se=F) + 
  facet_grid(rows = vars(pers), cols = vars(prop), margins=T) + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 0)) +
  theme(panel.grid.major = element_line(color="white"),
        panel.grid.major.y = element_line(color="grey92"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA),
        axis.line = element_line(color="black"),
        strip.text.x = element_text(size=12), # face="italic"),
        strip.text.y = element_text(size=12), # face="italic"),
        strip.background = element_rect(color="black", fill="grey92"),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key = element_rect(fill=NA),
        legend.title.align = .5,                 #      legend.box = "vertical",
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="black", size=12)) +
  coord_cartesian(xlim=c(-2.1,2.1)) +
  scale_color_manual(name="# Items:", values=c("black","grey72"),
                     labels=c("10","20")) +
  guides(linetype=guide_legend(override.aes=list(color="grey42"))) +
  scale_linetype_manual(name=expression(paste(italic(corr),"(",theta,"):")), values=c(1,4),
                        labels=c(".20",".50")) +
  scale_x_continuous("Item Difficulty", breaks=seq(-2,2,1)) +
  scale_y_continuous("RMSE", breaks=seq(0,1.25,.25))  

ggsave("cond_ALL_difficulty_RMSE.png", units="in", width=6.5, height=4.5, dpi=600)

ggsave("cond_ALL_difficulty_RMSE_landscape.png", units="in", width=9, height=6.5, dpi=600)

####################################################################################
## ITEM DIFFICULTY - CONDITIONAL RMSE PLOTS (MAXIMUM LIKELIHOOD)

#mrs <- cbind(read.xlsx("noncomp_MLE_results.xlsx",
#                       sheetName="cond_dm",
#                       as.data.frame=T),dim=1)
toi <- cbind(read.xlsx("noncomp_MLE_results.xlsx",
                       sheetName="cond_dt",
                       as.data.frame=T),dim=2)
ers <- cbind(read.xlsx("noncomp_MLE_results.xlsx",
                       sheetName="cond_de",
                       as.data.frame=T),dim=3)
diff <- rbind(toi,ers)
head(diff)

#diff <- diff[which(diff$dim==3), ] # & diff$items==20
diff$corr2 <- ifelse(diff$theta_corr=="low",".20",".50")
diff$dim <- factor(diff$dim, 
                   levels=c(2,3),
                   labels=c(#"paste(italic(d)^{(MRS)})",
                     "paste(italic(d)^{(TOI)})",
                     "paste(italic(d)^{(ERS)})"))
diff$prop <- ifelse(diff$MRS_prop=="low",1,
                    ifelse(diff$MRS_prop=="mid",2,3))
diff$prop <- factor(diff$prop, 
                    levels=c(1,2,3),
                    labels=c("11%",
                             "20%",
                             "38%"))

ggplot(diff, aes(x=as.numeric(cat), y=rmse,
                 color=as.factor(items),
                 linetype=as.factor(corr2), 
                 group=interaction(corr2, items))) +
  geom_smooth(method="loess",  se=F) + 
  facet_grid(rows = vars(pers), cols = vars(prop), margins=T) + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 0)) +
  theme(panel.grid.major = element_line(color="white"),
        panel.grid.major.y = element_line(color="grey92"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA),
        axis.line = element_line(color="black"),
        strip.text.x = element_text(size=12), # face="italic"),
        strip.text.y = element_text(size=12), # face="italic"),
        strip.background = element_rect(color="black", fill="grey92"),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key = element_rect(fill=NA),
        legend.title.align = .5,                 #      legend.box = "vertical",
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(color="black", size=12)) +
  coord_cartesian(xlim=c(-2.1,2.1)) +
  scale_color_manual(name="# Items:", values=c("black","grey72"),
                     labels=c("10","20")) +
  guides(linetype=guide_legend(override.aes=list(color="grey42"))) +
  scale_linetype_manual(name=expression(paste(italic(corr),"(",theta,"):")), values=c(1,4),
                        labels=c(".20",".50")) +
  scale_x_continuous("Item Difficulty", breaks=seq(-2,2,1)) +
  scale_y_continuous("RMSE", breaks=seq(0,1.25,.25))  

ggsave("cond_ALL_difficulty_RMSE_MLE.png", units="in", width=6.5, height=4.5, dpi=600)

ggsave("cond_ALL_difficulty_RMSE_landscape_MLE.png", units="in", width=9, height=6.5, dpi=600)

####################################################################################
## ITEM DISCRIMINATION - CONDITIONAL RMSE PLOTS

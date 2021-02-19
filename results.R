
library(psych)
library(xlsx)
library(ggplot2)

setwd("C:/Users/Aaron/Dropbox/NonComp/New_10-20/")

mltheta <- read.xlsx("noncomp_MLE_results.xlsx",
                     sheetName="MRS",
                     as.data.frame=T)


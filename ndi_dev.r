



library(ggplot2)
library(gamm4)
library(MASS)
library(car)
library(plyr)
library(dplyr)
library(forcats)
library(QuantPsyc)
library(data.table)
library(reshape2)
library(psych)
library(jtools)
library(interactions)
library(psycho)
library(tidyverse)
library(interactions)
library(simpleboot)
library(DescTools)
library(magrittr)
library(emmeans)
options(scipen=3)
setwd("/Users/dgaric/Google\ Drive/AHEAD_project/Datasets/master_datasets/n196") #CHANGE
dat<-read.csv("AHEAD_n196_master.csv", header = T) #CHANGE
attach(dat)


##covariates
ind_cov = c(which(names(dat)=="sex"), which(names(dat)=="wholebrain_wm_ndi"), which(names(dat)=="directions_kept"))




dat.scale<-dat

## Select IVs

ind_iv = c(which(names(dat)=="age_at_scan_days"))



for(k in 1:length(ind_iv)) dat.scale[,ind_iv[k]] = scale(as.numeric(dat.scale[,ind_iv[k]])) # standardize the IV to get standardized betas

## Select DVs

ind_dv = c(which(names(dat)=="lpresma_ndi"),which(names(dat)=="lsma_ndi"),which(names(dat)=="lifgop_ndi"),which(names(dat)=="lifgtri_ndi"),which(names(dat)=="caudate_L_ndi"),which(names(dat)=="putamen_L_ndi"),which(names(dat)=="rpresma_ndi"),which(names(dat)=="rsma_ndi"),which(names(dat)=="rifgop_ndi"),which(names(dat)=="rifgtri_ndi"),which(names(dat)=="caudate_R_ndi"),which(names(dat)=="putamen_R_ndi"))


for(j in 1:length(ind_dv)) dat.scale[,ind_dv[j]] = scale(as.numeric(dat.scale[,ind_dv[j]])) # standardize the DV to get standardized betas

names(dat)[ind_dv]
boxplot(dat[ind_dv])
summary(dat[ind_dv])

pause = function()
{
    if (interactive())
    {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else
    {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}
m1 <- matrix(nrow=12, ncol=18) # set the output matrix size to hold the results, nrow is dv*iv
mat_row = 1 #


for(i in 1:length(ind_dv)){
	for(j in 1:length(ind_iv)){
		print(c(i,j))
		pred<-paste(names(dat)[ind_iv[j]],"-->",names(dat)[ind_dv[i]])
		print(pred)
		form = paste(names(dat)[ind_dv[i]],"~", names(dat)[ind_iv[j]])
        for(k in 1:length(ind_cov)){
			form = paste(form,"+",names(dat)[ind_cov[k]])
			print(form)
		}
		form = formula(form)
			mod1 = rlm(formula = form, data = dat)
			print(summary(mod1))
			mod2 = rlm(formula = form, data = dat.scale)
			print(summary(mod2))
conf <- confint.default(mod1, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)
CI.low.main = conf[2,1]
CI.up.main = conf[2,2]

main.cover<-between(0,CI.low.main, CI.up.main)


output.main<-c(pred, "&", summary(mod1)$df[2], "&", round(summary(mod1)$coef[, 1][2], digits = 6),round(summary(mod1)$coef[, 2][2],digits = 6), "&", round(summary(mod2)$coef[, 1][2], digits = 6), "&",round(summary(mod1)$coef[, 3][2], digits = 6), "&",round(2*pt(-abs(summary(mod1)$coef[, 3][2]),summary(mod1)$df[2]), digits = 4), "&", round(CI.low.main, digits = 10), "to", round(CI.up.main, digits = 10), main.cover, "\\")



m1[mat_row,]<-output.main # place the output into the matrix that was dimensioned above
mat_row = mat_row + 1 # step to the next matrix row
	}

colnames(m1)<-c("Predictor $-->$ Outcome","&", "df", "&", "B","SE","&","beta","&","t","&", "p", "&", "CIlow", "to", "CIup", "insig","slash")



}

write.table(m1,file="/Users/dgaric/Google\ Drive/Dissertation/Results/Tables/Development/ndi_dev.txt")



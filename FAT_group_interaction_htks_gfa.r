library(boot)
library(MASS)
options(scipen=3)
setwd("/Users/dgaric/Google\ Drive/AHEAD_project/Datasets/master_datasets/n196")
dat<-read.csv("AHEAD_n196_master.csv", header = T) #CHANGE
nboot<-5000

#------------------------------------- Predictors ---------------------------------------#
#modify

var.list1<-factor(levels = c("wins_lpresma_op_gfa","wins_lpresma_tri_gfa","wins_lsma_op_gfa","wins_lsma_tri_gfa","wins_rpresma_op_gfa","wins_rpresma_tri_gfa","wins_rsma_op_gfa","wins_rsma_tri_gfa"))

#8 predictors 

#-------------------------------------- Outcome -----------------------------------------#
#modify

var.list2<-factor(levels = c("HTKS_final_3parts"))

#1 outcomes


#---------------------------------Create Matrix -----------------------------------------#
# number of rows = levels in var.list1 multiplied by levels in var.list2
# number of columns= based on colnames below , add 6 per every variable or control added

m <- matrix(nrow=8, ncol=51) 
i = 0
j = 0
mat_row = 1
for (i in levels(var.list1)) {
	print(i)
	for(j in levels(var.list2)) {
		print(j)
		predout<-paste(i,"-->",j, sep = " ")
		print(predout)
x<-dat[i][,1]
print(x)
y<-dat[j][,1]

#----------------------------------- Covariates -----------------------------------------#
#modify wholebrain change ending

g<-dat$diagnosis_group
a<-dat$age_at_scan_days
s<-dat$sex
w<-dat$wins_wholebrain_gfa
p<-dat$highest_parent_cat
d<-dat$directions_kept

#------------------------------- variables standardized ---------------------------------#

xstand<-scale(x)
gstand<-scale(g)
ystand<-scale(y)
astand<-scale(a)
sstand<-scale(s)
wstand<-scale(w)
#bstand<-scale(b)
pstand<-scale(p)
dstand<-scale(d)

#------------------------------- bootstrap function ------------------------------------#
#do not edit 
			m2.ph.fun<-function(data,i) {	
			d<-data
			d$y<- d$fitted+d$res[i]
			coef(update(m2.fit,data=d))
			}

#--------------------------------- regular regression -----------------------------------#			

m2.fit<-lm(y~x*g+a+s+w+d+p, na.action = na.omit)			
#m2.fit<-lm(y~x*g+a+s+w+b+p+d, na.action = na.omit)
print(summary(m2.fit))
m2.predb<-coefficients(summary(m2.fit))[2]
m2.predb2<-coefficients(summary(m2.fit))[3]
m2.predb3<-coefficients(summary(m2.fit))[4]
m2.predb4<-coefficients(summary(m2.fit))[5]
m2.predb5<-coefficients(summary(m2.fit))[6]
m2.predb6<-coefficients(summary(m2.fit))[7]
m2.predb7<-coefficients(summary(m2.fit))[8]
m2.predb8<-coefficients(summary(m2.fit))[9]


#------------------------------ standardized regression ---------------------------------#			

m2.predstand<-rlm(ystand~xstand*gstand+astand+sstand+wstand+dstand+pstand, na.action = na.omit)
#m2.predstand<-rlm(ystand~xstand*gstand+astand+sstand+wstand+bstand+pstand+dstand, na.action = na.omit)
m2.predstandb<-coefficients(summary(m2.predstand))[2]
m2.predstandb2<-coefficients(summary(m2.predstand))[3]
m2.predstandb3<-coefficients(summary(m2.predstand))[4]
m2.predstandb4<-coefficients(summary(m2.predstand))[5]
m2.predstandb5<-coefficients(summary(m2.predstand))[6]
m2.predstandb6<-coefficients(summary(m2.predstand))[7]
m2.predstandb7<-coefficients(summary(m2.predstand))[8]
m2.predstandb8<-coefficients(summary(m2.predstand))[9]
m2.prep<-na.omit(cbind(y,x,g,a,s,w,d,p,x*g))

#------------------------------ confidence interals -------------------------------------#

m2.ph<-data.frame(m2.prep, res=resid(m2.fit), fitted=fitted(m2.fit))
m2.ph.rlm.boot<-boot(m2.ph,m2.ph.fun, R = nboot)
m2.bootstrap.ci1<-boot.ci(m2.ph.rlm.boot, index = 2, conf=0.95, type = "norm")
m2.bootstrap.ci2<-boot.ci(m2.ph.rlm.boot, index = 3, conf=0.95, type = "norm")
m2.bootstrap.ci3<-boot.ci(m2.ph.rlm.boot, index = 4, conf=0.95, type = "norm")
m2.bootstrap.ci4<-boot.ci(m2.ph.rlm.boot, index = 5, conf=0.95, type = "norm")
m2.bootstrap.ci5<-boot.ci(m2.ph.rlm.boot, index = 6, conf=0.95, type = "norm")
m2.bootstrap.ci6<-boot.ci(m2.ph.rlm.boot, index = 7, conf=0.95, type = "norm")
m2.bootstrap.ci7<-boot.ci(m2.ph.rlm.boot, index = 8, conf=0.95, type = "norm")
m2.bootstrap.ci8<-boot.ci(m2.ph.rlm.boot, index = 9, conf=0.95, type = "norm")
m2.ci.est1<-data.frame(m2.bootstrap.ci1[4])
m2.ci.est2<-data.frame(m2.bootstrap.ci2[4])
m2.ci.est3<-data.frame(m2.bootstrap.ci3[4])
m2.ci.est4<-data.frame(m2.bootstrap.ci4[4])
m2.ci.est5<-data.frame(m2.bootstrap.ci5[4])
m2.ci.est6<-data.frame(m2.bootstrap.ci6[4])
m2.ci.est7<-data.frame(m2.bootstrap.ci7[4])
m2.ci.est8<-data.frame(m2.bootstrap.ci8[4])



#----------------------------------- standard error -------------------------------------#

m2.predse<-sd((m2.ph.rlm.boot$t)[,2])
m2.pred2se<-sd((m2.ph.rlm.boot$t)[,3])
m2.pred3se<-sd((m2.ph.rlm.boot$t)[,4])
m2.pred4se<-sd((m2.ph.rlm.boot$t)[,5])
m2.pred5se<-sd((m2.ph.rlm.boot$t)[,6])
m2.pred6se<-sd((m2.ph.rlm.boot$t)[,7])
m2.pred7se<-sd((m2.ph.rlm.boot$t)[,8])
m2.pred8se<-sd((m2.ph.rlm.boot$t)[,9])

#-------------------------------------- t-statistic -------------------------------------#

tvalue.m2pred<-m2.predb/m2.predse
tvalue.m2pred2<-m2.predb2/m2.pred2se
tvalue.m2pred3<-m2.predb3/m2.pred3se
tvalue.m2pred4<-m2.predb4/m2.pred4se
tvalue.m2pred5<-m2.predb5/m2.pred5se
tvalue.m2pred6<-m2.predb6/m2.pred6se
tvalue.m2pred7<-m2.predb7/m2.pred7se
tvalue.m2pred8<-m2.predb8/m2.pred8se

#------------------------------------ degrees of freedom --------------------------------#
df <-summary(lm(y~x*g+a+s+w+d+p))$df[2]

#--------------------------------------- p-value ----------------------------------------#

m2.pvalpred1<-dt(abs(tvalue.m2pred), length(df))
m2.pvalpred2<-dt(abs(tvalue.m2pred2), length(df))
m2.pvalpred3<-dt(abs(tvalue.m2pred3), length(df))
m2.pvalpred4<-dt(abs(tvalue.m2pred4), length(df))
m2.pvalpred5<-dt(abs(tvalue.m2pred5), length(df))
m2.pvalpred6<-dt(abs(tvalue.m2pred6), length(df))
m2.pvalpred7<-dt(abs(tvalue.m2pred7), length(df))
m2.pvalpred8<-dt(abs(tvalue.m2pred8), length(df))


#------------------------------------ adjusted r-square ---------------------------------#

adj.r.sq<-summary(lm(y~x*g+a+s+w+d+p))$adj.r.squared



#--------------------------------- output parameters ------------------------------------#


output<-cbind(predout, m2.predb, m2.predse, m2.predstandb, m2.ci.est1[,2], m2.ci.est1[,3], m2.pvalpred1, m2.predb2, m2.pred2se, m2.predstandb2, m2.ci.est2[,2], m2.ci.est2[,3], m2.pvalpred2, m2.predb3, m2.pred3se, m2.predstandb3, m2.ci.est3[,2], m2.ci.est3[,3], m2.pvalpred3, m2.predb4, m2.pred4se, m2.predstandb4, m2.ci.est4[,2], m2.ci.est4[,3], m2.pvalpred4, m2.predb5, m2.pred5se, m2.predstandb5, m2.ci.est5[,2], m2.ci.est5[,3], m2.pvalpred5, m2.predb6, m2.pred6se, m2.predstandb6, m2.ci.est6[,2], m2.ci.est6[,3], m2.pvalpred6, m2.predb7, m2.pred7se, m2.predstandb7, m2.ci.est7[,2], m2.ci.est7[,3], m2.pvalpred7, m2.predb8, m2.pred8se, m2.predstandb8, m2.ci.est8[,2], m2.ci.est8[,3], m2.pvalpred8, adj.r.sq, df) 

m[mat_row,]<-output	
colnames(m)<-c("prediction", "b1", "b1se", "stdb1", "b1CIlow", "b1CIHigh", "b1pval", "group", "groupse", "groupstdb", "groupCIlow", "groupCIHigh", "grouppval","age", "agese", "agestdb", "ageCIlow", "ageCIHigh", "agepval","sex", "sexse", "sexstdb", "sexCIlow", "sexCIHigh", "sexpval","wholebrain","wholebrainse","wholebrainstd","wholebrainCIlow","wholebrainCIhigh","wholebrainpval","dir_kept", "dir_keptse", "dir_keptstdb","dir_keptCIlow","dir_keptCIhigh","dir_keptpval","ses_b", "ses_se", "ses_stdb","ses_CIlow","ses_CIhigh","ses_pval","group_int","group_int_se","group_intstdb","group_intCIlow","group_intCIhigh","group_intpval","adj.r.sq","df")

mat_row = mat_row + 1	
	}	
	print(m)
}
#----------------------------------- write tables ---------------------------------------#
#modify change csv title
write.table(m, file = paste("/Users/dgaric/Google\ Drive/Dissertation/Results/Tables/htks/int/FAT_group_interaction_htks_gfa.csv", sep=""), quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)


library(car)
avPlots(fit1)



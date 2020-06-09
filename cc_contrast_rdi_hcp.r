#Need most up-to-date version of R (>3.5)
library(lme4)
library(MASS)
library(emmeans)
library(ggplot2)
library(compute.es)
library(Hmisc)

setwd("/Users/dgari004/Google\ Drive/CC_RDI_NODDI/new\ contrast\ analyses/")
data<-read.csv("/Users/dgari004/Google\ Drive/CC_RDI_NODDI/new\ contrast\ analyses/hcp_842_rdi_within_subjects.csv")
attach(data)

#make plot. Adjust fo
cc.plot <- function(data){
theme_set(theme_light(base_size = 16))
p<-ggplot(mapping = aes(x = cc_region, y = rdi)) +
    geom_point(position = position_jitter(0.1), shape=21, fill = "light blue", col = "grey", size=1, stroke = 1) +
    stat_summary(geom = "point", fun.data = mean_cl_normal, color = "black", size = 3) +
    stat_summary(geom = "errorbar", fun.data = mean_cl_normal, color = "black", width = .1) +
    stat_summary(fun.y = mean, geom="line") + ylab('RDI') +
    scale_x_discrete('Corpus Callosum Region', waiver(), labels = c("G1", "G2", "G3", "B1", "B2", "B3", "I", "S1", "S2", "S3"), c(0:9)) +
    ggtitle('RDI Values by Corpus Callosum Region') + theme(panel.border = element_blank(),
    panel.grid.major = element_blank(), axis.line = element_line(size = .5, linetype = "solid", colour = "black"),
    axis.text = element_text(size = 18, colour = "black"), plot.title = element_text(hjust = .5)
    )
    return(p)
}
tiff("rdi_HCP.tiff", units = 'in', width = 6, height = 8, res = 200, compression = "lzw")
cc.plot(data)
dev.off()

# provide a boxplot. more for data inspection purposes. does not save plot
data$cc_region<- factor(data$cc_region, levels=unique(data$cc_region))
boxplot(rdi ~ cc_region,
        data = data,
        ylab="RDI",
        xlab="CC_region")
levels(data$cc_region)


####IMPORTANT ADDITION FROM DEA; because number of observations exceeds 3000, must change limits like stated below 

emm_options(pbkrtest.limit = 8400)
emm_options(lmerTest.limit = 8400)

#run the mixed effects model and calculate contrast
mod1 <- lmer(rdi ~ cc_region + (1 | subject),  data = data, na.action = na.omit, REML=TRUE)
summary(mod1)
tapply(rdi, cc_region, mean, na.rm=TRUE) #summarize means of each condition
n<-tapply(rdi, cc_region, length)[1] #summarize n of each condition
means = emmeans(mod1, "cc_region") #complete summary with standard error estimates
Contrasts = list(contrast1 = c(-5.5,-3.5,-1.5,0.5,2.5,4.5,2.5,0.5,-0.5,0.5)) #specify the contrast
plot(c(1:10), Contrasts$contrast1)
print(contrast.test<-contrast(means, Contrasts)) #run the contrast
tval<-summary(contrast.test)$t.ratio

#compute Cohen's d. note that this is not ideal for repeated measures, but it is fine for comparing within the same dataset
#because the d's are computed across the same subjects. see
#http://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/

cohend<-tval*sqrt(2/n)


###DONT RUN
##this runs the model within the repeated measures ANOVA framework. Gives a different answer, as expected. More for a check.
modelAOV <- aov(rdi~factor(cc_region)+Error(factor(subject)), data = data, na.action = na.omit)
meansAOV = emmeans(modelAOV, "cc_region")
contrast(meansAOV, Contrasts)


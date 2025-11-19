library(JM)
data(package = 'JM')
help(aids)
str(aids)
nrow(aids)
for (i in (1:ncol(aids))){
  print(sum(is.na(aids[,i])))
}



library(geepack)
library(caret)
str(aids)
aids$AZT = factor(aids$AZT)
aids$gender  = factor(aids$gender)
aids$drug = factor(aids$drug)
aids$prevOI = factor(aids$prevOI)
aids$obstime = factor(aids$obstime)
aids$logcd4 = log(aids$CD4+1)
gee.exc = geeglm(logcd4 ~ AZT+gender+drug+prevOI+obstime+drug:obstime, data=aids, id=patient, 
             family=gaussian(), corstr="exchangeable")   

gee.ar1 = geeglm(logcd4 ~ AZT+gender+drug+prevOI+obstime+drug:obstime, data=aids, id=patient, 
             family=gaussian(), corstr="ar1")   

gee.uns = geeglm(logcd4 ~ AZT+gender+drug+prevOI+obstime+drug:obstime, data=aids, id=patient, 
             family=gaussian(), corstr="unstructured")   

gee.ind = geeglm(logcd4 ~ AZT+gender+drug+prevOI+obstime+drug:obstime, data=aids, id=patient, 
                 family=gaussian(), corstr="independence")   
mlist = list(gee.exc, gee.ar1,gee.uns, gee.ind)
do.call(rbind, lapply(mlist, QIC))
print.ORCIs.gee(gee)
anova(gee.ind)
summary(gee.ind)


library(lme4)
glmm.null = lmer(CD4~ 1+(1|patient), data = aids)
glmm = lmer(CD4 ~ prevOI+ (1|patient), data = aids)
anova(glmm.null, glmm)

library(survival)
Surv(aids$Time, aids$death)
survival = survfit(Surv(Time, death)~prevOI + AZT, data = aids)
summary(survival)
plot(survival)

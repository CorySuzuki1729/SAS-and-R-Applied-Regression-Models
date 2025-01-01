install.packages("lme4")
library(reshape2)
library(lme4)
library(readr)
library(rcompanion)
library(geepack)
library(MuMIn)

psoriasis_data = read.csv("./Example9.1Data.csv", header=T,
sep=",")

#creating longform dataset
longform_data = melt(psoriasis_data, id.vars=c("patid", "group"),
variable.name="visits", value.name="npatches")

#creating numeric variable for time
weeks = ifelse(longform_data$visits=="day1", 0.14,
ifelse(longform_data$visits=="week1", 1,
ifelse(longform_data$visits=="week2", 2,
ifelse(longform_data$visits=="week5", 5, 13))))

#specifying reference category
longform_data$group = as.factor(longform_data$group)
group_rel = relevel(longform_data$group, ref="Tx")

#fitting GEE model with autoregressive working correlation matrix
ar1_fitted = geeglm(npatches~group_rel+weeks, data=longform_data,
id=patid, family=poisson(link="log"), corstr="ar1")
summary(ar1_fitted)
QIC(ar1_fitted)

#fitting GEE model with exchangeable working correlation matrix
exch_fitted = geeglm(npatches~group_rel+weeks, data=longform_data, id=patid,
family=poisson(link="log"), corstr="exchangeable")
summary(exch_fitted)
QIC(exch_fitted)

#fitting GEE model with independent working correlation matrix
ind_fitted = geeglm(npatches~group_rel+weeks,
data=longform_data, id=patid, family=poisson(link="log"),
corstr="independence")
summary(ind_fitted)
QIC(ind_fitted)

#using AR fitted model for prediction
print(predict(ar1_fitted, type="response",
data.frame(patid=11, group_rel="Tx", weeks=5)))
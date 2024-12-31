install.packages("reshape2")
install.packages("nlme")
library(reshape2)
library(nlme)
library(readr)
library(rcompanion)

cholesterol_data = read.csv("./Example8.1Data.csv", header=T,
sep=",")

#creating longform dataset
longform_data = melt(cholesterol_data,
id.vars=c("id", "gender", "age"),
variable.name="LDLmonth", value.name="LDL")

#creating numeric variable for time
month = ifelse(longform_data$LDLmonth=="LDL0", 0,
ifelse(longform_data$LDLmonth=="LDL6", 6, 
ifelse(longform_data$LDLmonth=="LDL9", 9, 24)))

#plotting histogram with fitted normal density
plotNormalHistogram(longform_data$LDL)

#testing for normality of distribution
shapiro.test(longform_data$LDL)

#specifying reference categories
longform_data$gender = as.factor(longform_data$gender)
gender_rel = relevel(longform_data$gender, ref="M")

#unstructured covariance matrix of error terms
un_fitted_model = lme(LDL~gender_rel+age+month,
random=~1+month|id, data=longform_data,
correlation=corSymm(), weights=varIdent(form=~id|month))
summary(un_fitted_model)

getVarCov(un_fitted_model, type="conditional")

#computing aicc
n = 108
p = 17
print(AICC <- -2*logLik(un_fitted_model)+2*p*n/(n-p-1))

#checking model fit
null_model = glm(LDL~gender_rel+age+month,
data=longform_data, family=gaussian(link=identity))
deviance = -2*(logLik(null_model)-logLik(un_fitted_model))
print(deviance)

pvalue = pchisq(deviance, df=12, lower.tail=F)
print(pvalue)

#Toeplitz covariance matrix of error terms
toep_fitted = lme(LDL~gender_rel+age+month,
random=~1+month|id, data=longform_data,
correlation=corARMA(form=~1|id,p=1,q=1))
summary(toep_fitted)
getVarCov(toep_fitted, type="conditional")

#computing AICC
p = 11
print(AICC_toep <- -2*logLik(toep_fitted)+2*p*n/(n-p-1))

#checking model fit
deviance_toep = -2*(logLik(toep_fitted)-logLik(null_model))
print(deviance_toep)

pvalue_toep = pchisq(deviance, df=6, lower.tail=F)
print(pvalue_toep)

#spatial power covariance matrix of error terms

sppow_fitted = lme(LDL~gender_rel+age+month,
random=~1+month|id, data=longform_data,
correlation=corCAR1(form=~month|id))
summary(sppow_fitted)
getVarCov(sppow_fitted, type="conditional")

#computing AICC
p = 9
print(AICC_sppow <- -2*logLik(sppow_fitted)+2*p*n/(n-p-1))

#checking model fit
deviance_sppow = -2*(logLik(null_model)-logLik(sppow_fitted))
print(deviance_sppow)

pvalue_sppow = pchisq(deviance_sppow, df=4, lower.tail=F)
print(pvalue_sppow)

#autoregressive covariance matrix of error terms
ar1_fitted = lme(LDL~gender_rel+age+month,
random=~1+month|id, data=longform_data,
correlation=corAR1(form=~1|id))
summary(ar1_fitted)
getVarCov(ar1_fitted, type="conditional")

#computing AICC
p = 9
print(AICC_ar1 <- -2*logLik(ar1_fitted)+2*p*n/(n-p-1))

#checking model fit
deviance_ar1 = -2*(logLik(null_model)-logLik(ar1_fitted))
print(deviance_ar1)

pvalue_ar1 = pchisq(deviance_ar1, df=4, lower.tail=F)
print(pvalue_ar1)

#compound symmetric covariance matrix of error terms
cs_fitted = lme(LDL~gender_rel+age+month,
random=~1+month|id, data=longform_data,
correlation=corCompSymm(form=~1|id))
summary(cs_fitted)
getVarCov(cs_fitted, type="conditional")

#computing AICC
p = 9
print(AICC_cs <- -2*logLik(cs_fitted)+2*p*n/(n-p-1))

#checking model fit
deviance_cs = -2*(logLik(null_model)-logLik(cs_fitted))
print(deviance_cs)
pvalue_cs = pchisq(deviance_cs, df=3, lower.tail=F)
print(pvalue_cs)

#using ar1 model for prediction
ar1_pred = predict(ar1_fitted, data.frame(gender_rel="F",
age=48, month=3), level=0)
print(ar1_pred)
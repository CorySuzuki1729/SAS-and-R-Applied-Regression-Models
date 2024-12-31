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
iflese(longform_data$LDLmonth=="LDL6", 6, 
ifelse(longform_data$LDLmonth=="LDL9", 9, 24)))

#plotting histogram with fitted normal density
plotNormalHistogram(longform_data$LDL)

#testing for normality of distribution
shapiro.test(longform_data$LDL)

#specifying reference categories
longform_data$gender = as.factor(longform_data$gender)
gender_rel = relevel(longform_data$gender, ref="M")

fitted_model = lme(LDL~gender_rel+age+month,
random=~1+month|id, data=longform_data)
summary(fitted_model)

#computing AICC
n = 108
p = 8
AICC = -2*logLik(fitted_model)+2*p*n/(n-p-1)
print(AICC)

#checking model fit
null_model = glm(LDL~gender_rel+age+month,
data=longform_data)
deviance = -2*(logLik(null_model)-logLik(fitted_model))
print(deviance)

pvalue = pchisq(deviance, df=3, lower.tail=F)
print(pvalue)

#using fitted model for prediction
random_slope_pred = predict(fitted_model, data.frame(gender_rel="F",
age=48, month=3), level=0)
print(random_slope_pred)
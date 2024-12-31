install.packages("geepack")
install.packages("MuMIn")
library(geepack)
library(MuMIn)
library(reshape2)
library(readr)
library(rcompanion)

cholesterol_data = read.csv("./Example8.1Data.csv", header=T,
sep=",")

#creating long form dataset
longform_data = melt(cholesterol_data,
id.vars=c("id","gender","age"),
variable_name="LDLmonth", value.name="LDL")

#sorting longform data set by id
longform_data = longform_data[order(longform_data$id),]

#creating numeric variable for time
month = ifelse(longform_data$LDLmonth=="LDL0",0,
ifelse(longform_data$LDLmonth=="LDL6",6,
ifelse(longform_data$LDLmonth=="LDL9",9,24)))

#specifying reference category
cholesterol_data$gender = as.factor(cholesterol_data$gender)
gender_rel = relevel(cholesterol_data$gender, ref="M")

#GEE with unstructured working correlation matrix
un_fitted_gee = geeglm(LDL~gender_rel+age+month, data=longform_data,
id=id, family=gaussian(link="identity"),
corstr="unstructured")
summary(un_fitted_gee)
QIC(un_fitted_gee)

#GEE with autoregressive working correlation matrix
ar1_fitted_gee = geeglm(LDL~gender_rel+age+month,
data=longform_data, id=id, family=gaussian(link="identity"),
corstr="ar1")
summary(ar1_fitted_gee)
QIC(ar1_fitted_gee)

#GEE with compound symmetric working correlation matrix
cs_fitted_gee = geeglm(LDL~gender_rel+age+month,
data=longform_data, id=id, family=gaussian(link="identity"),
corstr="exchangeable")
summary(cs_fitted_gee)
QIC(cs_fitted_gee)

#GEE with independent working correlation matrix
ind_fitted_gee = geeglm(LDL~gender_rel+age+month,
data=longform_data, id=id, family=gaussian(link="identity"),
corstr="independence")
summary(ind_fitted_gee)
QIC(ind_fitted_gee)

#using ar1 fitted model for prediction
ar1_gee_pred = predict(ar1_fitted_gee, data.frame(gender_rel="F",
age=48, month=3))
print(ar1_gee_pred)
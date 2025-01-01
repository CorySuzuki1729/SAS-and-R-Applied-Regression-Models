library(reshape2)
library(readr)
library(rcompanion)
library(lme4)

dyads_data = read.csv("./Example10.1Data.csv", header=T,
sep=",")

#creating longform dataset
data_depr = melt(dyads_data[,c("family", "individual", "relation",
"depression1", "depression2", "depression3")],
id.vars=c("family", "individual", "relation"),
variable.name="depr.visits", value.name="depression")
data_qol = melt(dyads_data[,c("qol1", "qol2", "qol3")],
variable.name="qol.visits", value.name="qol")
longform_data = cbind(data_depr, data_qol)

#creating numeric variable for time
visit = ifelse(longform_data$depr.visits=="depression1", 1,
ifelse(longform_data$depr.visits=="depression2", 2, 3))

#plotting histogram with fitted normal density
plotNormalHistogram(longform_data$qol)

#testing for normality of distribution
shapiro.test(longform_data$qol)

#specifying reference category
depression_rel = relevel(as.factor(longform_data$depression), ref="1")

#fitting hierarchical model
fitted_model = lmer(qol~relation+depression_rel+visit+(1+visit|family)
+(1+visit|family:individual), data=longform_data)
summary(fitted_model)

#checking model fit
null_model = glm(qol~relation+depression_rel+visit,
data=longform_data)
print(deviance <- -2*(logLik(null_model)-logLik(fitted_model)))

print(pvalue <- pchisq(deviance, df=5, lower.tail=F))

#using fitted model for prediction
hier_norm_pred = predict(fitted_model, data.frame(family=25,
individual=1, relation="M", depression_rel="0", visit=3),
allow.new.levels=T)
print(hier_norm_pred)
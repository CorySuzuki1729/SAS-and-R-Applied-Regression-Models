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

#fitting hierarchical model for nonnormal response
fitted_model = glmer(depression~relation+qol+visit+(1|family),
data=longform_data, family=binomial(link="logit"))
summary(fitted_model)

#checking model fit
null_model = glm(depression~relation+qol+visit, data=longform_data,
family=binomial(link=logit))
print(deviance <- -2*(logLik(null_model)-logLik(fitted_model)))

print(pvalue <- pchisq(deviance, df=6, lower.tail=F))

#using fitted model for prediction
print(predict(fitted_model, data.frame(family=25,
individual=1, relation="M", qol=3.5, visit=3),
re.form=NA, type="response"))
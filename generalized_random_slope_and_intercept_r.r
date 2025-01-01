install.packages("lme4")
library(reshape2)
library(lme4)
library(readr)
library(rcompanion)

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

#fitting random slope and intercept Poisson model
random_slope_poisson = glmer(npatches~group_rel+weeks+(1+weeks|patid),
data=longform_data, family=poisson(link="log"))
summary(random_slope_poisson)

#fitting random intercept only Poisson model
rand_int_poi_only = glmer(npatches~group_rel+weeks+(1|patid),
data=longform_data, family=poisson(link="log"))
summary(rand_int_poi_only)

#computing AICC
n = 50
p = 4
print(AICC <- -2*logLik(rand_int_poi_only)+2*p**n/(n-p-1))

#checking model fit
null_model = glm(npatches~group_rel+weeks,
data=longform_data, family=poisson(link=log))
print(deviance <- -2*(logLik(null_model)-logLik(rand_int_poi_only)))

pvalue = pchisq(deviance, df=1, lower.tail=F)
print(pvalue)

#using the model for prediction
rand_poi_pred = predict(rand_int_poi_only, data.frame(patid=11,
group_rel="Tx", weeks=5), re.form=NA, type="response")
print(rand_poi_pred)
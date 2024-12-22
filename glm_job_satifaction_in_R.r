install.packages("rcompanion")
install.packages("readr")
library(rcompanion)
library(readr)

job.satisfaction.data = read.csv(file="C:/Users/coryg/OneDrive/Desktop/Datasets_Stat_510/Example1.1Data.csv", header=TRUE, sep=",")

#plotting histogram with fitted normal density
plotNormalHistogram(job.satisfaction.data$score)

#testing normality of distribution
shapiro.test(job.satisfaction.data$score)

#specifying reference levels
educ.rel = relevel(job.satisfaction.data$educ, ref="master")

#fitting general linear model
fitted_model = glm(score~gender+age+educ.rel, data=job.satisfaction.data,
family=gaussian(link=identity))
summary(fitted_model)

#outputting estimated sigma
sigma(fitted_model)

#checking model fit
intercept.only.model = glm(score~1, data=job.satisfaction.data,
family=gaussian(link=identity))
deviance = -2 *(logLik(intercept.only.model)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=4, lower.tail=FALSE)
print(p_value)

#using fitted model for prediction
print(predict(fitted_model, data.frame(gender="F", age=40, educ.rel="bachelor")))

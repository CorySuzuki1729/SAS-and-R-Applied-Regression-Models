install.packages("nnet")
library(readr)
library(rcompanion)
library(nnet)

oral_data = read.csv("./Example4.4Data.csv", header=T, sep=",")

#specifying reference categories
oral_data$gender = as.factor(oral_data$gender)
oral_data$choice = as.factor(oral_data$choice)
gender_rel = relevel(oral_data$gender, ref="M")
choice_rel = relevel(oral_data$choice, ref="tartar")

#fitting generalized logits model
fitted_model = multinom(choice_rel~gender_rel+age+nteeth,
data=oral_data)
summary(fitted_model)

#checking model fit
intercept_only_model = multinom(choice_rel~1, data=oral_data)
deviance = deviance(intercept_only_model)-deviance(fitted_model)
print(deviance)

p_value = pchisq(deviance, df=3, lower.tail=F)

#using fitted model for prediction
model_prediction = predict(fitted_model, type="probs",
data.frame(gender_rel="M", age=49, nteeth=7))
print(model_prediction)
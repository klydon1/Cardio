heart = read.csv2("Documents/4th Year/1st Semester/MGSC 401/Final Project/cardio_train.csv")
attach(heart)

heart$cardio = as.integer(heart$cardio)
attach(heart)

library(ggplot2)
library(methods)
library(rms)
library(GGally)
library(ggfortify)
require(olsrr)
require(psych)



### multiple logistic regression output
mlogit = glm(cardio~age+height+ap_hi+ap_lo+gluc+
               cholesterol+smoke+alco+active, family='binomial', data=heart)


### remove outliers
library(car)
outlierTest(mlogit)

heart2 = heart[-c(47254,55848,43134,57292,53983,4818,31784,29666,68664,42398),]
mlogit2 = glm(cardio~age+height+ap_hi+ap_lo+gluc+
                cholesterol+smoke+alco+active, family='binomial', data=heart2)
View(heart2)
outlierTest(mlogit2)
heart3 = heart2[-c(8,64912,2015,1877,41506,23989,35041,25241),]

heart3$smoke = as.factor(heart3$smoke)
heart3$alco = as.factor(heart3$alco)
heart3$active=as.factor(heart3$active)

### make weight numeric
heart3$weight=as.integer(heart3$weight)
heart3$height=as.integer(heart3$height)
attach(heart3)


View(heart3)
mlogit3 = glm(cardio~age+weight+height+ap_hi+ap_lo+gluc+
                cholesterol+smoke+alco+active, family='binomial', data=heart3)
View(heart3)
outlierTest(mlogit3)

summary(mlogit2)
summary(mlogit3)







require(rms)





mlogit_r = lrm(cardio~age+weight+height+ap_hi+ap_lo+gluc+
                cholesterol+smoke+alco+active, data=heart3)
mlogit_r

mlogit_fake = lrm(cardio~age+gender+height+ap_hi+ap_lo+gluc+
                    cholesterol+smoke+alco+active, data=heart)
mlogit_fake

library(stargazer)
stargazer(mlogit3,dep.var.labels = 'Cardio', 
          covariate.labels = c('Age','Weight','Height',
                               'Systolic', 'Diastolic','Glucose',
                               'Cholesterol','Smoke','Alcohol','Active'), digits=5,type='html')


### accuracy of mlogit3
predict = as.numeric(predict(mlogit3,type='response'))
missed_log = 0
actual_log = heart3$cardio

for (i in 1:69982){
  if(predict[i] > 0.5){
    predict[i] = 1
  } else {
    predict[i]=0
  }
}

for (i in 1:69982){
  if(predict[i]!=actual_log[i]){
    missed_log = missed_log + 1
  }
}

error_rate_log = missed_log/69982
error_rate_log









View(heart3)

nrow(heart)
### make age in days
heart3$age=(heart3$age)/365
heart3$id == 140



attach(heart3)
### some basic visualizations
hist_age = ggplot(heart3,aes(x=age))+geom_histogram(bins=50,col='red')
hist_age
hist_weight = ggplot(heart3,aes(x=weight))+geom_histogram(bins=60,col='red')
hist_weight
hist_height = ggplot(heart3,aes(x=height))+geom_histogram(bins=60,col='red')
hist_height
hist_aphi = ggplot(heart3,aes(x=ap_hi))+geom_histogram(bins=500,col='red')
hist_aphi

hist_chlo = ggplot(heart,aes(x=cholesterol))+geom_histogram(bins=3,col='red')
hist_chlo
hist_gluc = ggplot(heart,aes(x=gluc))+geom_histogram(bins=3,col='red')
hist_gluc

hist_smoke = ggplot(heart,aes(x=smoke))+geom_histogram(bins=2,col='red')
hist_smoke
hist_alco = ggplot(heart,aes(x=alco))+geom_histogram(bins=2,col='red')
hist_alco
hist_active = ggplot(heart,aes(x=active))+geom_histogram(bins=2,col='red')
hist_active


age_plot = ggplot(heart3,aes(y=cardio,x=age))
line = geom_smooth(method='glm', col='red',formula=y~x,method.args=list(family=binomial))
age_plot+geom_point()+line

weight_plot = ggplot(heart3,aes(y=cardio,x=weight))
weight_plot+geom_point()+line

height_plot = ggplot(heart3,aes(y=cardio,x=height))
height_plot+geom_point()+line




### Probabilities
table(heart$cardio)
table(heart$smoke)
table(heart$alco)
table(heart$active)




### Collinearity
heart_var = heart[,c(2:13)]
heart_var$cardio = as.numeric(heart_var$cardio)


pairs.panels(heart_var)
#no variables are too correlated!

### need lda, qda



### boosted forest
library(gbm)
set.seed(1)
boosted = gbm(cardio~age+weight+height+ap_hi+ap_lo+gluc+
                cholesterol+smoke+alco+active, data=heart3,
                distribution = 'bernoulli', n.trees=10000,interaction.depth=4
              )
summary(boosted)

prediction = predict(boosted,newdata=heart3,n.trees=10000, type='response')

missed = 0
actual = heart3$cardio

for (i in 1:69982){
  if(prediction[i] > 0.5){
    prediction[i] = 1
  } else {
    prediction[i]=0
  }
}

for (i in 1:69982){
  if(prediction[i]!=actual[i]){
    missed = missed + 1
  }
}

error_rate = missed/69982
error_rate

### pca graph here

View(heart3)
heart3_var = heart3[,c(2:13)]



pca = prcomp(heart3_var, scale=TRUE)
pca
autoplot(pca,data=heart3_var, loadings=TRUE, loadings.label=TRUE, colour=ifelse(heart3_var$cardio==1, 'dark green','grey'))





### use mlogit = glm(cardio~..., family='binomial')


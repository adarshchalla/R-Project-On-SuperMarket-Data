'''
BUDT 730 
TA2
Adarsh, Yousif and Rahil
'''

#Import dataset
library(readxl)
df <- read_excel("GoodMorning.xlsx")
View(df)
attach(df)
library(MASS)

#MODEL A----

###Regression----
ModelA<-lm(`Units Sold`~.,data=df)
summary(ModelA)

###Plot Residuals----
plot(resid(ModelA)~fitted(ModelA),cex=0.2)
abline(h=0, col="blue")

###Scatter Plot----
plot(`Units Sold`, fitted(ModelA), xlim = c(0,1000), ylim = c(0,1000), cex=0.2)
abline(lm(fitted(ModelA)~`Units Sold`), col="red")

###distribution----
install.packages("Hmisc")
library(Hmisc)
hist.data.frame(df)

###BIC and AIC----
BIC(ModelA)
AIC(ModelA)

###VIF----
library(car)
vif(ModelA)

#MODEL B----

###Stepwise for model selection----
####Backward----
stepBW <- step(ModelA, direction='backward', scope=formula(ModelA))
summary(stepBW)

####Forward----
#define intercept-only model
intercept_only <- lm(`Units Sold` ~ 1 , data=df)
stepFW<- step(intercept_only, direction='forward', scope=formula(ModelA))
summary(stepFW)

###Both----
stepBOTH<-step(ModelA, direction="both")
summary(stepBOTH)

###correlation----
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor(df))

###normalizing dist----
log_units_sold<- log(`Units Sold`)
hist(log_units_sold)
hist(`Units Sold`)

###REGRESSION----
ModelB<-lm(formula= `Units Sold`~ +`Endcap`+`Average Retail Price`+`Sales Rep`+Demo+`Demo1-3`+`Demo4-5`)
summary(ModelB)

###Plot Residuals----
plot(resid(ModelB)~fitted(ModelB),cex=0.2)
abline(h=0, col="blue")

###Scatter Plot----
plot(`Units Sold`, fitted(ModelB), xlim = c(0,1000), ylim = c(0,1000), cex=0.2)
abline(lm(fitted(ModelB)~`Units Sold`), col="red")

#BIC and AIC
BIC(ModelB)
AIC(ModelB)

#VIF
vif(ModelB)

#bar plot of sales rep and units sold
library(ggplot2)
ggplot(df, aes(x = factor(`Sales Rep`), y = `Units Sold`)) +  stat_summary(fun = "mean", geom = "bar")

#Scatter of average retail price and units sold.
plot(`Average Retail Price`,`Units Sold`, xlim = c(2.5,6.5), ylim = c(0,950), cex=0.2)





          ##########################################
          #         UNIVERSITE PARIS DAUPHINE      #
          #         MASTER STATISTIC AND BIG DATA  #
          #         TIME SERIES PROJECT n°1        #
          #         Oussama BERGUIGA               #
          ##########################################


getwd()
setwd("C:/Users/oussa/Downloads/Data Science/Master Statistique Big Data Dauphine/Module 2/Séries temporelles")

?armasubsets

library(forecast)#for cleaning outliers
library(TSA)#for fitting ARMA
library(randtests)#for some statistical tests


#1)Data Importation

data=read.csv("data.csv",header=F,sep=";",dec=",")
colnames(data)=c("Time","Value")
str(data)

?as.Date

data$Time=as.Date(data$Time,"%Y-%m-%d")


#2)Plot of the data

#What is stionnarity ?
#There are two types of stationnarity:

#strong stationnarity Z1,Z2,...Zt and Z1+k,Z2+k,..Zt+k have same law
#weak stationnarity :

#E[Zt] is constant against t
#Var(Zt) is constant against t AND finite (Var= sigma²< + oo)
#Cov(Zt,Zt+k) is a function that only depends on k


plot(data, ylab="M1 Money Stock data",type="l")

#we can see a tendancy so it cannot be stationnary
#since the expectation is increasing exponentially
#The tendancy seems to be exponential

#This can be confirmed by a Dickey-Fuller test :

#Dickey-Fuller test : 
#Ho=the time serie is not stationnary  
#H1=the time serie is stationnary

adf.test(data$Value,alternative = "stationary", k = 0)

#p-value nearly equals to 1 : absolutly not stationnary

#3)Log ratio

log_ratio=100*diff(log(data$Value))[-1]

summary(log_ratio)

plot(log_ratio,type="l")

#on that plot we can see : 
#that the mean is constant during the time :
#the variance is NOT constant : 
#there is heteroscedasticity
#outliers, strong and weak volatility
#the log ratio time serie seems 
#to have clusters of volatility 
#(probably because of different financial crisis)

sd(log_ratio[0:1000])^2
sd(log_ratio[1000:1500])^2
sd(log_ratio[1500:2000])^2

#the variance seems to increase a lot...



which.max(log_ratio)#outlier max
which.min(log_ratio)#outlier min

data$Time[1393]#9-11 attacks ?
data$Time[1392]#?




boxplot(as.data.frame(log_ratio))

abline(b=0,a=mean(log_ratio),col="red")


#option 1 : delete outliers (i.e observations which log_ratio >5)
#log_ratio=log_ratio[abs(log_ratio)<=5 ]

#option 2: use "tsoutliers algorithm" to treat the outliers
#?tsoutliers

#str(tsoutliers(log_ratio))

#log_ratio[tsoutliers$index]=tsoutliers$value

#at first sight the time serie seems to be 
#strongly stationnary with outliers
#(this means that it does not imply that it 
#is weakly stationnary)

#let's delete the outliers in order than
#we can expect the time serie to be weakly stationnary

#tsoutlier

#31/length(log_ratio)
#we can treat the outliers since 
#they are just 1.5% of the number 
#of values in the log ratio

#option 3 : use tsclean algorithm 
#to treat outliers

?tsclean


tsclean(log_ratio)




par(mfrow=c(2,1))
plot(log_ratio,type="l")
log_ratio=tsclean(log_ratio)
plot(log_ratio,type="l",ylim=c(-5,10))

#stationnarity ?

#in view of the plot the cleaned time serie seems to be weakly stationnary
#let's try an adf test to check with R 
#weather it is stationnary



adf.test(log_ratio,alternative = "stationary", k = 0)

#p-value= 1%, it is < than 5% so the log_ratio serie is probably stationnary
#we can suppose that the log_ratio is weakly stationnary 
#since the variance is
#finite with no more outliers, 
#BUT we can see that the variance is increasing 
#in the second part of the dataset)
#it seems that there are still volatility clusters ...



#4)acp and pacf

?acf()

par(mfrow=c(2,1))
acf(log_ratio,main="Log_Ratio_ACF")
pacf(log_ratio,main="Log_Ratio_PACF")

#if acf is equal to zero after q lags, we could fit an MA(q) model
#if pacf is equal to zero after p lags, we could fit an AR(p) model
#both pacf and acf seems to decrease to zero after some lags
#so perhaps we can fit an AR, or an MA model


#5)

#the last important lag in the pacf is for p=13, so we can fit an AR model :

?arima

AR_p=ar(log_ratio,order.max=NULL)

#we find a AR(32) !

AR_p
AR_p=arima(x=log_ratio,order=c(32,0,0))
summary(AR_p)

#6)

#the last important lag in the acf is for q=26 so we can fit an MA(26) model :

?arima

MA_q=arima(x=log_ratio,order=c(0,0,26))
summary(MA_q)

#7)
plot(AR_p$residuals)
plot(MA_q$residuals)

#in view of the plot, the residuals seems uncorrelated BUT 
#the variance of the residuals does not seem to be constant...

acf(AR_p$residuals)
acf(MA_q$residuals)



#correlation of the residuals : we can use a Box-Ljung test
#(or Box Pierce or portemanteau test)
#Box-Ljung test : 
#Ho=the residuals are not correlated  
#H1=the residuals are correlated

Box.test(AR_p$residuals,type='Ljung')
Box.test(MA_q$residuals,type='Ljung')



#p-values are nearly equal to 1
#both p-values are >> 5 % : we can assume 
#that the residuals are uncorrelated


#yet the variance of the residuals does not seem to be constant...


#8)

?armasubsets
?auto.arima

ARMA_1=auto.arima(log_ratio,max.p=5,max.q=5)
summary(ARMA_1)

ARMA_1=Arima(log_ratio,c(4,0,2))
summary(ARMA_1)

#auto.arima gives us p=4 and q=2

ARMA_subset=armasubsets(log_ratio,nar=5,nma=5)
plot(ARMA_subset)

#in view of that plot, the miminum BIC (-150) is got whith p=2 or 3, q=1 or 2

ARMA_2=Arima(log_ratio,c(2,0,1))

summary(ARMA_2)


ARMA_3=Arima(log_ratio,c(2,0,2))

summary(ARMA_3)


ARMA_4=Arima(log_ratio,c(3,0,1))

summary(ARMA_4)


ARMA_5=Arima(log_ratio,c(3,0,2))

summary(ARMA_5)


#we have less than 5 parameters in the ARMA model whereas 
#we had 32 and 13 parameters in the AR and MA models !


#9) 

#Let us compare the different models obtained :

summary(AR_p)
summary(MA_q)
summary(ARMA_1)
summary(ARMA_2)
summary(ARMA_3)
summary(ARMA_4)
summary(ARMA_5)


Box.test(AR_p$residuals,type='Ljung')
Box.test(MA_q$residuals,type='Ljung')
Box.test(ARMA_1$residuals,type='Ljung')
Box.test(ARMA_2$residuals,type='Ljung')
Box.test(ARMA_3$residuals,type='Ljung')
Box.test(ARMA_4$residuals,type='Ljung')
Box.test(ARMA_5$residuals,type='Ljung')



#AR_p :   AIC=2854 , log_likelyhood=-1394 , Box Test p value =0.99 
#MA_q :   AIC=2948,  log_likelyhood=-1447 , Box Test p value =0.74 
#ARMA_1 : AIC=3170 , log_likelyhood=-1577 , Box Test p value =0.99
#ARMA_2 : AIC=3205 , log_likelyhood=-1597 , Box Test p value =0.88
#ARMA_3 : AIC=3080 , log_likelyhood=-1534 , Box Test p value =3.3e-5
#ARMA_4 : AIC=3199 , log_likelyhood=-1593 , Box Test p value =1
#ARMA_5 : AIC=3054 , log_likelyhood=-1520 , Box Test p value =0.75



#on the basis of thoses statistics indicators, the most preferable model COULD BE
#AR_p (lowest AIC, highest log likelyhood).
#BUT HERE p = 32 : it's a trap! There are too much parameters !!
#so we should better keep the ARMA_5(ARMA (3,2)) model (p+q=5 << 32 or 13)
#(lowest AIC, highest log likelyhood, uncorrelated residuals based on Box Test p value)

#is it a White Noise ? We have to check if: 
#the residuals mean is equal to 0, 
#their variance IS CONSTANT and 
#if they are uncorrelated




mean(ARMA_5$residuals)#OK



par(mfrow=c(2,1))
plot(ARMA_5$residuals,ylim=c(-5,5))
acf(ARMA_5$residuals,main="")#residuals seem uncorrelated


#based on the plot, it seems that the variance is increasing ...
#since the variance is not constant, it cannot be a white noise


#test for uncorrelation of the residuals

#we already saw that the residuals where uncorrelated whith the Box Ljung test
Box.test(ARMA_5$residuals,type='Ljung')

#p-value = 0.75 >> 5 %


#let's try another test

?turning.point.test

#Turning Point Test : 
#Ho= the residuals are not correlated 
#H1=the residuals are correlated

turning.point.test(ARMA_5$residuals,alternative="two.sided")

#p-value is 0.67, > 5% : we can assume that residuals are not correlated

#BUT given the residual plot,the variance seems to increase
#there is no homoscedasticity 

#is it gaussian ?


qqnorm(ARMA_5$residuals)
qqline(ARMA_5$residuals,col=2)


#the qqplot fits well only for the theoretical Quantiles between -1 and 1




#Test for normality of the residuals : we can use a Shapiro-Wulk test
#Shapiro-Wulk test : 
#Ho= the statistical serie follow a normal distribution     
#H1=the statistical serie does not follow a normal distribution

?shapiro.test
shapiro.test(ARMA_5$residuals)

#p-value =2.5e-15 << 5%, so shapiro test shows that it is NOT gaussian



#on the basis of the Shapiro test and the qqplot, 
#we can conclude that the residuals of the ARMA(3,2) model 
#are uncorreleted,their mean is zero, but they are not gaussian
#moreover, since the variance seems to be not constant,
#the residuals cannot be a white noise
#the ARMA model may not be the best thing to do here
#we will probably see in the next course 
#that the GARCH model has been discovered by Robert Engle in 1982
#(he got the Economic Nobel Prize in 2003 for that) to treat 
#financial time series  
#whose variance evolves with time 
#(volatility clustering, conditional heteroscedasticity)




# iot-telemetry-analysis-univariate-fit

# Import libraries
library(anytime)
library(plyr)
library(ggplot2)
library(xtable)
library(moments)
library(gamlss)
library(gamlss.mx)
library(tidyverse)
library(dplyr)
library(GGally)
library(gridExtra)
library(corrplot)
library(cluster)
library(factoextra)
library(mclust)
library(clValid)
library(scales)
library(fpc)
library(hamlet)
library(hopkins)
library(NbClust)


set.seed(17)


# Import dataset

# Online from github repo
#df = read.csv('https://raw.githubusercontent.com/Giovo17/iot-telemetry-analysis/main/iot_telemetry_data.csv')

# Local from disk
setwd("~/Documents/University/Data\ Science/1°\ Year\ (2022-2023)/Data\ Analysis\ (1)/Exam\ -\ Data\ Analysis/Report/iot-telemetry-analysis")
df = read.csv("iot_telemetry_data.csv")

# Randomly select 5000 rows from dataset to speed up runtimes
df = df[sample(nrow(df), 5000), ]





df$ts = anytime::anytime(df$ts)

df$device = revalue(df$device, c("b8:27:eb:bf:9d:51"="Device 1", "00:0f:00:70:91:0a"="Device 2", "1c:bf:ce:15:ec:4d"="Device 3"))





### UNIVARIATE ANALYSIS ###



### ------------------------------------------------------------------------ ###
# TS (Timestamp of readings) (not sure if mantaining this variable)







### ------------------------------------------------------------------------ ###
# Device (MAC address of the device, categorical variable)
# useful for cluster analysis given that each device is located in a different place

table(df$device)





### ------------------------------------------------------------------------ ###
# Light (binary variable) can be modeled as a bernoulli variable (need to check) ???
# need to convert to numeric variable, unclass function doesn't work


table(df$light)





### ------------------------------------------------------------------------ ###
# Motion (binary variable)

table(df$motion)




### ------------------------------------------------------------------------ ###
# CO (Carbon monoxide)

summary(df$co)
var(df$co)
moments::skewness(df$co)
moments::kurtosis(df$co)




# Fitting
family=c('NO','LO','GU','RG','exGAUS','TF','PE','SN1','SN2',
         'EGB2','GT','JSU','SHASH','SST')
v=c()
for (f in family){
  fit.model=histDist(T, family=f,nbins = 30, main=paste(f,'distribution'))
  v=c(v,fit.model$df.fit)# number of parameters
  #v=c(v,fitted(fit.model, "mu")[1])# ML estimated parameter
  #v=c(v,fitted(fit.model, "sigma")[2])
  v=c(v,logLik(fit.model))
  v=c(v,AIC(fit.model))# AIC (to be minimized)
  v=c(v,fit.model$sbc)
}










### ------------------------------------------------------------------------ ###
# Humidity (%, so defined in [0,100])

summary(df$humidity)
var(df$humidity)
moments::skewness(df$humidity)
moments::kurtosis(df$humidity)


# EDA

jpeg(file="../LateX_project/images/chapter2/humidity_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=humidity)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/humidity_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=humidity)) +
  geom_histogram( binwidth=5, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()


# Fitting

X4 = df$humidity


# From the kernel smoothing approach is evident that the variable is bimodal so it's good to try a mixture of distributions


# Fitting Humidity with mixture models

# Fitting the variable with a mixture of 2 gamma distributions
Humidity_fit_GA_2 = gamlssMXfits(n = 2, X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity_fit_GA_2)
logLik(Humidity_fit_GA_2)
Humidity_fit_GA_2$prob # mixture weights
fitted(Humidity_fit_GA_2, "mu")[1]

mu_hat1 = exp(Humidity_fit_GA_2[["models"]][[1]][["mu_coefficients"]])    
sigma_hat1 = exp(Humidity_fit_GA_2[["models"]][[1]][["sigma_coefficients"]])

mu_hat2 = exp(Humidity_fit_GA_2[["models"]][[2]][["mu_coefficients"]])    
sigma_hat2 = exp(Humidity_fit_GA_2[["models"]][[2]][["sigma_coefficients"]])


hist(X4, breaks = 50,freq = FALSE)
lines(seq(min(X4),max(X4),length=length(X4)),fit_GA_2[["prob"]][1]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat1, sigma = sigma_hat1),lty=2,lwd=3,col=2)
lines(seq(min(X4),max(X4),length=length(X4)),fit_GA_2[["prob"]][2]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat2, sigma = sigma_hat2),lty=2,lwd=3,col=3)
lines(seq(min(X4),max(X4),length=length(X4)),
        fit_GA_2[["prob"]][1]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat1, sigma = sigma_hat1) +
        fit_GA_2[["prob"]][2]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat2, sigma = sigma_hat2),
        lty = 1, lwd = 3, col = 1)



# Fitting the variable with a mixture of 2 lognormal distributions
Humidity_fit_LOGNO_2 = gamlssMXfits(n = 2, X4~1, family = LOGNO, K = 2, data = NULL)  # n = initial points
str(Humidity_fit_LOGNO_2)
logLik(Humidity_fit_LOGNO_2)
Humidity_fit_LOGNO_2$prob # mixture weights
fitted(Humidity_fit_LOGNO_2, "mu")[1]

# Fitting the variable with a mixture of 2 gamma distributions
Humidity.fit.GA.2 = gamlssMXfits(n = 2, X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity.fit.GA.2)
logLik(Humidity.fit.GA.2)
Humidity.fit.GA.2$prob # mixture weights
fitted(Humidity.fit.GA.2, "mu")[1]

# Fitting the variable with a mixture of 2 gamma distributions
Humidity.fit.GA.2 = gamlssMXfits(n = 2, X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity.fit.GA.2)
logLik(Humidity.fit.GA.2)
Humidity.fit.GA.2$prob # mixture weights
fitted(Humidity.fit.GA.2, "mu")[1]

# Fitting the variable with a mixture of 2 gamma distributions
Humidity.fit.GA.2 = gamlssMX(X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity.fit.GA.2)
logLik(Humidity.fit.GA.2)
Humidity.fit.GA.2$prob # mixture weights
fitted(Humidity.fit.GA.2, "mu")[1]






### ------------------------------------------------------------------------ ###
# LPG (liquefied petroleum gas)

summary(df$lpg)
var(df$lpg)
moments::skewness(df$lpg)
moments::kurtosis(df$lpg)



# EDA

jpeg(file="../LateX_project/images/chapter2/lpg_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=lpg)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/lpg_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=lpg)) +
  geom_histogram( binwidth=0.0005, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()


# Fitting

X2 = df$lpg


# From the kernel smoothing approach is evident that the variable is bimodal so it's good to try a mixture of distributions


# Fitting Humidity with mixture models

# Fitting the variable with a mixture of 2 gamma distributions
LPG_fit_GA_2 = gamlssMXfits(n = 2, X2~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity_fit_GA_2)
logLik(Humidity_fit_GA_2)
Humidity_fit_GA_2$prob # mixture weights
fitted(Humidity_fit_GA_2, "mu")[1]

mu_hat1 = exp(Humidity_fit_GA_2[["models"]][[1]][["mu_coefficients"]])    
sigma_hat1 = exp(Humidity_fit_GA_2[["models"]][[1]][["sigma_coefficients"]])

mu_hat2 = exp(Humidity_fit_GA_2[["models"]][[2]][["mu_coefficients"]])    
sigma_hat2 = exp(Humidity_fit_GA_2[["models"]][[2]][["sigma_coefficients"]])


hist(X4, breaks = 50,freq = FALSE)
lines(seq(min(X4),max(X4),length=length(X4)),fit_GA_2[["prob"]][1]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat1, sigma = sigma_hat1),lty=2,lwd=3,col=2)
lines(seq(min(X4),max(X4),length=length(X4)),fit_GA_2[["prob"]][2]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat2, sigma = sigma_hat2),lty=2,lwd=3,col=3)
lines(seq(min(X4),max(X4),length=length(X4)),
      fit_GA_2[["prob"]][1]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat1, sigma = sigma_hat1) +
        fit_GA_2[["prob"]][2]*dGA(seq(min(X4),max(X4),length=length(X4)), mu = mu_hat2, sigma = sigma_hat2),
      lty = 1, lwd = 3, col = 1)



# Fitting the variable with a mixture of 2 lognormal distributions
Humidity_fit_LOGNO_2 = gamlssMXfits(n = 2, X4~1, family = LOGNO, K = 2, data = NULL)  # n = initial points
str(Humidity_fit_LOGNO_2)
logLik(Humidity_fit_LOGNO_2)
Humidity_fit_LOGNO_2$prob # mixture weights
fitted(Humidity_fit_LOGNO_2, "mu")[1]

# Fitting the variable with a mixture of 2 gamma distributions
Humidity.fit.GA.2 = gamlssMXfits(n = 2, X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity.fit.GA.2)
logLik(Humidity.fit.GA.2)
Humidity.fit.GA.2$prob # mixture weights
fitted(Humidity.fit.GA.2, "mu")[1]

# Fitting the variable with a mixture of 2 gamma distributions
Humidity.fit.GA.2 = gamlssMXfits(n = 2, X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity.fit.GA.2)
logLik(Humidity.fit.GA.2)
Humidity.fit.GA.2$prob # mixture weights
fitted(Humidity.fit.GA.2, "mu")[1]

# Fitting the variable with a mixture of 2 gamma distributions
Humidity.fit.GA.2 = gamlssMX(X4~1, family = GA, K = 2, data = NULL)  # n = initial points
str(Humidity.fit.GA.2)
logLik(Humidity.fit.GA.2)
Humidity.fit.GA.2$prob # mixture weights
fitted(Humidity.fit.GA.2, "mu")[1]






### ------------------------------------------------------------------------ ###
# Smoke

summary(df$smoke)
var(df$smoke)
moments::skewness(df$smoke)
moments::kurtosis(df$smoke)


# EDA

jpeg(file="../LateX_project/images/chapter2/smoke_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=smoke)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/smoke_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=smoke)) +
  geom_histogram( binwidth=0.002, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()


# Fitting






### ------------------------------------------------------------------------ ###
# Temp (temperature, defined in R)

summary(df$temp)
var(df$temp)
moments::skewness(df$temp)
moments::kurtosis(df$temp)


# EDA

jpeg(file="../LateX_project/images/chapter2/temp_boxplot.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(y=temp)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()


jpeg(file="../LateX_project/images/chapter2/temp_histogram.jpeg", width = 6, height = 6, units = 'in', res = 200)

ggplot(df, aes(x=temp)) +
  geom_histogram( binwidth=1, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  theme(plot.title = element_text(size=15)) +
  theme_minimal()

dev.off()


# Fitting

Temp.fit.GA <- gamlss(df$temp ~ 1, family=GA)

Temp.fit.EXP = histDist(df$temp, family=EXP, nbins=150, main="Temperature exponential")
Temp.fit.GA = histDist(df$temp, family=GA, nbins=150, main="Temperature gamma")
Temp.fit.IG = histDist(df$temp, family=IG, nbins=150, main="Temperature inverse gaussian")
Temp.fit.LOGNO = histDist(df$temp, family=LOGNO, nbins=150, main="Temperature log-normal distribution")
Temp.fit.WEI = histDist(df$temp, family=WEI, nbins=150, main="Temperature weibull")
Temp.fit.LO = histDist(df$temp, family=LO, nbins=150, main="Temperature logistic")





# Mixtures








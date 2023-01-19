# iot-telemetry-analysis-univariate-fit

# Import libraries
library(anytime)
library(plyr)
#library(ggplot2)
#library(xtable)
#library(moments)
library(gamlss)
library(gamlss.mx)
library(gtools)
#library(tidyverse)
#library(dplyr)
#library(GGally)
#library(gridExtra)
#library(corrplot)
#library(cluster)
#library(factoextra)
#library(mclust)
#library(clValid)
#library(scales)
#library(fpc)
#library(hamlet)
#library(hopkins)
#library(NbClust)


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

df$device = plyr::revalue(df$device, c("b8:27:eb:bf:9d:51"="Device 1", "00:0f:00:70:91:0a"="Device 2", "1c:bf:ce:15:ec:4d"="Device 3"))


normalize=function(x){
  (x-min(x))/(max(x)-min(x))
}

families_R = c('NO','LO','GU','RG','exGAUS','TF')



### UNIVARIATE FITTING ###


### ----------------------------------------------------------------------------
# CO (Carbon monoxide)

summary(df$co)


co_logit = gtools::logit(normalize(df$co))
co_logit = sort(co_logit)[-c(1, 4998:5000)]  # Removed Inf values
summary(co_logit)


# Fitting

v = c()

for (f in families_R){
  jpeg(file=paste("../LateX_project/images/chapter2/fitting/co_logit_", f, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
  
  fit.model = histDist(co_logit, family=f, nbins=30, main=paste(f,'distribution'))
  
  dev.off()
  
  v = c(v, fit.model$df.fit) # number of parameters
  v = c(v, logLik(fit.model))
  v = c(v, stats::AIC(fit.model))  # AIC (to be minimized)
  v = c(v, fit.model$sbc)
}


M = matrix(v, ncol=4, byrow=TRUE)
rownames(M) = families_R
colnames(M) = c('df', 'logLik', 'AIC', 'BIC')
print(M)
print(xtable::xtable(M, type="latex", digits=5))


# Select best models according to the indexes
paste(names(which(M[,2]==max(M[,2]))), max(M[,2]), 'logLik', sep=' ')
paste(names(which(M[,3]==min(M[,3]))), min(M[,3]), 'AIC', sep=' ')
paste(names(which(M[,4]==min(M[,4]))), min(M[,4]), 'BIC', sep=' ')

M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),]
print(xtable::xtable(M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),], type="latex", digits=5))


# winning model = TF



# Mixtures with TF and k=2

fit_co_TF_2 = gamlss.mx::gamlssMXfits(n=5, co_logit~1, family=TF, K=2, data=NULL)

mu_hat1 = fit_co_TF_2[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_co_TF_2[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_co_TF_2[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_co_TF_2[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_co_TF_2[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_co_TF_2[["models"]][[2]][["nu.coefficients"]]))



jpeg(file="../LateX_project/images/chapter2/fitting/co_logit_TF_mixtures_2.jpeg", width=6, height=6, units='in', res=200)

hist(co_logit, breaks=30, freq=FALSE)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)), fit_co_TF_2[["prob"]][1]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)),
                                                                                                       mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)), fit_co_TF_2[["prob"]][2]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)),
                                                                                                       mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)),
      fit_co_TF_2[["prob"]][1]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)), mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_co_TF_2[["prob"]][2]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)), mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=1, lwd=3, col=1)

dev.off()



# Mixtures with TF and k=3

fit_co_TF_3 = gamlss.mx::gamlssMXfits(n=5, co_logit~1, family=TF, K=3, data=NULL)

mu_hat1 = fit_co_TF_3[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_co_TF_3[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_co_TF_3[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_co_TF_3[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_co_TF_3[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_co_TF_3[["models"]][[2]][["nu.coefficients"]]))

mu_hat3 = fit_co_TF_3[["models"]][[3]][["mu.coefficients"]]
sigma_hat3 = abs(exp(fit_co_TF_3[["models"]][[3]][["sigma.coefficients"]]))
nu_hat3= abs(exp(fit_co_TF_3[["models"]][[3]][["nu.coefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/co_logit_TF_mixtures_3.jpeg", width=6, height=6, units='in', res=200)

hist(co_logit, breaks=30, freq=FALSE)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)), fit_co_TF_3[["prob"]][1]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)),
                                                                                                       mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)), fit_co_TF_3[["prob"]][2]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)),
                                                                                                       mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)), fit_co_TF_3[["prob"]][2]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)),
                                                                                                       mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=2, lwd=3, col=4)

lines(seq(min(co_logit), max(co_logit), length=length(co_logit)),
      fit_co_TF_3[["prob"]][1]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)), mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_co_TF_3[["prob"]][2]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)), mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2) +
        fit_co_TF_3[["prob"]][3]*dTF(seq(min(co_logit), max(co_logit), length=length(co_logit)), mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=1, lwd=3, col=1)

dev.off()


LR.test(fit_co_TF_2, fit_co_TF_3)




### ----------------------------------------------------------------------------
# Humidity

summary(df$humidity)


humidity_logit = gtools::logit(normalize(df$humidity))
humidity_logit = sort(humidity_logit)[-c(1:2, 5000)]  # Removed Inf values
summary(humidity_logit)


# Fitting

v = c()

for (f in families_R){
  jpeg(file=paste("../LateX_project/images/chapter2/fitting/humidity_logit_", f, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
  
  fit.model = histDist(humidity_logit, family=f, nbins=30, main=paste(f,'distribution'))
  
  dev.off()
  
  v = c(v, fit.model$df.fit)  # number of parameters
  v = c(v, logLik(fit.model))
  v = c(v, stats::AIC(fit.model))  # AIC (to be minimized)
  v = c(v, fit.model$sbc)
}


M = matrix(v, ncol=4, byrow=TRUE)
rownames(M) = families_R
colnames(M) = c('df', 'logLik', 'AIC', 'BIC')
print(M)
print(xtable::xtable(M, type="latex", digits=5))


# Select best models achumidityrding to the indexes
paste(names(which(M[,2]==max(M[,2]))), max(M[,2]), 'logLik', sep=' ')
paste(names(which(M[,3]==min(M[,3]))), min(M[,3]), 'AIC', sep=' ')
paste(names(which(M[,4]==min(M[,4]))), min(M[,4]), 'BIC', sep=' ')

M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),]
print(xtable::xtable(M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),], type="latex", digits=5))



# Mixtures with exGAUS and K=2

fit_humidity_exGAUS_2 = gamlss.mx::gamlssMXfits(n=5, humidity_logit~1, family=exGAUS, K=2, data=NULL)

mu_hat1 = fit_humidity_exGAUS_2[["models"]][[1]][["mu.humidityefficients"]]
sigma_hat1 = abs(exp(fit_humidity_exGAUS_2[["models"]][[1]][["sigma.humidityefficients"]]))
nu_hat1 = abs(exp(fit_humidity_exGAUS_2[["models"]][[1]][["nu.humidityefficients"]]))

mu_hat2 = fit_humidity_exGAUS_2[["models"]][[2]][["mu.humidityefficients"]]
sigma_hat2 = abs(exp(fit_humidity_exGAUS_2[["models"]][[2]][["sigma.humidityefficients"]]))
nu_hat2 = abs(exp(fit_humidity_exGAUS_2[["models"]][[2]][["nu.humidityefficients"]]))



jpeg(file="../LateX_project/images/chapter2/fitting/humidity_logit_exGAUS_mixtures_2.jpeg", width=6, height=6, units='in', res=200)

hist(humidity_logit, breaks=30, freq=FALSE)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), fit_humidity_exGAUS_2[["prob"]][1]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
                                                                                                       mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), fit_humidity_exGAUS_2[["prob"]][2]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
                                                                                                       mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
      fit_humidity_exGAUS_2[["prob"]][1]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_humidity_exGAUS_2[["prob"]][2]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=1, lwd=3, humidityl=1)

dev.off()



# Mixtures with exGAUS and k=3

fit_humidity_exGAUS_3 = gamlss.mx::gamlssMXfits(n=5, humidity_logit~1, family=exGAUS, K=3, data=NULL)

mu_hat1 = fit_humidity_exGAUS_3[["models"]][[1]][["mu.humidityefficients"]]
sigma_hat1 = abs(exp(fit_humidity_exGAUS_3[["models"]][[1]][["sigma.humidityefficients"]]))
nu_hat1 = abs(exp(fit_humidity_exGAUS_3[["models"]][[1]][["nu.humidityefficients"]]))

mu_hat2 = fit_humidity_exGAUS_3[["models"]][[2]][["mu.humidityefficients"]]
sigma_hat2 = abs(exp(fit_humidity_exGAUS_3[["models"]][[2]][["sigma.humidityefficients"]]))
nu_hat2 = abs(exp(fit_humidity_exGAUS_3[["models"]][[2]][["nu.humidityefficients"]]))

mu_hat3 = fit_humidity_exGAUS_3[["models"]][[3]][["mu.humidityefficients"]]
sigma_hat3 = abs(exp(fit_humidity_exGAUS_3[["models"]][[3]][["sigma.humidityefficients"]]))
nu_hat3 = abs(exp(fit_humidity_exGAUS_3[["models"]][[3]][["nu.humidityefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/humidity_logit_exGAUS_mixtures_3.jpeg", width=6, height=6, units='in', res=200)

hist(humidity_logit, breaks=30, freq=FALSE)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), fit_humidity_exGAUS_3[["prob"]][1]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
                                                                                                       mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), fit_humidity_exGAUS_3[["prob"]][2]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
                                                                                                       mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), fit_humidity_exGAUS_3[["prob"]][2]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
                                                                                                       mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=2, lwd=3, col=4)

lines(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)),
      fit_humidity_exGAUS_3[["prob"]][1]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_humidity_exGAUS_3[["prob"]][2]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2) +
        fit_humidity_exGAUS_3[["prob"]][3]*dexGAUS(seq(min(humidity_logit), max(humidity_logit), length=length(humidity_logit)), mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=1, lwd=3, humidityl=1)

dev.off()




### ----------------------------------------------------------------------------
# LPG

summary(df$lpg)


lpg_logit = gtools::logit(normalize(df$lpg))
lpg_logit = sort(lpg_logit)[-c(1, 4998:5000)]  # Removed Inf values
summary(lpg_logit)


# Fitting

v = c()

for (f in families_R){
  jpeg(file=paste("../LateX_project/images/chapter2/fitting/lpg_logit_", f, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
  
  fit.model = histDist(lpg_logit, family=f, nbins=30, main=paste(f,'distribution'))
  
  dev.off()
  
  v = c(v, fit.model$df.fit)  # number of parameters
  v = c(v, logLik(fit.model))
  v = c(v, stats::AIC(fit.model))  # AIC (to be minimized)
  v = c(v, fit.model$sbc)
}


M = matrix(v, ncol=4, byrow=TRUE)
rownames(M) = families_R
colnames(M) = c('df', 'logLik', 'AIC', 'BIC')
print(M)
print(xtable::xtable(M, type="latex", digits=5))


# Select best models according to the indexes
paste(names(which(M[,2]==max(M[,2]))), max(M[,2]), 'logLik', sep=' ')
paste(names(which(M[,3]==min(M[,3]))), min(M[,3]), 'AIC', sep=' ')
paste(names(which(M[,4]==min(M[,4]))), min(M[,4]), 'BIC', sep=' ')

M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),]
print(xtable::xtable(M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),], type="latex", digits=5))



# Mixtures with TF and K=2

fit_lpg_TF_2 = gamlss.mx::gamlssMXfits(n=5, lpg_logit~1, family=exGAUS, K=2, data=NULL)

mu_hat1 = fit_lpg_TF_2[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_lpg_TF_2[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_lpg_TF_2[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_lpg_TF_2[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_lpg_TF_2[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_lpg_TF_2[["models"]][[2]][["nu.coefficients"]]))



jpeg(file="../LateX_project/images/chapter2/fitting/lpg_logit_TF_mixtures_2.jpeg", width=6, height=6, units='in', res=200)

hist(lpg_logit, breaks=30, freq=FALSE)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
      fit_lpg_TF_2[["prob"]][1]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
      fit_lpg_TF_2[["prob"]][2]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
                                    mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
      fit_lpg_TF_2[["prob"]][1]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_lpg_TF_2[["prob"]][2]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
                                      mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=1, lwd=3, col=1)

dev.off()



# Mixtures with TF and K=3

fit_lpg_TF_3 = gamlss.mx::gamlssMXfits(n=5, lpg_logit~1, family=exGAUS, K=3, data=NULL)

mu_hat1 = fit_lpg_TF_3[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_lpg_TF_3[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_lpg_TF_3[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_lpg_TF_3[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_lpg_TF_3[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_lpg_TF_3[["models"]][[2]][["nu.coefficients"]]))

mu_hat3 = fit_lpg_TF_3[["models"]][[3]][["mu.coefficients"]]
sigma_hat3 = abs(exp(fit_lpg_TF_3[["models"]][[3]][["sigma.coefficients"]]))
nu_hat3 = abs(exp(fit_lpg_TF_3[["models"]][[3]][["nu.coefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/lpg_logit_TF_mixtures_3.jpeg", width=6, height=6, units='in', res=200)

hist(lpg_logit, breaks=30, freq=FALSE)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
      fit_lpg_TF_3[["prob"]][1]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
      fit_lpg_TF_3[["prob"]][2]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
                                    mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
      fit_lpg_TF_3[["prob"]][3]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
                                    mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=2, lwd=3, col=4)

lines(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)),
      fit_lpg_TF_3[["prob"]][1]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_lpg_TF_3[["prob"]][2]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
                                      mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2) +
        fit_lpg_TF_3[["prob"]][3]*dTF(seq(min(lpg_logit), max(lpg_logit), length=length(lpg_logit)), 
                                      mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=1, lwd=3, col=1)

dev.off()


LR.test(fit_lpg_TF_2, fit_lpg_TF_3)





### ----------------------------------------------------------------------------
# Smoke

summary(df$smoke)


smoke_logit = gtools::logit(normalize(df$smoke))
smoke_logit = sort(smoke_logit)[-c(1, 4998:5000)]  # Removed Inf values
summary(smoke_logit)


# Fitting

v = c()

for (f in families_R){
  jpeg(file=paste("../LateX_project/images/chapter2/fitting/smoke_logit_", f, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
  
  fit.model = histDist(smoke_logit, family=f, nbins=30, main=paste(f,'distribution'))
  
  dev.off()
  
  v = c(v, fit.model$df.fit)  # number of parameters
  v = c(v, logLik(fit.model))
  v = c(v, stats::AIC(fit.model))  # AIC (to be minimized)
  v = c(v, fit.model$sbc)
}


M = matrix(v, ncol=4, byrow=TRUE)
rownames(M) = families_R
colnames(M) = c('df', 'logLik', 'AIC', 'BIC')
print(M)
print(xtable::xtable(M, type="latex", digits=5))


# Select best models according to the indexes
paste(names(which(M[,2]==max(M[,2]))), max(M[,2]), 'logLik', sep=' ')
paste(names(which(M[,3]==min(M[,3]))), min(M[,3]), 'AIC', sep=' ')
paste(names(which(M[,4]==min(M[,4]))), min(M[,4]), 'BIC', sep=' ')

M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),]
print(xtable::xtable(M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),], type="latex", digits=5))



# Mixtures with TF and K=2

fit_smoke_TF_2 = gamlss.mx::gamlssMXfits(n=5, smoke_logit~1, family=exGAUS, K=2, data=NULL)

mu_hat1 = fit_smoke_TF_2[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_smoke_TF_2[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_smoke_TF_2[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_smoke_TF_2[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_smoke_TF_2[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_smoke_TF_2[["models"]][[2]][["nu.coefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/smoke_logit_TF_mixtures_2.jpeg", width=6, height=6, units='in', res=200)

hist(smoke_logit, breaks=30, freq=FALSE)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
      fit_smoke_TF_2[["prob"]][1]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
      fit_smoke_TF_2[["prob"]][2]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
                                    mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
      fit_smoke_TF_2[["prob"]][1]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_smoke_TF_2[["prob"]][2]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
                                      mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=1, lwd=3, col=1)

dev.off()



# Mixtures with TF and K=3

fit_smoke_TF_3 = gamlss.mx::gamlssMXfits(n=5, smoke_logit~1, family=exGAUS, K=3, data=NULL)

mu_hat1 = fit_smoke_TF_3[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_smoke_TF_3[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_smoke_TF_3[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_smoke_TF_3[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_smoke_TF_3[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_smoke_TF_3[["models"]][[2]][["nu.coefficients"]]))

mu_hat3 = fit_smoke_TF_3[["models"]][[3]][["mu.coefficients"]]
sigma_hat3 = abs(exp(fit_smoke_TF_3[["models"]][[3]][["sigma.coefficients"]]))
nu_hat3 = abs(exp(fit_smoke_TF_3[["models"]][[3]][["nu.coefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/smoke_logit_TF_mixtures_3.jpeg", width=6, height=6, units='in', res=200)

hist(smoke_logit, breaks=30, freq=FALSE)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
      fit_smoke_TF_3[["prob"]][1]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
      fit_smoke_TF_3[["prob"]][2]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
                                    mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
      fit_smoke_TF_3[["prob"]][3]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
                                    mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=2, lwd=3, col=4)

lines(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)),
      fit_smoke_TF_3[["prob"]][1]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
                                    mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_smoke_TF_3[["prob"]][2]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
                                      mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2) +
        fit_smoke_TF_3[["prob"]][3]*dTF(seq(min(smoke_logit), max(smoke_logit), length=length(smoke_logit)), 
                                      mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=1, lwd=3, col=1)

dev.off()






### ----------------------------------------------------------------------------
# Temperature

summary(df$temp)


temp = df$temp


# Fitting

v = c()

for (f in families_R){
  jpeg(file=paste("../LateX_project/images/chapter2/fitting/temp_", f, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
  
  fit.model = histDist(temp, family=f, nbins=30, main=paste(f,'distribution'))
  
  dev.off()
  
  v = c(v, fit.model$df.fit)  # number of parameters
  v = c(v, logLik(fit.model))
  v = c(v, stats::AIC(fit.model))  # AIC (to be minimized)
  v = c(v, fit.model$sbc)
}


M = matrix(v, ncol=4, byrow=TRUE)
rownames(M) = families_R
colnames(M) = c('df', 'logLik', 'AIC', 'BIC')
print(M)
print(xtable::xtable(M, type="latex", digits=5))


# Select best models according to the indexes
paste(names(which(M[,2]==max(M[,2]))), max(M[,2]), 'logLik', sep=' ')
paste(names(which(M[,3]==min(M[,3]))), min(M[,3]), 'AIC', sep=' ')
paste(names(which(M[,4]==min(M[,4]))), min(M[,4]), 'BIC', sep=' ')

M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),]
print(xtable::xtable(M[c(names(which(M[,2]==max(M[,2]))), names(which(M[,3]==min(M[,3]))), names(which(M[,4]==min(M[,4])))),], type="latex", digits=5))



# Mixtures with exGAUS and K=2

fit_temp_exGAUS_2 = gamlss.mx::gamlssMXfits(n=5, temp~1, family=exGAUS, K=2, data=NULL)

mu_hat1 = fit_temp_exGAUS_2[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_temp_exGAUS_2[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_temp_exGAUS_2[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_temp_exGAUS_2[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_temp_exGAUS_2[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_temp_exGAUS_2[["models"]][[2]][["nu.coefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/temp_exGAUS_mixtures_2.jpeg", width=6, height=6, units='in', res=200)

hist(temp, breaks=30, freq=FALSE)

lines(seq(min(temp), max(temp), length=length(temp)), 
      fit_temp_exGAUS_2[["prob"]][1]*dTF(seq(min(temp), max(temp), length=length(temp)),
                                      mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(temp), max(temp), length=length(temp)), 
      fit_temp_exGAUS_2[["prob"]][2]*dTF(seq(min(temp), max(temp), length=length(temp)),
                                      mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(temp), max(temp), length=length(temp)),
      fit_temp_exGAUS_2[["prob"]][1]*dTF(seq(min(temp), max(temp), length=length(temp)), 
                                      mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_temp_exGAUS_2[["prob"]][2]*dTF(seq(min(temp), max(temp), length=length(temp)), 
                                        mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=1, lwd=3, col=1)

dev.off()



# Mixtures with exGAUS and K=3

fit_temp_exGAUS_3 = gamlss.mx::gamlssMXfits(n=5, temp~1, family=exGAUS, K=3, data=NULL)

mu_hat1 = fit_temp_exGAUS_3[["models"]][[1]][["mu.coefficients"]]
sigma_hat1 = abs(exp(fit_temp_exGAUS_3[["models"]][[1]][["sigma.coefficients"]]))
nu_hat1 = abs(exp(fit_temp_exGAUS_3[["models"]][[1]][["nu.coefficients"]]))

mu_hat2 = fit_temp_exGAUS_3[["models"]][[2]][["mu.coefficients"]]
sigma_hat2 = abs(exp(fit_temp_exGAUS_3[["models"]][[2]][["sigma.coefficients"]]))
nu_hat2 = abs(exp(fit_temp_exGAUS_3[["models"]][[2]][["nu.coefficients"]]))

mu_hat3 = fit_temp_exGAUS_3[["models"]][[3]][["mu.coefficients"]]
sigma_hat3 = abs(exp(fit_temp_exGAUS_3[["models"]][[3]][["sigma.coefficients"]]))
nu_hat3 = abs(exp(fit_temp_exGAUS_3[["models"]][[3]][["nu.coefficients"]]))


jpeg(file="../LateX_project/images/chapter2/fitting/temp_exGAUS_mixtures_3.jpeg", width=6, height=6, units='in', res=200)

hist(temp, breaks=30, freq=FALSE)

lines(seq(min(temp), max(temp), length=length(temp)), 
      fit_temp_exGAUS_3[["prob"]][1]*dTF(seq(min(temp), max(temp), length=length(temp)),
                                         mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1), lty=2, lwd=3, col=2)

lines(seq(min(temp), max(temp), length=length(temp)), 
      fit_temp_exGAUS_3[["prob"]][2]*dTF(seq(min(temp), max(temp), length=length(temp)),
                                         mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2), lty=2, lwd=3, col=3)

lines(seq(min(temp), max(temp), length=length(temp)), 
      fit_temp_exGAUS_3[["prob"]][3]*dTF(seq(min(temp), max(temp), length=length(temp)),
                                         mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=2, lwd=3, col=4)

lines(seq(min(temp), max(temp), length=length(temp)),
      fit_temp_exGAUS_3[["prob"]][1]*dTF(seq(min(temp), max(temp), length=length(temp)), 
                                         mu=mu_hat1, sigma=sigma_hat1, nu=nu_hat1) +
        fit_temp_exGAUS_3[["prob"]][2]*dTF(seq(min(temp), max(temp), length=length(temp)), 
                                           mu=mu_hat2, sigma=sigma_hat2, nu=nu_hat2) +
        fit_temp_exGAUS_3[["prob"]][3]*dTF(seq(min(temp), max(temp), length=length(temp)), 
                                           mu=mu_hat3, sigma=sigma_hat3, nu=nu_hat3), lty=1, lwd=3, col=1)

dev.off()



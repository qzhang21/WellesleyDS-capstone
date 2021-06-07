# ruhlman

# DATA EXPLORATION
setwd("Desktop/Ruhlman")
library("readxl")
data = read_excel("data.xlsx",col_names=T)
dim(data)
head(data)
colnames(data)
summary(data)
str(data)
data=as.data.frame(data)
# striosomality == 1 --> Strio - should I convert? nope.
data[data$striosomality=='Strio',]$striosomality = 1
data[data$striosomality=='Matrix',]$striosomality = 0
data$striosomality = as.double(data$striosomality)

# check correlation matrix
# 1) eliminate multicollin
# non-zero reward traces are pretty highly correlated --> take average non-zero reward?
# data$avgReward = (data$reward1+data$reward2+data$reward3)/3
# take average cost to be y-variable
data$y = data$cost1*0.375+data$cost2*0.375+data$cost3*0.25
#data$striosomality = as.factor(data$striosomality)

attach(data)
str(data)

# check identical slope and constant variance assumption for striosomality
with(subset(data,striosomality==1),plot(y~firstSessionAge,col="blue"))
with(subset(data,striosomality==0),points(y~firstSessionAge,col="red"))
f1 = lm(y~firstSessionAge,subset=striosomality==1)
f2 = lm(y~firstSessionAge,subset=striosomality==0)
abline(f1,lty=1)
abline(f2,lty=2)

with(subset(data,striosomality==1),plot(y~cost0,col="blue"))
with(subset(data,striosomality==0),points(y~cost0,col="red"))
f1 = lm(y~cost0,subset=striosomality==1)
f2 = lm(y~cost0,subset=striosomality==0)
abline(f1,lty=1)
abline(f2,lty=2)

with(subset(data,striosomality==1),plot(y~reward0,col="blue"))
with(subset(data,striosomality==0),points(y~reward0,col="red"))
f1 = lm(y~reward0,subset=striosomality==1)
f2 = lm(y~reward0,subset=striosomality==0)
abline(f1,lty=1)
abline(f2,lty=2)

with(subset(data,striosomality==1),plot(y~reward1,col="blue"))
with(subset(data,striosomality==0),points(y~reward1,col="red"))
f1 = lm(y~reward1,subset=striosomality==1)
f2 = lm(y~reward1,subset=striosomality==0)
abline(f1,lty=1)
abline(f2,lty=2)

# create separate data for strio and matrix
data_s = data[striosomality==1,c(3,7:12)]
data_m = data[striosomality==0,c(3,7:12)]

# 2) VIF for quant. predictors
library(usdm)
vifstep(data_s[,-7],th=5)
# goodbye reward2
data_s = data_s[,-5]
str(data_s)

vifstep(data_m[,-7],th=5)
# goodbye reward1, reward2
data_m = data_m[,-c(4,5)]
str(data_m)

# fitting the model
# 3) run two different procedures AIC/BIC, arrive at a couple best 1st order models
fm_s = lm(data_s$y~.,data=data_s)
summary(fm_s)
model_aic_s = step(fm_s, direction="both", k=2)
# data_s$y ~ firstSessionAge + reward0 + reward1
model_bic_s = step(fm_s, direction="both", k=log(nrow(data)))
# data_s$y ~ reward0 + reward1

fm_m = lm(data_m$y~.,data=data_m)
summary(fm_m)
model_aic_m = step(fm_m, direction="both", k=2)
# data_m$y ~ cost0 + reward3
model_bic_m = step(fm_m, direction="both", k=log(nrow(data)))
# data_m$y ~ cost0 + reward

# 4) apply cross validation techniques to determine best
# CV scores (LOOCV)
set.seed(1)
# loocv
library("asbio")
press(fm_s) # 572.7292
press(model_aic_s) # 536.3569 - lowest! R^2 0.4774, adj 0.4251
press(model_bic_s) # 572.923
summary(model_aic_s)
press(fm_m) # 305.9396
press(model_aic_m) # 268.7525 - lowest, R^2 0.5975, adj 0.564
press(model_bic_m) # same as above
summary(model_aic_m)
# 5) consider interaction terms to further refine model
# data_s$y ~ firstSessionAge + reward0 + reward1
# data_m$y ~ cost0 + reward3
m1_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward0*firstSessionAge,data=data_s)
m2_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward1*firstSessionAge,data=data_s)
m3_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward0*firstSessionAge+reward1*firstSessionAge,data=data_s)
m4_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward0*reward1,data=data_s)
m5_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward0*reward1+firstSessionAge*reward0,data=data_s) 
m6_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward0*reward1+firstSessionAge*reward1,data=data_s)
m7_s = lm(data_s$y~firstSessionAge+reward0+reward1+reward0*reward1+firstSessionAge*reward0+firstSessionAge*reward1,data=data_s)
press(m1_s) # 564.8076
press(m2_s) # 570.6745
press(m3_s) # 594.6896
press(m4_s) # none of these are better
press(m5_s)
press(m6_s)
press(m7_s)
# no interactions

m1_m = lm(data_m$y~cost0+reward3+cost0*reward3,data=data_m)
press(m1_m) # 262.3974
summary(m1_m) # R^2 is a tad better but it doesn't really make sense

# 6) regression diagnosis (check assumptions, transformations, etc.)
# linearity of ivs and dp; normal dist of residuals; homoscedasticity

# ===STRIO===
ms = model_aic_s
summary(ms)
par(mfrow=c(2,2))
plot(ms)

# possibly slightly heteroskedastic and non-normal residuals, so consider transforming
library("MASS")
addVal = abs(min(data_s$y)) + 0.01
ms_t = lm((data_s$y+addVal)~firstSessionAge+reward0+reward1,data=data_s)
summary(ms_t)
boxcox(ms_t) # lambda=1

ms_t2 = lm(log(data_s$y+addVal)~firstSessionAge+reward0+reward1,data=data_s)
par(mfrow=c(2,2))
plot(ms_t2)
summary(ms_t2)
addVal_r1 = abs(min(data_s$reward1))+0.01
addVal_r0 = abs(min(data_s$reward0))+0.01
ms_t3 = lm(log(data_s$y+addVal)~firstSessionAge+log(reward0+addVal_r0)+reward1,data=data_s)
par(mfrow=c(2,2))
plot(ms_t3)
ms_t4 = lm(log(data_s$y+addVal)~firstSessionAge+reward0+log(reward1+addVal_r1),data=data_s)
par(mfrow=c(2,2))
plot(ms_t4)
ms_t5 = lm(log(data_s$y+addVal)~log(firstSessionAge)+reward0+reward1,data=data_s)
par(mfrow=c(2,2))
plot(ms_t5)
ms_t6 = lm(data_s$y~firstSessionAge+log(reward0+addVal_r0)+reward1,data=data_s)
par(mfrow=c(2,2))
plot(ms_t6) #ok
summary(ms_t6)

ms_t7 = lm(data_s$y~firstSessionAge+reward0+log(reward1+addVal_r1),data=data_s)
par(mfrow=c(2,2))
plot(ms_t7) # ok
summary(ms_t7) # better

ms_t8 = lm(data_s$y~log(firstSessionAge)+reward0+reward1,data=data_s)
par(mfrow=c(2,2))
plot(ms_t8) # ok
summary(ms_t8) # nope
ms_t9 = lm(data_s$y~firstSessionAge+log(reward0+addVal_r0)+log(reward1+addVal_r1),data=data_s)
par(mfrow=c(2,2))
plot(ms_t9) # ok
summary(ms_t9) # nope
ms_t10 = lm(data_s$y~log(firstSessionAge)+log(reward0+addVal_r0)+reward1,data=data_s)
par(mfrow=c(2,2))
plot(ms_t10) # ok
summary(ms_t10) # nope
ms_t11 = lm(data_s$y~log(firstSessionAge)+reward0+log(reward1+addVal_r1),data=data_s)
par(mfrow=c(2,2))
plot(ms_t11) # ok
summary(ms_t11) # nope
ms_t12 = lm(data_s$y~log(firstSessionAge)+log(reward0+addVal_r0)+log(reward1+addVal_r1),data=data_s)
par(mfrow=c(2,2))
plot(ms_t12) # ok
summary(ms_t12) # nope

outliers_t7 = c(3,4,33)
addVal_r1_2 = abs(min(data_s$reward1[-outliers_t7]))+0.01
ms_t7_2 = lm(y~firstSessionAge+reward0+log(reward1+addVal_r1_2),data=data_s[-outliers_t7,])
par(mfrow=c(2,2))
plot(ms_t7_2)
summary(ms_t7_2) # great, this will be our Strio model

MODEL_S = ms_t7_2
press(MODEL_S) # 282.7289 - acceptable
# ===Matrix===
mm = model_bic_m
summary(mm)
par(mfrow=c(2,2))
plot(mm)
# slightly heteroskedastic and non-normal residuals. First consider removing outliers
outliers_mm = c(41,50,56)
data_mm = data[-outliers_mm,]
data_mm = data_mm[data_mm$striosomality==0,]
mm_2 = lm(y ~ cost0 + reward3,data=data_mm)
par(mfrow=c(2,2))
plot(mm_2)
summary(mm_2) # great, this will be our model

MODEL_M = mm_2
press(MODEL_M) # 83.61072 - great
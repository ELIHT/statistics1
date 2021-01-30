#phan tich tong hop anh huong bien thien di xa hon phan tich tong hop anh huong
#bat bien mot buoc bang cach xem xet den nhung khac biet giua cac nghien cuu. 

#phan tich tong hop anh huong bat bien va phan tich tong hop anh huong
#bien thien chi khac nhau o phuong 

#phan tich tong hop bien nhi phan - bo sung calcium va vitamin D
#va gay co xuong dui

study = c("Chapuy (1992)", "Dawson-Hughes (1997", "Chapuy (2002)", 
          "Harwood (2004", "Avenell (20004)", "Grant (2005)", 
          "Porthouse (2005)", "Jackson (2006)", "Salovaara (2010)")
treat.events = c(80, 0, 27, 1, 1, 46, 8, 175, 4)
treat.n = c(1387, 187, 389, 75, 35, 1306, 1321, 18176, 1586)
control.events = c(110, 1, 21, 1, 1, 41, 17, 199, 2)
control.n = c(1403, 202, 194, 37, 35, 1332, 1993, 18106, 1609)

cad = data.frame(study, treat.events, treat.n, control.events, control.n)

cad

install.packages("metafor")
library(Matrix)
library(metafor)

#tinh effect size la RR
#ham escalc
#RR duoc hoan chuyen sang don vi log => logRR (tuc bien yi)
#va phuong sai cua logRR (bien vi)
es = escalc(measure = "RR", ai = treat.events, bi = treat.n-treat.events, 
            ci=control.events, di=control.n-control.events, data=cad, append=T) 
es[, c("yi", "vi")]

# yi     vi 
#1 -0.3070 0.0202 
#2 -1.0218 2.6564 
#3 -0.4444 0.0769 
#4 -0.7066 1.9596 
#5  0.0000 1.9429 
#6  0.1348 0.0446 
#7 -0.3425 0.1826 
#8 -0.1324 0.0106 
#9  0.7075 0.7487 

#ham rma de tinh logRR cho 9 nghien cuu (mo hinh random effects)
re.model = rma(yi, vi, data = es)
summary(re.model)

#Random-Effects Model (k = 9; tau^2 estimator: REML)

#logLik  deviance       AIC       BIC      AICc 
#-4.8454    9.6907   13.6907   13.8496   16.0907   

#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0186)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00

#Test for Heterogeneity:
#  Q(df = 8) = 5.7511, p-val = 0.6751

#Model Results:
  
#  estimate      se     zval    pval    ci.lb    ci.ub 
#-0.1714  0.0731  -2.3465  0.0189  -0.3147  -0.0282

#ham forest
forest(re.model, slab = study, atransf = exp, xlab = "Relative Risk")

#Bieu do pheu funnel plot: khong cho thay co bang chung ve hieu ung cong bo
funnel(re.model)


#--------------------
#MO HINH FIXED EFFECTS
es = escalc(measure = "RR", ai = treat.events, bi = treat.n-treat.events, 
            ci=control.events, di=control.n-control.events, data=cad, append=T)
fe.model = rma(yi, vi, method = "FE", data = es)
summary(fe.model)
#Fixed-Effects Model (k = 9)

#logLik  deviance       AIC       BIC      AICc 
#-4.2465    5.7511   10.4930   10.6902   11.0644   

#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  0.72

#Test for Heterogeneity:
#  Q(df = 8) = 5.7511, p-val = 0.6751

#Model Results:
  
#  estimate      se     zval    pval    ci.lb    ci.ub 
#-0.1714  0.0731  -2.3465  0.0189  -0.3147  -0.0282

forest(fe.model, slab = study, atransf = exp, xlab = "Relative Risk")


#----------------------
#MO HINH BAYES
install.packages("bayesmeta")
library(magrittr)
library(forestplot)
library(grid)
library(checkmate)
library(numDeriv)
library(bayesmeta)

#tinh effect size cho moi nghien cuu
es = escalc(measure = "RR", ai = treat.events, bi = treat.n-treat.events, 
            ci=control.events, di=control.n-control.events, data=cad, append=T)
tauprior = function(t) {dunif(t, 0, 100)}
ma0 = bayesmeta(es, labels = es$Authors, mu.prior.mean = 0, mu.prior.sd = 100,
                tau.prior = tauprior, data = es)

ma1 = bayesmeta(es, labels = es$Authors, mu.prior.mean = 0, mu.prior.sd =0.1034,
                tau.prior = tauprior, data = es)

ma2 = bayesmeta(es, label=es$Authors, mu.prior.mean=-0.301, mu.prior.sd=0.1401,
                tau.prior = tauprior, data = es)

summary(ma0); summary(ma1); summary(ma2)


#----------------------
#PHAN TICH TONG HOP BIEN LIEN TUC

n1 = c(155, 31, 75, 18, 8, 57, 34, 110, 60)
los1 = c(55, 27, 64, 66, 14, 19, 52, 21, 30)
sd1 = c(47, 7, 17, 20, 8, 7, 45, 16, 27)
n2 = c(156, 32, 71, 18, 13, 52, 33, 183, 52)
los2 = c(75, 29, 119, 137, 18, 18, 41, 31, 23)
sd2 = c(64, 4, 29, 48, 11, 4, 34, 27, 20)
los = data.frame(n1, los1, sd1, n2, los2, sd2)

#library(metafor)
es2 = escalc(measure = "MD", n1i = n1, m1i = los1, sd1i = sd1, n2i = n2, 
             m2i = los2, sd2i = sd2, data = los, append = T)
es2

res = rma(yi, vi, data = es2)
summary(res)

plot(res)

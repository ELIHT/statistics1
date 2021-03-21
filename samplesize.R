#UOC TINH CO MAU
#1 - Mo hinh nghien cuu: dua tren thoi gian va dac tinh
#Thoi gian: tai mot thoi diem hien tai (present) cross-sectional study
#~nghien cuu cat ngang/ nghien cuu tai mot thoi diem

#trong mot thoi gian, hien tai va tuong lai prospective/longitudinal study
#~nghien cuu xuoi thoi gian

#tien hanh o hien tai nhung co dinh huong tim hieu qua khu retrospective study
#~nghien cuu doi chung/ case-control study

#2 - Tieu chi
#qdinh 1 tieu chi so sanh chinh (primary outcome measure) de can cu vao do 
#uoc tinh co mau

#3 - Muc do anh huong (effect size/ standardized difference)

#4 - Sai sot loai I, II va khai niem "power"

#uoc tinh co mau cho mot ti le
#p = ti le hien hanh muon uoc tinh
#m = margin error (sai so chuan)
#gia du khoang tin cay 95%, tuc alpha = 0.05
#n = so co mau can thiet

p = 0.2
m = 0.05
n = (1.96/m)^2*p*(1-p)
cbind(p,m,n)
#p    m        n
#[1,] 0.2 0.05 245.8624

#cach khac
library(foreign)
library(MASS)
library(nnet)
library(epiDisplay)
n.for.survey(p=0.20, delta=0.05, alpha=0.05)
#Sample size for survey. 
#Assumptions: 
#  Proportion       = 0.2 
#Confidence limit = 95 % 
#Delta            = 0.05 from the estimate. 

#Sample size      = 246 

#Uoc tinh co mau cho mot so trung binh
#s = do lech chuan
#d = margin error
#n = so co mau can thiet

s = 0.20
d = 0.05
n2 = (1.96*s/d)^2
cbind(s, d, n2)
#s    d      n2
#[1,] 0.2 0.05 61.4656

#uoc tinh co mau cho 2 ti le
library(epiDisplay)
n.for.2p(p1 = 0.10, p2 = 0.06, power = 0.9, alpha = 0.01, ratio = 1)
#Estimation of sample size for testing Ho: p1==p2 
#Assumptions: 
  
#  alpha = 0.01 
#power = 0.9 
#p1 = 0.1 
#p2 = 0.06 
#n2/n1 = 1 

#Estimated required sample size: 
  
#  n1 = 1416 
#  n2 = 1416 
#n1 + n2 = 2832 

#uoc tinh co mau cho 2 so trung binh
library(epiDisplay)
n.for.2means(mu1 = 0.80, mu2 = 0.84, sd1 = 0.12, sd2 = 0.12, 
             power = 0.9, alpha = 0.05, ratio = 1)
#Estimation of sample size for testing Ho: mu1==mu2 
#Assumptions: 
  
#  alpha = 0.05 
#  power = 0.9 
#  n2/n1 = 1 
#  mu1 = 0.8 
#  mu2 = 0.84 
#  sd1 = 0.12 
#  sd2 = 0.12 

#  Estimated required sample size: 
  
#  n1 = 190 
#  n2 = 190 
#  n1 + n2 = 380 

#uoc tinh co mau cho ti so nguy co (risk ratio)
library(epiR)
smoke = 1.4*(5*413)/1000000
nonsmoke = (5*413)/100000
epi.cohortsize(exposed=smoke, unexposed=nonsmoke, n=NA, power=0.90, r = 1, 
               design = 1, sided.test = 1, conf.level = 0.95)
$n.crude
$n.total
$n.exposed
$n.unexposed

#uoc tinh co mau cho odds ratio trong nghien cuu benh chung
library(epiR)
epi.ccsize(OR=2, p0=0.30, n=NA, power=0.90, r=1, sided.test=2, conf.level=0.95, 
           method="unmatched")
$n.total
$n.case
$n.control

#uoc tinh co mau so sanh hai survival curve hoac uoc tinh hazard ratio
epi.survivalsize(treat = 0.50, control=0.60, n=NA, power=0.80, r=1, design=1, 
                 sided.test=2, conf.level=0.95)
$n.crude
$n.total
$n.treat
$n.control

#uoc tinh co mau cho nghien cuu chan doan
install.packages("MKmisc")
library(MKmisc)
power.diagnostic.test(sens=0.95, delta=0.10, power=0.80)


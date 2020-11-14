#TINH TOAN XAC SUAT

#cigarettes
install.packages("plyr")
library(plyr)
cig = read.table("http://ww2.amstat.org/publications/jse/datasets/cigarettes.dat.txt")
cig = rename (cig, c("V1"="brand","V2"="tar","V3"="nicotine","V4"="weight","V5"="carbon"))
head(cig)

#primary biliary cirrhosis
pbc = read.table("http://www4.stat.ncsu.edu/~boos/var.select/pbc.dat.txt")
pbc = rename (pbc, c("V1"="id","V2"="futime","V3"="status","V4"="drug","V5"="age",
                     "V6"="sex","V7"="ascites","V8"="hepato","V9"="spiders","V10"="edema",
                     "V11"="bili","V12"="chol","V13"="albumin","V14"="copper","V15"="alk_phos",
                     "V16"="sgot","V17"="trig","V18"="platelet","V19"="protime","V20"="stage"))
head(pbc)

#probability distribution of logRR
m=log(1.42); sd=log(1.75/1.15)/3.92
RR=seq(-0.5, 1.00, 0.01)
drr=dnorm(RR,m,sd)
rr=data.frame(z=RR, ht=drr)
zc=log(1.30)
plot(rr,type="n", ylab="Probability", xlab="LogRR", main="Probability distibution of logRR")
t=subset(rr,z>=zc)
polygon(c(rev(t$z),t$z),
        c(rep(0,nrow(t)),t$ht),col="lightblue",
        border = NA)
lines(rr,lwd=2)

#phan bo nhi thuc (binomial distribution)
pnorm(149, mean=163.3, sd=6.6)
dbinom(5, 50, 0.2)

#phan bo poisson
ppois(16,lambda=12,lower=FALSE)
ppois(16,lambda=12)


#phan bo chuan
p_170 = pnorm(170, mean=156, sd=6)
p_170
p_over_170 = 1-pnorm(170, mean=156, sd=6)
p_over_170

p_between_150_156 = pnorm(156, mean=156, sd=6)-pnorm(150, mean=156, sd=6)
p_between_150_156

mean = 52; sd = 12
se = sd/sqrt(10)
pnorm(55,mean,se) - pnorm(45,mean,se)

#phan bo nhi phan
dbinom(6,12,prob=1/4)
dbinom(5,12,prob=1/4)

pbinom(6,size=12,prob=1/4)

#phan bo poisson
dpois(2,0.5)
ppois(2,0.5)

#phan bo mu
pexp(120,1/150)

#luat so lon
#mo phong 10000 lan mot bien so X gom 2 gia tri 0 va 1
set.seed(123)
n=10000
x=sample(0:1,n,replace=T)
s=cumsum(x)
r=s/(1:n)
plot(r, ylim=c(0.40, 1.00), type="l")
lines(c(0,n),c(0.5,0.5),col="red")

#lay mau mot bien ngau nhien x co trung binh 68 va do lech chuan 5.5
n = c(1:10000)
x = rnorm (10000, mean=68, sd=5.5)
s = cumsum(x)
plot(s/n, xlab = "n", ylab = "Trung binh mau", type = "l")
abline(a = 68, b = 0, col="red")

#central limit theorem
wt = c(85, 61, 75, 56, 72, 70, 68, 65, 57, 80)
sample1 = sample(wt, 7)
mean(sample1); var(sample1)

sample2 = sample(wt, 7)
mean(sample2); var(sample2)

sample3 = sample(wt, 7)
mean(sample3); var(sample3)
#so on
set.seed(123)
n = 10000
sample.means = numeric(n)
for (i in 1:n) {
  samplei = sample(wt, 7)
  sample.means[i] = mean(samplei)
}

mean(sample.means)
var(sample.means)
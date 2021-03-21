#epidemiology

#Sens va Spec
#Do nhay = P(thu nghiem(+)|benh nhan mac benh) = 1 - ty le duong tinh gia
#do nhay con duoc goi la ty le duong tinh that

#Do dac hieu = P(thu nghiem(-)|benh nhan khong mac benh) = 1 - ty le am tinh gia
#do dac hieu con duoc goi a ty le am tinh that

#Gia tri du doan duong tinh PPV, mot bai toan dang Bayesian

#Duong cong ROC nguyen thuy dc tinh toan trong cong nghe radar dung de phan biet
#muc do thay doi cua tin hieu vien trong kha nang bam sinh nhan dang tin hieu
#hien nay duong cong ROC thuong dc dung de danh gia chat lg cuong do phan biet
#cua 1 thu nghiem su dung so lieu cho phep chung ta co the danh gia dc kha nang
#phan biet cua mot thu nghiem su dung so lieu co bieu thi do nhay va do dac hieu

#uoc luong dien tich duoi duong cong ROC (AUC) => do do chinh xac
#neu dien tich duoi duong cong <= 0.5, cuong do phan biet cua phep thu khong tot
#ket qua neu co thu duoc cung chi la ngau nhien
#AUC = 1 thi do la mot thu nghiem tuyet hao

# set.seed(1237)
m = 50000     #iterations
PI = numeric(m)     #vector for results
PI[1] = 0.5     #initial value
alpha = 1; beta = 1     #Beta parameter
sens = 0.99; spec = 0.97     #sens and spec
N = 1579; A = 265; B=N-A     #actual data

for (i in 2:m)
{
num.x = PI[i-1]*sens;
den.x = num.x + (1-PI[i-1])*(1-spec)
X = rbinom(1, A, num.x/den.x)
num.y = PI[i-1]*(1-sens)
den.y = num.y + (1-PI[i-1])*spec
Y = rbinom(1, B, num.y/den.y)
PI[i] = rbeta(1, X + Y + alpha, n-X-Y+beta)
}
aft.brn = seq(m/2+1, m)
mean(PI[aft.brn])
#0.0006471388

quantile(PI[aft.brn], c(0.025, 0.975))
#        2.5%        97.5% 
#1.733797e-05 2.206625e-03 

par(mfrow=c(2,1))
#1.733797e-05 2.206625e-03 

plot(aft.brn, PI[aft.brn], type = "1")
hist(PI[aft.brn], prob=T)

par(mfrow=c(1,1))
#1.733797e-05 2.206625e-03 


#PAF POPULATION ATTRIBUTABLE FRACTION thuong dung de danh gia tam anh huong cua 
#mot yeu to nguy co. Mot phuong phap pho bien Levin, PAF tuy thuoc vao 2 yeu to
#ti le hien hanh (prevalence, ki hieu P) cua yeu to nguy co, va
#moi lien he giua yeu to nguy co voi nguy co mac benh, the hien = relative risk

library(survival)
library(epiR)

data = as.table(matrix(c(115, 1677, 18, 393), nrow = 2, byrow = T))
epi.2by2(data, method = "cohort.count", conf.level = 0.95)
#             Outcome +    Outcome -      Total        Inc risk *        Odds
#Exposed +          115         1677       1792              6.42      0.0686
#Exposed -           18          393        411              4.38      0.0458
#Total              133         2070       2203              6.04      0.0643

#Point estimates and 95% CIs:
#  -------------------------------------------------------------------
#  Inc risk ratio                               1.47 (0.90, 2.38)
#Odds ratio                                   1.50 (0.90, 2.49)
#Attrib risk *                                2.04 (-0.24, 4.32)
#Attrib risk in population *                  1.66 (-0.56, 3.87)
#Attrib fraction in exposed (%)               31.75 (-10.85, 57.99)
#Attrib fraction in population (%)            27.46 (-10.35, 52.31)
#-------------------------------------------------------------------
#  Test that OR = 1: chi2(1) = 2.447 Pr>chi2 = 0.12
#Wald confidence limits
#CI: confidence interval
#* Outcomes per 100 population units 

#PAF = 25.5%, khoan tin cay 95% cua PAF dao dong tu -10.4 den 52.3%

#MISSING VALUES
pbc = read.csv("file://D:/R-book-master/pbc.csv")
#source http://www-eio.upc.edu/~pau/cms/rdata/csv/survival/pbc.csv
install.packages("VIM")
library(colorspace)
library(grid)
library(VIM)

aggr(pbc,prop = T, numbers = T)
matrixplot(pbc, interactive = F)

#impute cac gia tri trong bang ham mice
install.packages("mice")
library(mice)
fpbc = mice(pbc, seed = 1234, printFlag = F)
full.pbc = complete(fpbc, action = 3)

summary(lm(chol~age + sex, data = pbc))
summary(lm(chol~age + sex, data = full.pbc))

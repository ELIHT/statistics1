#thoi gian song sot o benh nhan nhiem HIV
#Mot nhom benh nhan n=23 nhiem HIV duoc theo doi mot thoi gian, du lieu duoi day
#phan anh so ngay song sot.
#Ap dung pp Kaplan-Meier de uoc tinh xac suat song cua cac benh nhan theo tgian
id = 1:23
time = c(6, 12, 21, 27, 32, 39, 43, 43, 46, 89, 115, 139, 181, 211, 217,  261, 263,
         270, 295, 311, 335, 346, 365)
status = c(1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,1,1,1,0,1,0,0,0)
dat = data.frame(id, time, status)
dat
library(survival)
survtime = Surv(time, status == 1)
kp = survfit(Surv(time, status == 1) ~ 1)
summary(kp)
plot(kp)
#xac suat song con
install.packages("ggfortify")
library(ggplot2)
library(ggfortify)
autoplot(kp, surv.colour = "blue", ylab="Probability", xlab="Time")

#xac suat tu vong
autoplot(kp, fun = "event", surv.colour = "blue", 
         ylab="Probability", xlab="Time")

#so doi tuong at risk tai moi thoi diem
install.packages("survminer")
library(ggpubr)
library(survminer)
ggsurvplot(kp, fun="event", data = dat, risk.table = T, col="blue")

#so sanh survival curves giua hai nhom
#Nhom 1 la nhom chung, nhom 2 la nhung benh nhan co benh man tinh. 
#tat ca benh nhan nhom 2 deu tu vong. nhom 1 co 3 benh nhan con song.
time1 = c(12.3, 5.4, 8.2, 12.2, 11.7, 10.0, 5.7, 9.8, 2.6, 11.0, 9.2, 12.1, 6.6,
          2.2, 1.8, 10.2, 10.7, 11.1, 5.3, 3.5, 9.2, 2.5, 8.7, 3.8, 3.0)
status1 = c(0,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1)
time2 = c(5.8, 2.9, 8.4, 8.3, 9.1, 4.2, 4.1, 1.8, 3.1, 11.4, 2.4, 1.4, 5.9, 1.6,
          2.8, 4.9, 3.5, 6.5, 9.9, 3.6, 5.2, 8.8, 7.8, 4.7, 3.9)
status2 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
time = c(time1, time2)
status = c(status1, status2)
group = c(rep(1,25), rep(2,25))
dat2 = data.frame(time, status, group)
#xac suat song con cho moi nhom va khoang tin cay 95%
head(dat2)
kp.by.group = survfit(Surv(time, status == 1) ~ group)
summary(kp.by.group)

#so sanh xac suat giua hai nhom
survdiff(Surv(time, status==1)~group)

#ggplot
ggsurvplot(kp.by.group, data = dat2, risk.table = T)

#them tri so P va khoang tin cay 95%
ggsurvplot(kp.by.group, data = dat2, risk.table = T, pval = T, conf.int = T)
#the hien bang xac suat "evemt"
ggsurvplot(kp.by.group, fun="event", data=dat2, risk.table = T, pval = T,
           conf.int = T)

#Mo hinh Cox
group3 = c(rep(1,21), rep(0,21))
time3 = c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35,1,1,2,2,3,4,
          4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
status3 = c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,1,1,0,1,
            1,0,1,1,1,0,1,1)
wbc = c(2.31, 4.06, 3.28, 4.43, 2.96, 2.88, 3.60, 2.32, 2.57, 3.20, 2.80, 2.70,
        2.60, 2.16, 2.05, 2.01, 1.78, 2.20, 2.53, 1.47, 1.45, 2.80, 5.00, 4.91, 
        4.48, 4.01, 4.36, 2.42, 3.49, 3.97, 3.52, 3.05, 2.32, 3.26, 3.49, 2.12,
        1.50, 3.06, 2.30, 2.95, 2.73, 1.97)
dat3 = data.frame(group3, time3, status3, wbc)
#uoc tinh xac suat song con
baseline = Surv(time3, status3 == 0)
km = survfit(baseline ~ 1)
ggsurvplot(km, data = dat3, risk.table = T, color = "blue")

#anh huong cua group3 và wbc qua mo hinh Cox:
cox = coxph(Surv(time3, status3 == 1)~group + wbc)
summary(cox)
#Voi cung chi so bach cau tuong tu nhau thi rui ro tu vong trong nhom dieu tri 
#chi bang 32% nhom chung (HR 0.32, khoang tin cay 95% 0.13 den 0.79; P=0.013)
#Ngoai ra, bat ke benh nhan thuoc nhom nao thi bach cau tang cang cao, rui ro
#tu vong cung tang cao. Moi don vi tang WBC lam gia tang rui ro tu vong gap 6.5
#lan (KTC95 la tu 3.1 den 13.9), va anh huong nay co y nghia thong ke.

#MO HINH COX DA BIEN
#primary biliary cirrhosis
#thuoc D-penicillamnine
#chi so lien quan den gan nhu bilirubin, albumin sgot, protime
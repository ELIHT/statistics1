#check normal distribution
weight = c(45.0, 38.0, 57.0, 49.5, 57.0, 42.0, 54.0, 54.0, 34.0, 45.0, 
           42.0, 49.0, 27.0, 57.0, 50.0, 61.0, 49.5, 48.0, 54.0, 49.0,
           48.5, 58.0, 52.0, 40.0, 49.0, 59.0, 50.0, 50.0, 47.0, 50.0,
           47.0, 45.0, 48.0, 49.0, 50.0, 53.0, 48.5, 53.0, 43.0, 49.0,
           53.0, 52.0, 49.0, 50.0, 50.0, 43.0, 60.0, 55.0, 57.0, 58.0,
           48.0, 52.0, 50.0, 45.0, 56.0, 53.0, 57.0, 47.0, 51.0, 60.0,
           30.0, 35.0, 50.0, 60.0, 42.0, 40.0, 62.0, 54.5, 38.0, 41.0,
           47.0, 38.5, 59.0, 69.0, 59.0, 44.0, 66.0, 57.0, 44.0, 50.0,
           42.0, 45.0, 44.0, 55.0, 59.0, 53.0, 42.0, 53.0, 47.0, 47.0,
           58.0, 55.0, 41.0, 49.5, 49.0, 38.0, 58.0, 58.0, 39.5, 42.8)
install.packages("psych")
library(psych)
describe(weight, skew = F)
hist(weight, col = "blue", border = "white", 
     main = "Distribution of weight", xlab="weight")

#use Shapiro-Wilk normality test
shapiro.test(weight)
#use qqnorm & qqline
qqnorm(weight)
qqline(weight, col = 2)

#compare by bootstrap
normal = c(10.2, 2.2, 0.0, 2.6, 0.0, 43.1, 45.8, 63.6, 1.8, 0.0, 3.7, 0.0)
hypertensive = c(92.8, 54.8, 51.6, 61.7, 250.8, 84.5, 34.7, 62.2, 11.0, 39.1)
hist(c(normal, hypertensive), breaks=20, col="blue", border="white", 
     main="", xlab="Na+")

install.packages("simpleboot")
library(simpleboot)
bmed = two.boot(hypertensive, normal, FUN=median, R=1000)
bmed$t0
library(boot)
boot.ci(bmed)

hist(bmed, breaks=20)

#compare by robust
install.packages("dplyr")
library(dplyr)
na = c(normal, hypertensive)
group1 = c(rep("Normal", 12), rep("Hypert", 10))
install.packages("WRS2")
library(WRS2)
yuen(na ~ group1)

#RISK RATIO, ODDS RATIO, HAZARD RATIO
#trong mot nghien cuu tren 700 phu nu Viet Nam, cac nha nghien cuu phat hien 148
#nguoi bi loang xuong. Mot nghien cuu khac o Uc cung o phu nu tren cung do tuoi
#(n=1287) va co 345 nguoi bi loang xuong. Hai ty le nay that su khac nhau?
dt = matrix(c(148, 700-148, 345, 1287-345), nrow=2, byrow=T)
prop.test(dt)
#prop 1    prop 2 
#0.2114286 0.2680653

install.packages("epiR")
library(epiR)

#prevalence ratio - PR
rownames(dt) = c("VN", "Australia")
colnames(dt) = c("Osteo", "Non-osteo")
epi.2by2(dt, method="cross.sectional")
#Output
#Prevalence ratio    0.79 (0.67, 0.93)


#incidence rates
#McQuinland va dong nghiep (BMJ 1998) bao cao mot nghien cuu ve chat luong cham
#soc benh nhan. nghien cuu gom 2 nhom: nhom A gom 20 benh nhan duoc quan ly chat
#che, nhom B gom 54 benh nhan duoc danh gia la duoc cham soc chua dung muc. Sau 
#1 thoi gian theo doi, nhom A co 5 benh nhan tu vong, nhom B co 26 tu vong.
dt2 = matrix(c(5,15,26,28), nrow=2, byrow=T)
rownames(dt2) = c("Group A", "Group B")
colnames(dt2) = c("Death", "Survivor")

epi.2by2(dt2, method="cohort.count")
#Output
#Inc risk ratio    0.52 (0.23, 1.16)

#randomized controlled clinical trial
#Tam soat ung thu vu duoc khuyen cao la mot cach de giam tu vong vi ung thu vu. 
#Mot nhom nghien cuu Thuy Dien kiem tra gia thuyet do bang cach chon mot mau 
#nghien cuu voi hon 120,000 phu nu va chia thanh 2 nhom: nhom 1 duoc tam soat 
#bang nhu anh thuong xuyen (screening), va nhom 2 khong can thiep (no screening)
#Sau mot thoi gian theo doi, ket qua tra ve tu vong la nhu sau:
#ty le tu vong trong nhom tam soat la 188/66,103 = 0.277%
#ty le tu vong trong nhom chung la 177/66,105.
dt3 = matrix(c(183, 66103-183, 177, 66105-177), nrow = 2, byrow = T)
rownames(dt3) = c("Screening", "No Screening")
colnames(dt3) = c("Death", "Survivor")
epi.2by2(dt3, method = "cohort.count")
#Output
#Inc risk ratio    1.03 (0.84, 1.27)

#So sanh ti suat phat sinh trong nghien cuu doan he
#Cong trinh nghien cuu WHI danh gia anh huong cua HRT (thay the hormones) o phu 
#nu sau man kinh. Ho thuc hien mot cong trinh RCT voi hai nhom phu nu: nhom duoc
#dieu tri bang HRT (estrogen va progestin) va nhom chung. Tinh trung binh moi 
#nhom duoc theo doi hon 5 nam, va ghi nhan so ca ung thu vu moi mac.
#HRT: 166/8,506 trong 5.18 nam
#chung: 124/8,102 trong 5.10 nam
install.packages("rateratio.test")
library(rateratio.test)
rateratio.test(c(166, 124), c(44061, 41320))
#Output
#Rate Ratio    1.255429606

#So sanh 2 ty suat
#Nam 2006, mot nhom nghien cuu My-Thai Lan tuyen 16,395 doi tuong tu cong dong
#(khong phai nhom co nguy co cao), tuoi tu 18 den 30, theo cac tieu chuan dinh
#san tu 2 tinh cua Thai Lan. Tat ca  nhung nguoi nay deu khong bi nhiem HIV luc 
#tham gia cong trinh nghien cuu; ho duoc chia thanh 2 nhom mot cach ngau nhien:
#8,197 nguoi duoc tiem 6 lieu vaccine RV44
#8,198 nguoi dung gia duoc (placebo)
#Sau 3 nam theo doi, ket qua nhu sau: 51 nguoi trong nhom vaccine nhiem HIV va
#74 nguoi trong nhom gia duoc nhiem HIV.
rateratio.test(c(51, 74), c(8197,8198))
#Output
#Rate Ratio    0.689273267

#Odds ratio
#Giri va dong nghiep (2004) thuc hien mot nghien cuu so khoi de danh gia moi
#lien quan giua phoi nhiem chat doc da cam (AO) va ung thu tien liet tuyen.
#Ho chon 47 cuu chien binh My tung tham chien o VN va mac benh ung thu tien liet
#tuyen, va 144 nguoi khong ung thu. Ho phong van de tim hieu bao nhieu nguoi 
#bi phoi nhiem AO, va ket qua nhu sau:
#                     Ung thu tien liet tuyen (n=47)   Nhom chung (n=142)      
#Phoi nhiem AO:                 11                          17
#Khong phoi nhiem AO:           29                          106
#Khong ro:                      7                           21
dt4 = matrix(c(11,17,36,127), nrow = 2, byrow = T)
rownames(dt4) = c("AO", "Not AO")
colnames(dt4) = c("Cancer", "Control")
epi.2by2(dt4, method = "case.control")
#output
#Odds ratio (W)   2.28 (0.98, 5.31)

#Odds ratio for matched case control
#Mot nghien cuu benh chung duoc thuc hien de tham dinh moi lien quan giua loang 
#xuong va gay co xuong dui. Nghien cuu co 51 ca gay co xuong dui, va duoc bat
#cap voi 51 nguoi khong bi gay xuong.
#Nhom benh                                Nhom chung
#                           Loang xuong           Khong loang xuong
#Loang xuong                  11                      28
#Khong loang xuong            2                       10
dt5 = matrix(c(11,28,2,10), nrow = 2, byrow = T)
install.packages("exact2x2")
library(exact2x2)
mcnemar.exact(dt5)
#output 
#odds ratio    14 
mcnemar.test(dt5)

#Bayes
#Mot nghien cuu co dien ve moi lien he giua bo sung calcium va vitamin D (CaD)
#va nhoi mau co tim (myocardial infarction - MI),chia benh nhan thanh 2 nhom:
#nhom chung (placebo) gom 8289 nguoi, va nhom bo sung CaD gom 8429 nguoi. Sau 5 
#nam theo doi, so nguoi bi MI trong nhom chung la 168 va nhom CaD la 209.
n1 = 8429; n2 = 8289
x1 = 209; x2 = 168

p1 = x1/n1; p2 = x2/n2
d = p1 - p2
v1 = p1*(1-p1)/n1
v2 = p2*(1-p2)/n2
s = sqrt(v1+v2)
z = abs(d/s)
u95 = d + 1.96*s
p.value = 1 - pnorm(z)
c(d, 195, u95)
c(z, p.value)

n = 50000
a1 = 1; b1 = 1; a2 = 1; b2 = 1
p11 = rbeta(n, x1+a1, n1-x1+b1)
p21 = rbeta(n, x2+a2, n2-x2+b2)
diff = p11 - p21
plot(density(diff))
quantile(diff, c(0.025, 0.50, 0.975))


rr = p11/p21
quantile(rr,c(0.025, 0.50, 0.975))

#Ki
dt6 = matrix(c(146, 267, 121,
               71, 166, 102,
               51, 71, 46,
               17, 12, 6), nrow = 4, byrow = T)
rownames(dt6) = c("Chewing", "Smoking", "Alcohol", "None")
colnames(dt6) = c("Tonge cancer", "Buccal mucosa", "Other cancers")
chisq.test(dt6)

dt7 = c(78, 71, 87, 86)
exp = sum(dt7)/4
xsq = (dt7-exp)^2/exp
chisq = sum(xsq)
pchisq(chisq, length(dt7)-1)

#ANALYSIS OF VARIANCE or ANOVA
A = c(8, 9, 11, 4, 7, 8, 5)
B = c(7, 17, 10, 14, 12, 24, 11, 22)
C = c(28, 21, 26, 11, 24, 19)
D = c(26, 16, 13, 12, 9, 10, 11, 17, 15)
hormone = c(A, B, C, D)
group2 = c(rep("A", 7), rep("B", 8), rep("C", 6), rep("D", 9))
dt8 = data.frame(hormone, group2)
boxplot(hormone ~ group2)
av = aov(hormone ~ group2)
summary(av)

TukeyHSD(av)
plot(TukeyHSD(av), ordered=T)

#analysis of covariance
height = c(168, 165, 166, 167, 167, 167, 170, 171, 172, 172, 173, 177, 178, 166,
           172, 175, 173, 174, 176, 176, 178, 179, 178, 180, 180, 185, 186, 186,
           189, 193)
hb = c(11.0, 14.5, 14.0, 13.5, 13.8, 14.1, 13.8, 14.7, 15.0, 15.1, 13.8, 13.1, 
       13.8, 16.0, 16.5, 18.9, 16.1, 16.3, 16.2, 17.0, 16.5, 16.4, 16.2, 16.3, 
       17.0, 17.1, 17.3, 15.8, 16.3, 15.9)
sex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2)
sex = as.factor(sex)
dt9 = data.frame(height, hb, sex)
pp = ggplot(data = dt9, aes(x = height, y = hb, col = sex))
pp + geom_point()
par(mfrow=c(1,2))
boxplot(height ~ sex, ylab = "Height", xlab = "Sex", col = c("red", "blue"))
boxplot(hb ~ sex, ylab = "Hb", xlab = "Sex", col = c("red", "blue"))

m = lm(hb ~ height + sex, data = dt9)
summary(m)

#repeated analysis of variance
#mixed effects model
y = c(5.9, 3.9, 3.9, 3.6, 5.3, 4.7, 3.5, 3.2, 4.6, 3.7, 3.3, 3.2, 6.2, 4.6, 4.3,
      3.9, 6.0, 5.4, 5.2, 4.8, 6.4, 4.7, 4.8, 4.3, 7.6, 4.1, 3.8, 4.1, 5.9, 3.1,
      3.6, 3.3, 7.5, 6.1, 5.4, 4.6, 6.2, 5.3, 4.9, 4.5, 6.9, 5.6, 5.9, 5.9, 5.6, 
      4.7, 4.6, 4.0, 5.1, 3.9, 2.9, 2.9, 5.7, 4.7, 4.3, 4.6, 5.0, 4.0, 3.5, 3.3,
      5.2, 4.2, 4.0, 3.8, 7.7, 6.2, 6.1, 5.7, 8.0, 5.8, 6.5, 6.0, 7.7, 5.0, 6.3, 
      6.2)
id = rep(1:19, each = 4)
time = rep(c(0,2,3,4), 19)
treatment = rep(1:2, c(9*4, 10*4))
glucose = data.frame(id, treatment, time, y)
ppp = ggplot(data = glucose, aes(x = time, y = y, group = id, 
                                 col = factor(treatment)))
ppp = ppp + geom_line() + facet_grid(. ~ treatment) + stat_smooth(aes(group=1))
ppp

install.packages("lme4")
library(lme4)
fit2 = lmer(y ~ 1 + treatment + time + time:treatment + (1+time|id), 
            data = glucose, REML=0)
summary(fit2)

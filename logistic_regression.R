#LOGISTIC REGRESSION

install.packages("DescTools")
library(DescTools)

bw = read.csv("file:///D:/R-book-master/birthwt.csv")
Desc(factor(bw$low))
factor(bw$low)

#estimate parameters
m = glm(low ~ smoke, family = binomial, data = bw)
summary(m)

#odds ratio
install.packages("epiDisplay")
library(epiDisplay)
logistic.display(m)

#use sjp.glm
install.packages("sjPlot")
install.packages("assertive")
library(sjPlot)
library(sjmisc)
library(dplyr)
library(psych)
library(testthat)
library(assertive, warn.conflicts = FALSE)
bw$smoke = as.factor(bw$smoke)
m = glm(low ~ smoke, family = binomial, data = bw)
sjp.glm(m, trns.ticks = F)
sjp.glm(m, vars = "smoke", type = "pred", show.ci = T, trns.ticks = F)

#
bw1 = read.csv("file:///D:/R-book-master/birthwt.csv", header = T)
bw1$mother.wt = bw1$lwt*0.453592
m = glm(low ~ mother.wt, family = binomial, data = bw1)
summary(m)

m = glm(low ~ I(mother.wt/5), family = binomial, data = bw1)
summary(m)

bw1$mother.wt = (bw1$lwt*0.453592)/5
m = glm(low ~ mother.wt, family = binomial, data = bw1)
sjp.glm(m, vars = "mother.wt", type = "pred", show.ci = T, trns.ticks = F)


#truong hop so ca mac benh kha thap (hay hiem) so voi dan so -> mo hinh hoi quy 
#Poisson. Mo hinh Poisson tim hieu moi lien he giua bien tien luong va bien phu 
#thuoc tuan theo luat phan bo Poisson.
age = c(19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5, 89.5)
population = c(192675, 146207, 121374, 111353, 83004, 55932, 29007, 7538)
cases = c(1, 16, 30, 71, 102, 130, 133, 40)

#use glm
pois = glm(cases ~ age + offset(log(population)), family = poisson)
summary(pois)

sjp.glm(pois, vars = c("age"), type = "pred", show.ci = T, trns.ticks = F)

#ty so nguy co cua benh vien cong tren benh vien tu
births = c(236, 739, 970, 2371, 309, 679, 26, 1272, 3246, 1904, 357, 1080, 1027,
           28, 2507, 138, 502, 1501, 2750, 192)
case = c(8, 16, 15, 23, 5, 13, 4, 19, 33, 19, 10, 16, 22, 2, 22, 2, 18, 21, 24, 
         9)
hospital = c(0,1,1,1,1,1,0,1,1,1,1,1,1,0,1,0,1,1,1,1)
model = glm(case ~ births + hospital, family = poisson)
summary(model)

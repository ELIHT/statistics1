#MULTIVARIATE MODEL _ MO HINH DA BIEN (mo hinh co nhieu bien phu thuoc)
#1 - phan tich phan dinh tuyen tinh - linear discrimination analysis LDA
#2 - phan tich thanh phan - principal component analysis PCA
#3 - phan tich cum - cluster analysis CA

#1
wine = read.csv("file:///D:/R-book-master/wine.csv")
names(wine) = c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium",
                "Phenols","Flavanoids","Nonflavanoids","Proanthocyanins",
                "Color","Hue","Dilution","Proline")
head(wine)
library(MASS)
wlda = lda(wine$Type ~ wine$Alcohol + wine$Malic + wine$Ash + wine$Alcalinity +
             wine$Magnesium + wine$Phenols + wine$Flavanoids + 
             wine$Nonflavanoids + wine$Proanthocyanins + wine$Color + wine$Hue +
             wine$Dilution + wine$Proline)
wlda
#Proportion of trace:
#LD1    LD2 
#0.6875 0.3125 
#hai latent variables chinh la LD1 va LD2. Hai bien nay giai thich 100% phuong 
#sai cua du lieu, voi LD1 giai thich gan 69% phuong sai

#tinh gia tri tien luong cho loai ruou
wpredict = predict(wlda)
wpredict

#Ve hinh phan dinh voi ham LD1
ldahist(data = wpredict$x[,1], g=wine$Type)

#Ve hinh phan dinh voi ham LD2
ldahist(data = wpredict$x[,2], g=wine$Type)

#kha nang phan dinh 3 loai ruou dua vao LD1, LD2 duoi day
plot(wpredict$x[,1], wpredict$x[,2])
text(wpredict$x[,1], wpredict$x[,2], wine$Type, cex=0.7, pos=4, col="red")

#So sanh ket qua tien luong voi thuc te
wlda
w.pred = predict(wlda, wine[, 2:14])
table(wine$Type, predict(wlda)$class)
#Ket qua tien luong nhat quan voi thuc te
#     1  2  3
#1 59  0  0
#2  0 71  0
#3  0  0 48

#CACH 2: dung ldfa de phan tich phan dinh tuyen tinh
#Chon bien tien luong
xvars = wine[,-1]

#chon bien outcome (loai ruou)
yvar = wine[,1]
yvar = as.factor(wine$Type)
wine$Type = as.factor(wine$Type)

library(plyr)
library(grDevices)
install.packages("rARPACK")
library(rARPACK)
install.packages("lfda")
library(ggplot2)
library(ggfortify)
install.packages("fable")
library(fable)
library(lfda)

m = lfda(xvars, yvar, r=4, metric="plain")
autoplot(m, data = wine, frame = T, frame.colour="Type")

#mo hinh phan tich thanh phan (principal component analysis)
install.packages("carData")
library(carData)
library(car)
s.wine = as.data.frame(scale(wine[2:14]))
wpca = prcomp(s.wine)
wpca
summary(wpca)
screeplot(wpca, type="lines")

#dung autoplot
wine$Type = as.factor(wine$Type)
library(ggplot2)
library(ggfortify)
autoplot(wpca, data = wine, colour="Type")

#CLUSTER
library(cluster)
#chuan hoa du lieu de tat ca cac bien co gia tri trung binh 0 va phuong sai 1
wstd = scale(wine[, -1])
#tinh khoang cach qua ham dist
d = dist(wstd, method = "euclidean")
#hierarchical cluster analysis
hc = hclust(d, method = "complete")
plot(hc, labels = FALSE, hang = -1)
rect.hclust(hc, k=3, border = 2:4)

#partitioning around medoids de tim so cum toi uu
wpam = pam(d, 3)
library(ggfortify)
autoplot(pam(wine[-1], 3), frame = TRUE, frame.type = 'norm')

#RANDOM FOREST
install.packages("caret")
library(lattice)
library(caret)
#chia du lieu train va test
wine$Type = as.factor(wine$Type)
index = createDataPartition(y=wine$Type, p=0.6, list=F)
train = wine[index, ]
test = wine[-index, ]
control = trainControl(method = "cv", number = 10)
#RF de huan luyen
library(randomForest)
fit.train = train(Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium + 
                    Phenols + Flavanoids + Nonflavanoids + Proanthocyanins + 
                    Color + Hue + Dilution + Proline, data = train, method="rf",
                  trControl=control)
fit.train
# mtry  Accuracy   Kappa    
#2    1.0000000  1.0000000
#7    0.9818182  0.9724957
#13    0.9543939  0.9310869

#kiem dinh mo hinh trong du lieu test
#tinh gia tri tien luong bang ham predict dua tren mo hinh trong train va
#dua ket qua vao du lieu test
pred = predict(fit.train, newdata = test)
confusionMatrix(test$Type, pred)
#Statistics by Class:

#Class: 1 Class: 2 Class: 3
#Sensitivity            0.9583   1.0000   0.9500
#Specificity            1.0000   0.9545   1.0000
#Pos Pred Value         1.0000   0.9286   1.0000
#Neg Pred Value         0.9787   1.0000   0.9804
#Prevalence             0.3429   0.3714   0.2857
#Detection Rate         0.3286   0.3714   0.2714
#Detection Prevalence   0.3286   0.4000   0.2714
#Balanced Accuracy      0.9792   0.9773   0.9750
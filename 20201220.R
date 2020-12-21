#ggplot2
#read table, check NA value
ob = read.csv("file:///D:/R-book-master/Obesity data.csv")
table(is.na(ob$OB))

#tagging
ob$OB[ob$bmi < 18.5] = "Underweight"
ob$OB[ob$bmi >= 18.5 & ob$bmi <= 24.9] = "Normal"
ob$OB[ob$bmi >= 25.0 & ob$bmi <= 29.9] = "Overweight"
ob$OB[ob$bmi >= 30] = "Obese"
ob$OB = factor(ob$OB, levels=c("Underweight", "Normal", "Overweight", "Obese"))

#ggplot
library(ggplot2)
p1 = ggplot(data = ob, aes(OB, fill = OB)) + geom_bar()
p1 = p1 + xlab("Obesity group") + ylab("Frequency")
p1 + theme(legend.position = "none")

#ggthemes and gridExtra
#Correlation between pcfat and weight
install.packages("ggthemes")
library("ggthemes")
install.packages("gridExtra")
library("gridExtra")
p2 = ggplot(data = ob, aes(x=weight, y=pcfat))
p2 = p2 + geom_point() + geom_smooth()
p2 = p2 + ggtitle("Correlation between pcfat and weight") +
  xlab("Weight") + ylab("Pcfat")
p2 = p2 + theme(plot.title = element_text(hjust=0.4))
p2
p2 = p2 + theme_economist()
p2

#Correlation between pcfat and weight by Gender
p3 = ggplot(data = ob, aes(x=weight, y=pcfat, fill=gender, col=gender))
p3 = p3 + geom_point() + geom_smooth()
p3 = p3 + ggtitle("Correlation between pcfat and weight by Gender") +
  xlab("Weight") + ylab("Pcfat")
p3 = p3 + theme(plot.title = element_text(hjust=0.4))
p3 + theme_economist()

#ggExtra
install.packages("ggExtra")
library(ggExtra)
p4 = ggplot(data = ob, aes(x=bmi, y=pcfat, fill=gender, col=gender))
p4 = p4 + geom_point() + geom_smooth()
ggMarginal(p4, type = "density", groupColour = T, groupFill = T)

ggMarginal(p4, type = "histogram", groupColour = T, groupFill = T)
ggMarginal(p4, type = "violin", groupColour = T, groupFill = T)
ggMarginal(p4, type = "boxplot", groupColour = T, groupFill = T)

#GGally
install.packages("GGally")
library(GGally)
dt = ob[, c("gender", "age", "bmi", "weight", "height", "pcfat")]
ggpairs(dt)
ggpairs(data = dt, mapping = aes(color = gender))
ggpairs(data = ob, mapping = aes(color = gender), columns = 
          c("age", "weight", "bmi", "pcfat"))
ggpairs(data=ob, mapping = aes(color = ob$OB), columns = 
          c("OB", "weight", "bmi", "pcfat"))
# Author : Ramya B
# IRS 2017 Data

library(ggplot2)
library(cowplot)
library(corrplot)
data <- read.csv("C:/Users/HP/Desktop/IRS17.csv")

#Data Preprocessing
str(data)
summary(data)
data$STATEFIPS <- as.factor(data$STATEFIPS)
data$ STATE<- as.factor(data$STATE)
data$zipcode <-as.factor(data$zipcode )
data$agi_stub <-as.factor(data$agi_stub)

# ------ Correlation Analysis-------------
sub1 <- data[,c(23,25,27,29,31,37,33,46)]
sub2 <- data[,c(23,25,27,29,31,37,33,46)]
f<-cor(sub1,sub2)
corrplot(f, method = "square")
print(f)
#------ Regression Analysis ----------------
# Using ggplot2 package
ggplot(data=data,aes(x=A02650,y=A06500))+
  geom_point()+
  xlab("Total Income Amount")+
  ylab("Income Tax Amount ")+
  geom_smooth(method='lm',se=TRUE)+
  labs(title="Relationship between Total Income and Income Tax Amount")

#Using Method 2
scatter.smooth(x=data$A02650,y=data$A06500,
               main='Relationship between Total Income and Income Tax Amount',
               lpars =list(col = "blue", lwd = 3, lty = 3))

cor(data$A02650,data$A06500)

#Performing Linear Regression Model
model.1 <- lm( A06500 ~ A02650, data=data)
print(model.1)
summary(model.1)

#Considering Confidence Interval is 95%.
# We know significance Level is 1-Confidance Level i.e. 1-0.95 = 0.05
# Hence Significance Level is 0.05

# We see through the summary function that p <2.2e-16 which is less than 0.05.
# Hence the relationship is significant

#--------------- Hypothesis Testing -----------------
# If the number of single returns are greater than number of joint returns.

t.test(data$mars1,data$MARS2, alternative="greater",var.equal=T)

#If the number of single returns is greater than number of joint returns p=0.0002432 which is less than the significance level 0.05.
#Also, CI is between 111.0788 and  Inf which indicates it doesn't contain the 0-mean value. 
#Therefore, we can reject the null hypothesis and support the claim. 
#So, our decision is that the alternate hypothesis is true, and Number of Single Returns is > Number of Joint Returns.

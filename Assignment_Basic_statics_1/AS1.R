PSW <- read.csv("D:/Data_science/Assignments/assignment_1.csv")
mean(PSW$Points)
mean(PSW$Score)
mean(PSW$weigh)

median(PSW$Points)
median(PSW$Score)
median(PSW$weigh)

install.packages("modeest")
install.packages("XML")
library(modeest)
library(XML)

#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(PSW$Points)
getmode(PSW$Score)
getmode(PSW$weigh)

#Measures of Dispersion
var(PSW$Points)
var(PSW$Score)
var(PSW$weigh)

sd(PSW$Points)
sd(PSW$Score)
sd(PSW$weigh)


range(PSW$Points)
range(PSW$Score)
range(PSW$weigh)


rangevalue <- function(x){max(x)-min(x)}
rangevalue(PSW$Points)
rangevalue(PSW$Score)
rangevalue(PSW$weigh)

Carspeed <- read.csv("D:/Data_science/Assignments/car_speed.csv")

#Measures of skewness
install.packages("moments")
library(moments)

#Measures of skewness
skewness(Carspeed$Speed)
kurtosis(Carspeed$Speed)
skewness(Carspeed$Dist)
kurtosis(Carspeed$Dist)

boxplot(Carspeed$Speed)
hist(Carspeed$Speed)
barplot(Carspeed$Speed)
dotchart(Carspeed$Speed)

boxplot(Carspeed$Dist)
hist(Carspeed$Dist)
barplot(Carspeed$Dist)
dotchart(Carspeed$Dist)

SPWT <- read.csv("D:/Data_science/Assignments/SPWT.csv")

#Measures of skewness
install.packages("moments")
library(moments)

#Measures of skewness
skewness(SPWT$SP)
kurtosis(SPWT$SP)
skewness(SPWT$WT)
kurtosis(SPWT$WT)


hist(SPWT$SP)

hist(SPWT$WT)
PMPG <- read.csv("D:/Data_science/Assignments/Cars.csv")
library(dplyr)
library(psych)
library(modeest)
library(XML)
library(prob)



mean=mean(PMPG$MPG) # calculation of mean #34.42
sigma=sd(PMPG$MPG)  # calculation of standard deviation #9.13

#pnorm=z=(X-mean)/sigma
#pnorm() gives z proportion below the mentioned value, so if needs to get proportion 
#above it needs to substract from 1

# Greater than==GT, LessThan==LT

prob_MPG_GT_38 = 1-pnorm(38,mean,sigma) #0.34
prob_MPG_LT_40 = pnorm(40,mean,sigma)#0.73
prob_MPG_GT_20_LT_50= pnorm(50,mean,sigma)-pnorm(20,mean,sigma) # 0.89





summary(PMPG)
P1mean<-mean(PMPG$MPG)
P1sd<-sd(PMPG$MPG)
P1median<-median(PMPG$MPG)
P1<- (PMPG$MPG > 38)
P1
mpg38=pnorm(P1,P1mean,P1sd)

mpg38

pnorm(PMPG$MPG > 38)

hist(PMPG$MPG, 
     main = "Histogram of MPG", 
     xlab = "MPG")
qqnorm(PMPG$MPG); qqline(PMPG$MPG)

PMPG <- read.csv("D:/Data_science/Assignments/wc-at.csv")
qqnorm(PMPG$AT); qqline(PMPG$AT)
p <- pt((260-270)/(90/sqrt(18)), 18)

pt(tscore,df)  
?pt

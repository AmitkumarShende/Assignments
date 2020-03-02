############################## problem  1
library(nortest)
library(readxl)
############ read the data ##################
cutlet<-read.csv(file.choose())    # cutlet.xlsx
colnames(cutlet)
View(cutlet)
attach(cutlet)


#Normality test
#Ho : data are normal (p value>0.05) Accept Null Hypothesis
#ha: data are not normal (p value<0.05)Accept Alternative Hypothesis

#############Normality test###############

shapiro.test(cutlet$Unit.A)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(cutlet$Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

qqnorm(cutlet$Unit.A)
qqline(cutlet$Unit.A)

qqnorm(cutlet$Unit.B)
qqline(cutlet$Unit.B)
#############Variance test###############

#Create Hypothesis for variances of Unit A and Unit B
#Ho= Variance of diameters of Unit A is equal to the variance of diameters of Unit B
#Ha= Variance of diameters of Unit A is not equal to the variance of diameters of Unit B

var.test(cutlet$Unit.A,cutlet$Unit.B)

#Since p-value = 0.3136 > 0.05, we Accept null Hypothesis, 
# So will Go with 2 sample t Test 

############2 sample T Test ##################
#Ho= No significant difference in Diameters of Unit A and Unit B
#Ha= Significant difference in Diameters of Unit A and Unit B

t.test(cutlet$Unit.A,cutlet$Unit.B,alternative = "two.sided",conf.level = 0.95, var.equal = FALSE)

# Since P value = 0.4723 > 0.05 we Accept null Hypothesis.  
# Mean Of UnitA is 7.019, and of Unit.B is 6.9642 
# so we can conclude that there is No significant difference in Diameters of Unit A and Unit B

############################## problem  2
#Busines objective: Is there any difference in the average Turn Around Time (TAT) 
# of reports of the laboratories on their preferred list
library(car)
Input <-read.csv(file.choose())    # TAT.csv
colnames(Input)
View(Input)

attach(Input)

# Inputs are 4 lab reports. So Input is Discrete in more than 2 categories.
# Output is continuous as we are trying to see the difference in average TAT.
# we proceed with ANOVA test

#Normality test
#Ho : data are normal (p value>0.05) Accept Null Hypothesis
#ha: data are not normal (p value<0.05)Accept Alternative Hypothesis

#############Normality test###############

shapiro.test(Laboratory.1) 
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution
shapiro.test(Laboratory.2)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution
shapiro.test(Laboratory.3) 
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution
shapiro.test(Laboratory.4)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution

qqline(Laboratory.1)
qqline(Laboratory.2)
qqline(Laboratory.3)
qqline(Laboratory.4)

#############Variance test###############
stacklab=stack(Input)
#Create Hypothesis for variances 
#Ho= Variances are equal 
#Ha= Variances are not equal 
leveneTest(values~ ind, data = stacklab)

#Since p-value = 0.05161 > 0.05, we Accept null Hypothesis 
# Variances are equal for all laboratory TAT data
# So we proceed with one way ANOVA test 
############# ONE WAY ANOVA TEST ###############
#Create Hypothesis for ANOVA TEST
#Ho= No difference in the average TAT of all the laboratories 
#Ha= Difference in the average TAT of allthe laboratories 

Anova_results <- aov(values~ind,data = stacklab)
summary(Anova_results)
# p-value = 2e-16 < 0.05 so reject null hypothesis 
# So TAT for all the laboratories is differ with respect to each other
############################## problem  3
# Business objective : To determine buyers ratio of male and Female is similar across region.
# Inputs are 4 discrete variables(east,west,north,south) 
# Output is also discrete. We are trying to find out if proportions of male and female are similar or not across the regions.
# We proceed with chi-square test
# 
# So create hpothesis for normal distribution
# Ho: data is normal. Accept null Hypothesis
# Ha: data is not normal. Accept alternative Hypothesis
# We have to test for all the 4 input variables.

BuyersRatio <-read.csv(file.choose())    # BuyerRatio.csv
colnames(BuyersRatio)
View(BuyersRatio)
attach(BuyersRatio)
#############Normality test###############
Boxplot(East)
Boxplot(West)
Boxplot(North)
Boxplot(South)
# Data is normal. Accept null Hypothesis
# We proceed with chi-square test Hypothesis
# Ho= Buyer Ratios of Male and Female are same
# Ha= Buyer Ratios of Male and Female are not same
############# Chisq. Test ###############
DT=as.table(as.matrix(BuyersRatio[,2:5]))
DT
chisq <- chisq.test(DT)
chisq
# p-value = 0.6603 > 0.05 so Accept null hypothesis 
# So Buyers ratio across all region is same for male and Female
############################## problem  4
# Business objective : To determine whether the data is defective or not 
# Here both variables are discrete and have more than 2 variables hence we choose chi square test.
# here we have to test
# Ho : defective % are same at all the centers
# Ha: defective % varies  from center to center
Customerorderform <-read.csv(file.choose())    # Customerorderform.csv
colnames(Customerorderform)
View(Customerorderform)
COF<-ifelse(Customerorderform=="Error Free",0,1)
View(COF)
COF1=table(COF)
chisq.test(COF1)
# p-value = 2.2e-16 < 0.05 so Reject null hypothesis 
# So Data is Defective, and varies with the centers
############################## problem  5
#Business objective: Proportion of male and female customer visiting the store is differ on the day of the week
#Inputs are 2 discrete variables.
#Output is Discrete as we are trying to find out if proportions of male and female walking in to the store is same or not
#Here both the data are discrete . So we go for two proportion t test

# Ho: The Proportion of Male Vs Female are equal during weekdays and Weekends
# Ha: The Proportion of Male Vs Female are not equal during weekdays and Weekends

fantaloons<-read.csv(file.choose())
View(fantaloons)
attach(fantaloons)
tab<-table(Weekdays, Weekend)

############# Proportion Test ###############

prop.test(table(Weekdays, Weekend),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

# Here p-value 0.9681 is greater than alpha value so we accept null hypothesis 
# So The Proportion of Male Vs Female are equal during weekdays and Weekends

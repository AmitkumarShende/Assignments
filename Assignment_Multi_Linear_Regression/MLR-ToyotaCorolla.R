#Consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

Corolla <- read.csv(file.choose())
View(Corolla)

Corolla1<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
Corolla_Pred1 <- as.data.frame(Corolla1)
class(Corolla_Pred1)
View(Corolla_Pred1)
attach(Corolla_Pred1)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(Corolla_Pred1)

plot(Age_08_04, Price)

plot(KM, Price)

plot(HP, Price)

plot(cc, Price)

plot(Doors, Price)

plot(Gears, Price)

plot(Quarterly_Tax, Price)

plot(Weight, Price)

windows()
# 7. Find the correlation between Output (Price) & inputs (Age_08_04,KM,HP,CC,Doors,Gears,Quarterly_Tax,Weight) - SCATTER DIAGRAM
pairs(Corolla_Pred1)

# 8. Correlation coefficient - Strength & Direction of correlation
cor(Corolla_Pred1)

# The Linear Model of interest
corolla.price <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(corolla.price)

corolla.price2 <-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(corolla.price2)

### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla_Pred1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

### Partial Correlation matrix - Pure correlation between the variables
# install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla_Pred1))

install.packages("mvinfluence")
library(mvinfluence)
library(car)

# It is better to delete a single observation rather than entire variable to get rid of collinearity problem
# Deletion Diagnostics for identifying influential variable
influence.measures(corolla.price)

influenceIndexPlot(corolla.price, id.n=3) # Index Plots of the influence measures
influencePlot(corolla.price, id.n=3)
                
# infIndexPlot.mlm()
                
## Regression after deleting the 81st observation, which is influential observation
corolla.price1 <- lm(Price~Age_08_04+KM+HP+cc+Doors
                     +Gears+Quarterly_Tax+Weight,data=Corolla_Pred1[-81,])
summary(corolla.price1)

### Variance Inflation Factors is a formal way to check for collinearity
vif(corolla.price1)  # VIF is > 10 => collinearity

plot(Age_08_04,KM, col="dodgerblue4",pch=20)
plot(HP,cc, col="dodgerblue4",pch=20)

layout(matrix(c(1,2,3,4),2,2))
plot(corolla.price1)

avPlots(corolla.price1, id.n=2, id.cex=0.7) # Added Variable Plots

# VIF & avPlots has given us an indication to delete 'WT' variable
finalmodel <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(finalmodel)

Price_Predict <- predict(corolla.price,interval="predict")

Pred_final <- predict(corolla.price)
Final <- cbind(Price,Pred_final,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
View(Final)


# Evaluate model LINE assumptions
plot(finalmodel)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

qqPlot(finalmodel, id.n=5) # QQ plots of studentized residuals, helps identify outliers

library("MASS")
stepAIC(finalmodel) # backward

# Lower the AIC value better is the model. AIC is used only if you build multiple models.
setwd("D:/Data_science/Assignments/Assignment_Simple_linear_regression")

# Build a prediction model for Salary_hike


ye.sh <- read.csv(file.choose()) # choose the Salary_Data data set
View(ye.sh)

# 30 Observations of 2 variables

# Scatter Diagram (Plot x,y)
plot(ye.sh$YearsExperience,ye.sh$Salary)

# Other Exploratory data analysis and Plots

boxplot(ye.sh)

hist(ye.sh$YearsExperience)

hist(ye.sh$Salary)

summary(ye.sh)

# Correlation coefficient value for Years of Experience and Employee Salary Hike
ye<- ye.sh$YearsExperience
sh <- ye.sh$Salary
cor(ye,sh)

# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.9782416). 
# This has a strong Positive Correlation 

# Simple model without using any transformation
reg<-lm(sh~ye)
summary(reg)

# Probability value should be less than 0.05(5.51e-12)
# The multiple-R-Squared Value is 0.957 which is greater than 0.8(In General)
# Adjusted R-Squared Value is 0.9554 
# The Probability Value for F-Statistic is 2.2e-16(Overall Probability Model is also less than 0.05)
confint(reg,level = 0.95) # confidence interval

# The above code will get you 2 equations 
# 1 to caliculate the lower range and other for upper range

# Function to Predict the above model 
predict(reg,interval="predict")

# predict(reg,type="prediction")
# Adjusted R-squared value for the above model is 0.9554 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(sh~log(ye))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.8539
# Adjusted R-squared:  0.8487 

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(sh)~ye) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))

# Multiple R-squared value - 0.932
# Adjusted R SQuare Value - 0.9295 
# Higher the R-sqaured value - Better chances of getting good model 
# for Salary hike and Years of Experience

# Quadratic model
ye.sh[,"ye_sq"] = ye*ye

# Quadratic model
quad_mod <- lm(sh~ye+I(ye^2),data=ye.sh)
summary(quad_mod)
confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9538 
#Multiple R -Squared Value = 0.957

# Quadratic model
qd_model <- lm(sh~ye+ye_sq,data=ye.sh)
summary(qd_model)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9538 
#Multiple R -Squared Value = 0.957

# Cubic model
poly_mod <- lm(sh~ye+I(ye^2)+I(ye^3),data=ye.sh)
summary(poly_mod) # 0.9636

confint(poly_mod,level=0.95)

predict(poly_mod,interval="predict")

# Adjusted R-Squared = 0.9594
#Multiple R -Squared Value = 0.9636

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.9554,0.8487,0.9295,0.9538,0.9594)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Cubic  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value

Final <- cbind(YearsofExp=ye.sh$YearsExperience,Sal_Hike = ye.sh$Salary,Pred_sal_hike=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-sh)^2))
rmse
plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution

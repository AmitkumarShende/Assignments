setwd("D:/Data_science/Assignments/Assignment_Simple_linear_regression")

# Build a prediction model for Salary_hike vs Churn_out_rate


sh.cod <- read.csv(file.choose()) # choose the Churn_out_rate_Data data set
View(sh.cod)

# 14 Observations of 2 variables

# Scatter Diagram (Plot x,y)
plot(sh.cod$Salary_hike,sh.cod$Churn_out_rate)

# Other Exploratory data analysis and Plots

boxplot(sh.cod)

hist(sh.cod$Salary_hike)

hist(sh.cod$Churn_out_rate)

summary(sh.cod)

# Correlation coefficient value for Salary_hike and Churn_out_rate 
sh<- sh.cod$Salary_hike
cod <- sh.cod$Churn_out_rate
cor(cod,sh)

# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong negative Correlation 

# Simple model without using any transformation
reg<-lm(cod~sh)
summary(reg)

# Probability value should be less than 0.05(1.96e-05)
# The multiple-R-Squared Value is 0.8312 which is greater than 0.8(In General)
# Adjusted R-Squared Value is 0.8101 
# The Probability Value for F-Statistic is 0.0002386(Overall Probability Model is also less than 0.05)
confint(reg,level = 0.95) # confidence interval

# The above code will get you 2 equations 
# 1 to caliculate the lower range and other for upper range

# Function to Predict the above model 
predict(reg,interval="predict")

# predict(reg,type="prediction")
# Adjusted R-squared value for the above model is 0.8101 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(cod~log(sh))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.8486
# Adjusted R-squared:  0.8297 

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(cod)~sh) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))

# Multiple R-squared value - 0.8735
# Adjusted R SQuare Value - 0.8577 
# Higher the R-sqaured value - Better chances of getting good model 
# for Delivery Time and Sorting Time

# Quadratic model
sh.cod[,"sh_sq"] = sh*sh

# Quadratic model
quad_mod <- lm(cod~sh+I(sh^2),data=sh.cod)
summary(quad_mod)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9662
#Multiple R -Squared Value = 0.9737

# Quadratic model
qd_model <- lm(cod~sh+sh_sq,data=sh.cod)
summary(qd_model)


confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9662 
#Multiple R -Squared Value = 0.9737

# Cubic model
poly_mod <- lm(cod~sh+I(sh^2)+I(sh^3),data=sh.cod)
summary(poly_mod) # 0.9893


confint(poly_mod,level=0.95)

predict(poly_mod,interval="predict")
# Adjusted R-Squared = 0.9893
#Multiple R -Squared Value =   0.984


model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8101,0.8297,0.8577,0.9662,0.984)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Exponential  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value


Final <- cbind(Salary_hike = sh.cod$Salary_hike,Churn_out_rate=sh.cod$Churn_out_rate,Predicted_Salary_hike=predicted_Value)
View(Final)

rmse<-sqrt(mean((predicted_Value-cod)^2))
rmse
plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution

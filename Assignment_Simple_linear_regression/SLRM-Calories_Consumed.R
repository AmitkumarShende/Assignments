setwd("D:/Data_science/Assignments/Assignment_Simple_linear_regression")

# Build a prediction model for Weight_Gain vs Calories_consum


wg.cc <- read.csv(file.choose()) # choose the Calories_consum_Data data set
View(wg.cc)

# 14 Observations of 2 variables

# Scatter Diagram (Plot x,y)
plot(wg.cc$Weight_Gain,wg.cc$Calories_consum)

# Other Exploratory data analysis and Plots

boxplot(wg.cc)

hist(wg.cc$Weight_Gain)

hist(wg.cc$Calories_consum)

summary(wg.cc)

# Correlation coefficient value for Weight_Gain and Calories_consum 
wg<- wg.cc$Weight_Gained
cc <- wg.cc$Calories_Consumed
cor(wg,cc)

# If |r| is smaller than  0.946991 then Co-relation is strong(Correlation Co-efficient = 0.8259). 
# This has a strong Correlation 

# Simple model without using any transformation
reg<-lm(wg~cc)
summary(reg)

# Probability value should be less than 0.05(4.54e-05)
# The multiple-R-Squared Value is  0.8968 which is more than 0.8(In General)
# Adjusted R-Squared Value is 0.8882
# The Probability Value for F-Statistic is 2.856e-07(Overall Probability Model is also less than 0.05)
confint(reg,level = 0.95) # confidence interval

# The above code will get you 2 equations 
# 1 to caliculate the lower range and other for upper range

# Function to Predict the above model 
predict(reg,interval="predict")

# predict(reg,type="prediction")
# Adjusted R-squared value for the above model is 0.6655 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(wg~log(cc))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.8077
# Adjusted R-squared:   0.7917  

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(wg)~cc) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))

# Multiple R-squared value - 0.8776
# Adjusted R SQuare Value - 0.8674
# Higher the R-sqaured value - Better chances of getting good model 
# for Calories_consum hike and Years of Experience

# Quadratic model
wg.cc[,"cc_sq"] = cc*cc

# Quadratic model
quad_mod <- lm(wg~cc+I(cc^2),data=wg.cc)
summary(quad_mod)
confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared =  0.9521
#Multiple R -Squared Value =  0.9433

# Quadratic model
qd_model <- lm(wg~cc+cc_sq,data=wg.cc)
summary(qd_model)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.9521 
#Multiple R -Squared Value =  0.9433

# Cubic model
poly_mod <- lm(wg~cc+I(cc^2)+I(cc^3),data=wg.cc)
summary(poly_mod) 


confint(poly_mod,level=0.95)

predict(poly_mod,interval="predict")
# Adjusted R-Squared = 0.9811
#Multiple R -Squared Value =   0.9755 


model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8968,0.7917,0.8674,0.9433,0.9755)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Exponential  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value


Final <- cbind(Calories_consum=wg.cc$Calories_Consumed ,Weight_Gain = wg.cc$Weight_Gained,Predicted_Weight_Gain=predicted_Value)
View(Final)

rmse<-sqrt(mean((predicted_Value-cc)^2))
rmse
plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution

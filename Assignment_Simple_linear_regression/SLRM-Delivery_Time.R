setwd("D:/Data_science/Assignments/Assignment_Simple_linear_regression")

# Build a prediction model for Delivery_Time vs Sorting_Time


dt.st <- read.csv(file.choose()) # choose the Sorting_Time_Data data set
View(dt.st)

# 21 Observations of 2 variables

# Scatter Diagram (Plot x,y)
plot(dt.st$Delivery_Time,dt.st$Sorting_Time)

# Other Exploratory data analysis and Plots

boxplot(dt.st)

hist(dt.st$Delivery_Time)

hist(dt.st$Sorting_Time)

summary(dt.st)

# Correlation coefficient value for Delivery_Time and Sorting_Time 
dt<- dt.st$Delivery_Time
st <- dt.st$Sorting_Time
cor(dt,st)

# If |r| is smaller than  0.85 then Co-relation is moderate(Correlation Co-efficient = 0.8259). 
# This has a moderate Correlation 

# Simple model without using any transformation
reg<-lm(dt~st)
summary(reg)

# Probability value should be less than 0.05(0.00115)
# The multiple-R-Squared Value is  0.6823 which is less than 0.8(In General)
# Adjusted R-Squared Value is 0.6655
# The Probability Value for F-Statistic is 3.983e-06(Overall Probability Model is also less than 0.05)
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
reg_log<-lm(dt~log(st))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

# Multiple R-squared value for the above model is 0.6954
# Adjusted R-squared:  0.6794  

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(dt)~st) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))

# Multiple R-squared value - 0.7109
# Adjusted R SQuare Value - 0.6957 
# Higher the R-sqaured value - Better chances of getting good model 
# for Sorting_Time hike and Years of Experience

# Quadratic model
dt.st[,"st_sq"] = st*st

# Quadratic model
quad_mod <- lm(dt~st+I(st^2),data=dt.st)
summary(quad_mod)
confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.6934 
#Multiple R -Squared Value = 0.6594 

# Quadratic model
qd_model <- lm(dt~st+st_sq,data=dt.st)
summary(qd_model)

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.6934 
#Multiple R -Squared Value =  0.6594

# Cubic model
poly_mod <- lm(dt~st+I(st^2)+I(st^3),data=dt.st)
summary(poly_mod) # 0.9636

confint(poly_mod,level=0.95)

predict(poly_mod,interval="predict")

# Adjusted R-Squared = 0.7034
#Multiple R -Squared Value = 0.65110.9636

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.6655,0.6794,0.6957,0.6594,0.6511)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Exponential  model gives the best Adjusted R-Squared value
predicted_Value <- exp(predict(reg_exp))
predicted_Value


Final <- cbind(Sorting_Time=dt.st$Sorting.Time ,Delivery_Time = dt.st$Delivery.Time,Predicted_Delivery_time=predicted_Value)
View(Final)

rmse<-sqrt(mean((predicted_Value-st)^2))
rmse
plot(reg_exp)

hist(residuals(reg_exp)) # close to normal distribution

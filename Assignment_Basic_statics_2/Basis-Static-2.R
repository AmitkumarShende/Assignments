install.packages("shiny")
library(shiny)
library(car)
set1=read_xlsx(file.choose())
View(set1)
set1=as.data.frame(set1)
class(set1)
attach(set1)
mean(`Measure X`)
median(`Measure X`)
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(`Measure X`)

#Measures of skewness
install.packages("moments")
library(moments)

#Measures of skewness
skewness(`Measure X`)

#Measures of Kurtosis 
kurtosis(`Measure X`)

#Measures of Dispersion
var(X)
sd(X)
range(`Measure X`)
rangevalue <- function(x){max(x)-min(x)}
rangevalue(`Measure X`)

boxplot(`Measure X`,horizontal = TRUE )

bx$out

bx = boxplot(`Measure X`,horizontal = TRUE)$out
bx
hist(mba$gmat)
barplot(mba$gmat)
dotchart(mba$gmat)

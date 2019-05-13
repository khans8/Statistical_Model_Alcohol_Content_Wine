### Calling packages from library. 
library('psych') 
library('moments')
library('Hmisc') 
library('ggplot2') 
library('plyr')

### Open .csv File
wine <- read.csv("~/Desktop/winequality-white.csv", sep=";")

wine$quality <- NULL #getting rid of quality.

### Dimenstions of .csv file.
dim(wine) 

### Column names
names(wine)

### Vectors
fixedAcidity <-wine$fixed.acidity
volatileAcid <- wine$volatile.acidity
citricAcid <- wine$citric.acid
residualSugar <- wine$residual.sugar
chlorides <- wine$chlorides
freeSulferDioxide <- wine$free.sulfur.dioxide
sulfurDioxide <- wine$total.sulfur.dioxide
density <- wine$density
ph <- wine$pH
sulphates <- wine$sulphates
alcohol <- wine$alcohol



### Histograms
par(mfcol=c(2,2))
hist(alcohol, xlab = "Fixed Acidity", main = "Fig 1. Hist. of Alcohol")
hist(fixedAcidity, xlab = "Fixed Acidity", main = "Fig 2. Hist. of Fixed Acidity")
hist(volatileAcid, xlab = "Volatile Acid", main = "Fig 3. Hist. of Volatile Acidity")
hist(citricAcid, xlab = "Citric Acid", main = "Fig 4. Hist. of Citric Acid")
hist(residualSugar, xlab = "Residual Sugar", main = "Fig 5. Hist. of Residual Sugar")
hist(chlorides, xlab = "Chlorides", main = "Fig 6. Hist. of Chlorides")
hist(freeSulferDioxide, xlab = "Free Sulfer Dioxide", main = "Fig 7. Hist. of Free Sulfer Dioxide")
hist(sulfurDioxide, xlab = "Sulfur Dioxide", main = "Fig 8. Hist. of Total Sulfur Dioxide")
hist(density, xlab= "Density", main="Fig 9. Hist. of Density")
hist(ph, xlab = "pH", main = "Fig 10. Hist. of pH")
hist(sulphates, xlab = "Sulphates", main = "Fig 11. Hist. of Sulphates")

### Scatterplots of each independant variable, with dependant variable.
plot(fixedAcidity,alcohol,xlab="Fixed Acidity",ylab="Alcohol", main="Fig.12 Plot Fixed Acidity v Alcohol")
plot(volatileAcid,alcohol,xlab="Volatile Acid",ylab="Alcohol", main="Fig.13 Plot Volatile Acid v Alcohol")
plot(citricAcid,alcohol,xlab="Citric Acid",ylab="Alcohol", main="Fig.14 Plot Citric Acid v Alcohol")
plot(residualSugar,alcohol,xlab="Residual Sugar",ylab="Alcohol", main="Fig.15 Plot Residual Sugar v Alcohol")
plot(chlorides,alcohol,xlab="Chlorides",ylab="Alcohol", main="Fig.16 Chlorides v Alcohol")
plot(freeSulferDioxide,alcohol,xlab="Free Sulfur Dioxide",ylab="Alcohol", main="Fig.17 Plot Free Sulfur Dioxide v Alcohol")
plot(sulfurDioxide,alcohol,xlab="Sulfur Dioxide",ylab="Alcohol", main="Fig.18 Plot Total Sulfur Dioxide v Alcohol")
plot(density,alcohol,xlab="Density",ylab="Alcohol", main="Fig.19 Plot Density v Alcohol")
plot(ph,alcohol,xlab="pH",ylab="Alcohol", main="Fig.20 Plot pH v Alcohol")
plot(sulphates, alcohol, xlab="Sulphates",ylab="Alcohol", main="Fig.21 Plot Sulphates v Alcohol")

### Descriptive Statistics
summary(wine)
library(moments)
skewness(wine)
kurtosis(wine)
sd(alcohol)
sd(fixedAcidity)
sd(volatileAcid)
sd(citricAcid)
sd(residualSugar)
sd(chlorides)
sd(freeSulferDioxide)
sd(sulfurDioxide)
sd(density)
sd(ph)
sd(sulphates)

### Correlation Analysis
cor(alcohol,fixedAcidity)
cor(alcohol, volatileAcid)
cor(alcohol,citricAcid)
cor(alcohol,residualSugar)
cor(alcohol,chlorides)
cor(alcohol,freeSulferDioxide)
cor(alcohol,sulfurDioxide)
cor(alcohol,density)
cor(alcohol,ph)
cor(alcohol,sulphates)

### Correlation Matrix
cor(wine)

### Linear regression
dim(wine)
fit <- lm(alcohol~citricAcid+residualSugar+chlorides+freeSulferDioxide+sulfurDioxide+density+ph+sulphates)
fit

names(fit)
summary(fit) #R squared found
library(lmtest)
dwtest(fit)
fit$coefficients
fit$fitted.values
fit$residuals

### Predicted value
predict(fit, wine)

max(dfWine$resid)

dfWine<-data.frame(wine,pred=fit$fitted.values,resid=fit$residuals)
par(mfcol=c(2,2))

### Histogram of Residual Values
myhist <- hist(dfWine$resid, breaks = b, xlab = "Residual Values", main = "Fig 22. Hist. of Residual Values")

### Actual Vs. Predicted
plot(dfWine$pred, dfWine$alcohol, xlab = "Predicted Values", ylab = "Actual Values", main = "Fig 23. Actual v Predicted Values")
fit1<-lm(alcohol~pred,data=dfWine,na.action=na.omit)
abline(fit1, col = "red")
### Any violations of assumptions? All graphs should show no relationships.
par(mfcol=c(2,2))
plot(dfWine$fixed.acidity,dfWine$resid, xlab ="Fixed Acidity", ylab="Residuals", main = "Fig 24. Scatterplot of Fixed Acidity v Residuals"); abline(h=0, col = "red")
plot(dfWine$volatile.acidity,dfWine$resid, xlab ="Volatile Acidity", ylab="Residuals", main = "Fig 25. Scatterplot of Volatile Acidity v Residuals"); abline(h=0, col = "red")
plot(dfWine$citric.acid,dfWine$resid, xlab ="Citric Acid", ylab="Residuals", main = "Fig 26. Scatterplot of Citric Acid v Residuals"); abline(h=0, col = "red")
plot(dfWine$residual.sugar,dfWine$resid, xlab ="Residual Sugar", ylab="Residuals", main = "Fig 27. Scatterplot of Residual Sugar v Residuals"); abline(h=0, col = "red")
plot(dfWine$chlorides,dfWine$resid, xlab ="Chlorides", ylab="Residuals", main = "Fig 28. Scatterplot of Chlorides v Residuals"); abline(h=0, col = "red")
plot(dfWine$free.sulfur.dioxide,dfWine$resid, xlab ="Free Sulfur Dioxide", ylab="Residuals", main = "Fig 29. Scatterplot of Free Sulfur Dioxide v Residuals"); abline(h=0, col = "red")
plot(dfWine$total.sulfur.dioxide,dfWine$resid, xlab ="Total Sulfur Dioxide", ylab="Residuals", main = "Fig 30. Scatterplot of Total Sulfur Dioxide v Residuals"); abline(h=0, col = "red")
plot(dfWine$density,dfWine$resid, xlab ="Density", ylab="Residuals", main = "Fig 31. Scatterplot of Density v Residuals"); abline(h=0, col = "red")
plot(dfWine$pH,dfWine$resid, xlab ="pH", ylab="Residuals", main = "Fig 32. Scatterplot of pH v Residuals"); abline(h=0, col = "red")
plot(dfWine$sulphates, dfWine$resid, xlab = "Sulphates", ylab = "Residuals", main = "Fig 33. Scatterplot of Sulphates v Residuals"); abline(h=0, col = "red")

#GAM
names(wine)
library(mgcv)
gamfit <- gam(alcohol~s(fixedAcidity) + s(volatileAcid) + s(citricAcid) + s(residualSugar) + s(chlorides) + s(freeSulferDioxide) + s(sulfurDioxide) + s(density) + s(ph) + s(sulphates), data = wine)
plot.gam(gamfit,shade = "True")


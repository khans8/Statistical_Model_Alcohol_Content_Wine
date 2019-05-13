### Calling packages from library. 
library('psych') 
library('moments')
library('Hmisc') 
library('ggplot2') 
library('plyr')

### Open .csv File
wine <- read.csv("~/Desktop/Red Wine Quality.csv")

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
hist(fixedAcidity, xlab = "Fixed Acidity", main = "Fig 1. Hist. of Fixed Acidity")
hist(volatileAcid, xlab = "Volatile Acid", main = "Fig 2. Hist. of Volatile Acid")
hist(citricAcid, xlab = "Citric Acid", main = "Fig 3. Hist. of Citric Acid")
hist(residualSugar, xlab = "Residual Sugar", main = "Fig 4. Hist. of Residual Sugar")
hist(chlorides, xlab = "Chlorides", main = "Fig 5. Hist. of Chlorides")
hist(freeSulferDioxide, xlab = "Free Sulfer Dioxide", main = "Fig 6. Hist. of Free Sulfer Dioxide")
hist(sulfurDioxide, xlab = "Sulfur Dioxide", main = "Fig 7. Hist. of Total Sulfur Dioxide")
hist(density, xlab= "Density", main="Fig 8. Hist. of Density")
hist(ph, xlab = "pH", main = "Fig 9. Hist. of pH")
hist(sulphates, xlab = "Sulphates", main = "Fig 10. Hist. of Sulphates")
hist(alcohol, xlab = "Alcohol", main = "Fig 11. Hist. of Alcohol")


### Scatterplots of each independant variable, with dependant variable.
plot(fixedAcidity,alcohol,xlab="Fixed Acidity",ylab="Alcohol", main="Fig.1 Plot Fixed Acidity v Alcohol")
plot(volatileAcid,alcohol,xlab="Volatile Acid",ylab="Alcohol", main="Fig.2 Plot Volatile Acid v Alcohol")
plot(citricAcid,alcohol,xlab="Citric Acid",ylab="Alcohol", main="Fig.3 Plot Citric Acid v Alcohol")
plot(residualSugar,alcohol,xlab="Residual Sugar",ylab="Alcohol", main="Fig.4 Plot Residual Sugar v Alcohol")
plot(chlorides,alcohol,xlab="Chlorides",ylab="Alcohol", main="Fig.5 Chlorides v Alcohol")
plot(freeSulferDioxide,alcohol,xlab="Free Sulfur Dioxide",ylab="Alcohol", main="Fig.6 Plot Free Sulfur Dioxide v Alcohol")
plot(sulfurDioxide,alcohol,xlab="Sulfur Dioxide",ylab="Alcohol", main="Fig.7 Plot Total Sulfur Dioxide v Alcohol")
plot(density,alcohol,xlab="Density",ylab="Alcohol", main="Fig.8 Plot Density v Alcohol")
plot(ph,alcohol,xlab="pH",ylab="Alcohol", main="Fig.8 Plot pH v Alcohol")
plot(sulphates,quality,xlab="Sulphates",ylab="Alcohol", main="Fig.9 Plot Sulphates v Alcohol")

### Descriptive Statistics
describe(wine)

### Correlation Analysis
cor(alcohol,fixedAcidity)
cor(alcohol,citricAcid)
cor(alcohol,residualSugar)
cor(alcohol,chlorides)
cor(alcohol,freeSulferDioxide)
cor(alcohol,sulfurDioxide)
cor(alcohol,density)
cor(alcohol,ph)
cor(alcohol,sulphates)



### Correlation Matrix
correlation <- cor(wine[,c("citric.acid","residual.sugar","chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates" , "alcohol")],use="na.or.complete")
round(correlation,3) ### Rounding to 3 decimal places


### Linear regression
dim(wine)
fit <- lm(alcohol~citricAcid+residualSugar+chlorides+freeSulferDioxide+sulfurDioxide+density+ph+sulphates)
fit


names(fit)
summary(fit) #R squared found

fit$coefficients
fit$fitted.values
fit$residuals

### Predicted value
predict(fit, wine)


### Residual Analysis
dfWine<-data.frame(wine,pred=fit$fitted.values,resid=fit$residuals)
par(mfcol=c(2,2))
hist(dfWine$resid, xlab = "Residual Values", main = "Hist. of Residual Values")
plot(dfWine$pred,dfWine$alcohol, xlab= "Predicted Values", ylab="Actual Values", main="Actual v Predicted Values")
fit1 <-lm(alcohol~pred,data=dfWine,na.action=na.omit)
abline(fit1, col = "red")


### Any violations of assumptions? All graphs should show no relationships.
par(mfcol=c(2,2))
plot(dfWine$fixed.acidity,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$volatile.acidity,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$citric.acid,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$residual.sugar,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$chlorides,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$free.sulfur.dioxide,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$total.sulfur.dioxide,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$density,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$pH,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$sulfates,dfWine$resid); abline(h=0, col = "red")
plot(dfWine$alcohol,dfWine$resid); abline(h=0, col = "red")


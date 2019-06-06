#Brent Young
#PREDICT 411
#Wine Sales Poisson Regression Project

#Part 0: Load & Prepare Data

library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(rJava)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(mice)
library(Hmisc)
library(xlsxjars)
library(xlsx)
library(VIM)
library(pROC)
library(pscl) # For "counting" models (e.g., Poisson and Negative Binomial)
library(ggplot2) # For graphical tools
library(readr)
library(corrplot)


setwd("~/R/Wine")

wine=read.csv("Wine_Training.csv",header=T)

#Part 1: Data Exploration

#Data Quality Check
str(wine)
summary(wine)

library(Hmisc)
describe(wine)

#TARGET
par(mfrow=c(1,2))
hist(wine$TARGET, col = "#A71930", xlab = "TARGET ", main = "Histogram of Wine Sales Purchased")
boxplot(wine$TARGET, col = "#A71930", main = "Boxplot of Wine Sales Purchased ")
par(mfrow = c(1,1))
#Chemistry 
# FixedAcidity and VolatileAcidity
par(mfrow=c(2,2))
hist(wine$FixedAcidity, col = "#A71930", xlab = "FixedAcidity", main = "Histogram of FixedAcidity")
hist(wine$VolatileAcidity, col = "#09ADAD", xlab = "VolatileAcidity", main = "Histogram of VolatileAcidity")
boxplot(wine$FixedAcidity, col = "#A71930", main = "Boxplot of FixedAcidity")
boxplot(wine$VolatileAcidity, col = "#09ADAD", main = "Boxplot of VolatileAcidity")
par(mfrow=c(1,1))

# CitricAcid and ResidualSugar
par(mfrow=c(2,2))
hist(wine$CitricAcid, col = "#A71930", xlab = "CitricAcid", main = "Histogram of CitricAcid")
hist(wine$ResidualSugar, col = "#DBCEAC", xlab = "ResidualSugar ", main = "Histogram of ResidualSugar")
boxplot(wine$CitricAcid, col = "#A71930", main = "Boxplot of CitricAcid")
boxplot(wine$ResidualSugar, col = "#DBCEAC", main = "Boxplot of ResidualSugar")
par(mfrow=c(1,1))

#Chlorides and FreeSulfur Dioxide
par(mfrow=c(2,2))
hist(wine$Chlorides, col = "#A71930", xlab = "Chlorides", main = "Histogram of Chlorides")
hist(wine$FreeSulfurDioxide, col = "#DBCEAC", xlab = "FreeSulfurDioxide ", main = "Histogram of FreeSulfurDioxide")
boxplot(wine$Chlorides, col = "#A71930", main = "Boxplot of Chlorides")
boxplot(wine$FreeSulfurDioxide, col = "#DBCEAC", main = "Boxplot of FreeSulfurDioxide")
par(mfrow=c(1,1))

#TotalSulfurDioxide and Density
par(mfrow=c(2,2))
hist(wine$TotalSulfurDioxide, col = "#A71930", xlab = "TotalSulfurDioxide", main = "Histogram of TotalSulfurDioxide")
hist(wine$Density, col = "#DBCEAC", xlab = "Density", main = "Histogram of Density")
boxplot(wine$TotalSulfurDioxide, col = "#A71930", main = "Boxplot of TotalSulfurDioxide")
boxplot(wine$Density, col = "#DBCEAC", main = "Boxplot of Density")
par(mfrow=c(1,1))

#pH and Sulphates
par(mfrow=c(2,2))
hist(wine$pH, col = "#A71930", xlab = "pH", main = "Histogram of pH")
hist(wine$Sulphates, col = "#09ADAD", xlab = "Sulphates", main = "Histograms of Sulphates")
boxplot(wine$pH, col = "#A71930", main = "Boxplot of pH")
boxplot(wine$Sulphates, col = "#09ADAD", main = "Boxplot of Sulphates")
par(mfrow=c(1,1))

#Alcohol and Acid Index
par(mfrow=c(2,2))
hist(wine$Alcohol, col = "#A71930", xlab = "Alcohol", main = "Histogram of Alcohol")
hist(wine$AcidIndex, col = "#DBCEAC", xlab = "AcidIndex", main = "Histograms of AcidIndex")
boxplot(wine$Alcohol, col = "#A71930", main = "Boxplot of Alcohol")
boxplot(wine$AcidIndex, col = "#DBCEAC", main = "Boxplot of AcidIndex")
par(mfrow=c(1,1))

#Label Appeal and STARS
par(mfrow=c(2,2))
hist(wine$LabelAppeal, col = "#A71930", xlab = "LabelAppeal", main = "Histogram of LabelAppeal ")
hist(wine$STARS, col = "#09ADAD", xlab = "STARS", main = "Histogram of STARS")
boxplot(wine$LabelAppeal, col = "#A71930", main = "Boxplot of LabelAppeal")
boxplot(wine$STARS, col = "#09ADAD", main = "Boxplot of STARS")
par(mfrow=c(1,1))

# Scatterplot Matrix 

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~ wine$TARGET + wine$FixedAcidity+ wine$VolatileAcidity+ wine$CitricAcid+ wine$ResidualSugar+ wine$Chlorides+ wine$FreeSulfurDioxide+ wine$TotalSulfurDioxide, lower.panel = panel.smooth)
par(mfrow=c(1,1))

pairs(~ wine$TARGET + wine$Density+ wine$pH+ wine$Sulphates+ wine$Alcohol + wine$LabelAppeal+ wine$AcidIndex + wine$STARS, lower.panel = panel.smooth)
par(mfrow=c(1,1))

#Correlation Matrix
subdatnum <- subset(wine, select=c(
  "FixedAcidity",
  "VolatileAcidity",
  "CitricAcid",
  "ResidualSugar",
  "Chlorides",
  "FreeSulfurDioxide",
  "TotalSulfurDioxide",
  "Density",
  "pH",
  "Sulphates",
  "Alcohol",
  "LabelAppeal",
  "AcidIndex",
  "STARS",
  "TARGET"))

require(corrplot)
mcor <- cor(subdatnum)
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)

#Part 2: Data Preparation
library(mice)

#Check for missing values
sapply(wine, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(wine,2,pMiss)

#Create Flag Variables (Missing Data)

wine$NoResidualSugar <- 0
wine$NoResidualSugar [is.na(wine$ResidualSugar)] <- 1

wine$NoChlorides  <- 0
wine$NoChlorides [is.na(wine$Chlorides)] <- 1

wine$NoFreeSulfurDioxide <- 0
wine$NoFreeSulfurDioxide[is.na(wine$FreeSulfurDioxide)] <- 1

wine$NoTotalSulfurDioxide <- 0
wine$NoTotalSulfurDioxide[is.na(wine$TotalSulfurDioxide)] <- 1

wine$NopH <- 0
wine$NopH[is.na(wine$pH)] <- 1

wine$NoSulphates <- 0
wine$NoSulphates [is.na(wine$Sulphates)] <- 1

wine$NoResidualSugar <- 0
wine$NoResidualSugar [is.na(wine$ResidualSugar)] <- 1

wine$NoAlcohol <- 0
wine$NoAlcohol [is.na(wine$Alcohol)] <- 1

wine$NoSTARS<- 0
wine$NoSTARS [is.na(wine$STARS)] <- 1

str(wine)

#Run imputation
tempData <- mice(wine,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

#Check N/A values have been removed
wine2 <- complete(tempData,1)
apply(wine2,2,pMiss)
summary(wine2)

densityplot(tempData)

#Straighten Relationships - Create transformed variables that we can look at later
wine2$logFixedAcidity <- log(wine2$FixedAcidity)
wine2$logVolatileAcidity <- log(wine2$VolatileAcidity)
wine2$logCitricAcid <- log(wine2$CitricAcid)
wine2$logResidualSugar <- log(wine2$ResidualSugar)
wine2$logChlorides <- log(wine2$Chlorides)
wine2$logFreeSulfurDioxide <- log(wine2$FreeSulfurDioxide)
wine2$logTotalSulfurDioxide <- log(wine2$TotalSulfurDioxide)
wine2$logDensity <- log(wine2$Density)
wine2$logpH <- log(wine2$pH)
wine2$logSulphates <- log(wine2$Sulphates)
wine2$logAlcohol <- log(wine2$Alcohol)
wine2$logLabelAppeal <- log(wine2$LabelAppeal)
wine2$logAcidIndex <- log(wine2$AcidIndex)
wine2$logSTARS <- log(wine2$STARS)

#Create SQRT Transformations of Some of the Variables
wine2$SQRT_STARS <- sqrt(wine2$STARS)
wine2$SQRT_AcidIndex <- sqrt(wine2$AcidIndex)

#Trim Data
wine2$FixedAcidity [(wine2$FixedAcidity >= 20)] = 20
wine2$FixedAcidity [(wine2$FixedAcidity <= -5)] = -5

wine2$VolatileAcidity [(wine2$VolatileAcidity >= 2)] = 2
wine2$VolatileAcidity [(wine2$VolatileAcidity  <= -1.5)] = -1.5

wine2$CitricAcid [(wine2$CitricAcid >= 2)] = 2
wine2$CitricAcid [(wine2$CitricAcid <= -1.5)] = -1.5

wine2$ResidualSugar [(wine2$ResidualSugar >= 65)] = 65
wine2$ResidualSugar [(wine2$ResidualSugar <= -65)] = -65

wine2$Chlorides [(wine2$Chlorides >= 0.7)] = 0.7
wine2$Chlorides [(wine2$Chlorides <= -0.6)] = -0.6

wine2$FreeSulfurDioxide [(wine2$FreeSulfurDioxide >= 350)] = 350
wine2$FreeSulfurDioxide [(wine2$FreeSulfurDioxide <= -275)] = -275

wine2$TotalSulfurDioxide [(wine2$TotalSulfurDioxide >= 725)] = 725
wine2$TotalSulfurDioxide [(wine2$TotalSulfurDioxide <= -400)] = -400

wine2$Density [(wine2$Density >= 1.06)] = 1.06
wine2$Density [(wine2$Density <= 0.93)] = 0.93

wine2$pH [(wine2$pH >= 4.75)] = 4.75
wine2$pH [(wine2$pH <= 1.5)] = 1.5

wine2$Sulphates [(wine2$Sulphates >= 2.5)] = 2.5
wine2$Sulphates [(wine2$Sulphates <= -1.5)] = -1.5

wine2$Alcohol [(wine2$Alcohol >= 20)] = 20
wine2$Alcohol [(wine2$Alcohol <= 3)] = 3

wine2$AcidIndex [(wine2$AcidIndex >= 11)] = 11
wine2$AcidIndex [(wine2$AcidIndex <= 5)] = 5

summary(wine2)

#Correlation Matrix
subdatnum2 <- subset(wine2, select=c(
  "FixedAcidity",
  "VolatileAcidity",
  "CitricAcid",
  "ResidualSugar",
  "Chlorides",
  "FreeSulfurDioxide",
  "TotalSulfurDioxide",
  "Density",
  "pH",
  "Sulphates",
  "Alcohol",
  "LabelAppeal",
  "AcidIndex",
  "STARS",
  "NoResidualSugar",
  "NoChlorides",
  "NoFreeSulfurDioxide",
  "NoTotalSulfurDioxide",
  "NopH",
  "NoSulphates",
  "NoSTARS",
  "NoAlcohol",
  "TARGET"))

require(corrplot)
mcor <- cor(subdatnum2)
corrplot(mcor, method="number", shade.col=NA, tl.col="black",tl.cex=0.8)
par(mfrow=c(1,1))  

#Part 3: Model Creation

MLRResult1<- lm(formula = TARGET ~ STARS + NoSTARS + LabelAppeal + AcidIndex + VolatileAcidity, data = wine2)

anova(MLRResult1)
summary(MLRResult1)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRResult1)
vif(MLRResult1)
coefficients(MLRResult1)

# Stepwise Approach

stepwisemodel <- lm(formula = TARGET ~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates + Alcohol + LabelAppeal + AcidIndex + STARS + NoResidualSugar + NoChlorides + NoFreeSulfurDioxide + NoTotalSulfurDioxide + NopH + NoSulphates + NoAlcohol + NoSTARS, data = wine2)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)
anova(stepwise)
summary(stepwise)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(stepwise)
vif(stepwise)
coefficients(stepwise)





#Model 3 (reduced Stepwise)

Model3 <- lm(formula = TARGET ~ VolatileAcidity + FreeSulfurDioxide + TotalSulfurDioxide + Chlorides + Alcohol + LabelAppeal + logAcidIndex + logSTARS + NoSTARS, data = wine2)

anova(Model3)
summary(Model3)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(Model3)
vif(Model3)
coefficients(Model3)

#Poisson Model 

poisson_model <- glm(TARGET ~ VolatileAcidity + FreeSulfurDioxide + TotalSulfurDioxide + Chlorides + LabelAppeal + logAcidIndex + logSTARS + NoSTARS, family="poisson"(link="log"), data=wine2)

anova(poisson_model, test="Chisq")
summary(poisson_model)
coef(poisson_model)

wine2$poisson_yhat <- predict(poisson_model, newdata = wine2, type = "response")

with(poisson_model, cbind(res.deviance = deviance, df = df.residual,
                          p = pchisq(deviance, df.residual, lower.tail=FALSE)))

library(AER)
deviance(poisson_model)/poisson_model$df.residual
dispersiontest(poisson_model)

#what type of dispersion does sample have?
mean(wine2$TARGET)
var(wine2$TARGET)

library(car)
influencePlot(poisson_model)
res <- residuals(poisson_model, type="deviance")
plot(log(predict(poisson_model)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

#Negative Binomial Distribution

NBR_Model<-glm.nb(TARGET ~ VolatileAcidity + LabelAppeal + SQRT_AcidIndex + logSTARS + NoSTARS, data=wine2)

summary(NBR_Model)

wine2$NBRphat <- predict(NBR_Model, newdata = wine2, type = "response")

odTest(NBR_Model)

#ZERO INFLATED POISSON (ZIP)

ZIP_Model<-zeroinfl(TARGET ~ VolatileAcidity + FreeSulfurDioxide + TotalSulfurDioxide + Chlorides + Alcohol + LabelAppeal + logAcidIndex + logSTARS + NoSTARS, data=wine2)

summary(ZIP_Model)

vuong(poisson_model, ZIP_Model)

wine2$ZIPphat <- predict(ZIP_Model, newdata = wine2, type = "response")

#ZERO INFLATED NEGATIVE BINOMIAL REGRESSION (ZINB)
ZINB_Model<-zeroinfl(TARGET ~ Alcohol + LabelAppeal + logAcidIndex + logSTARS + NoSTARS, data=wine2, dist = "negbin", EM=TRUE)

summary(ZINB_Model)

vuong(NBR_Model, ZINB_Model)

wine2$ZINBphat <- predict(ZINB_Model, newdata = wine2, type = "response")

#what type of dispersion does sample have?
mean(wine2$TARGET)
var(wine2$TARGET)

#Part 4: Performance 

#Function for Mean Square Error Calculation
mse <- function(sm) 
  mean(sm$residuals^2)

AIC(MLRResult1)
AIC(stepwisemodel)
AIC(Model3)
AIC(poisson_model)
AIC(NBR_Model)
AIC (ZIP_Model)
AIC(ZINB_Model)


BIC(MLRResult1)
BIC(stepwisemodel)
BIC(Model3)
BIC(poisson_model)
BIC(NBR_Model)
BIC(ZIP_Model)
BIC(ZINB_Model)


mse(MLRResult1)
mse(stepwisemodel)
mse(Model3)
mse(poisson_model)
mse(NBR_Model)
mse(ZIP_Model)
mse(ZINB_Model)

#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

#Part 5: Test Data 

wine_test=read.csv("wine_test.csv",header=T)

#Part 2: Data Preparation
library(mice)

#Check for missing values
sapply(wine_test, function(x) sum(is.na(x)))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(wine_test,2,pMiss)

#Create Flag Variables (Missing Data)

wine_test$NoResidualSugar <- 0
wine_test$NoResidualSugar [is.na(wine_test$ResidualSugar)] <- 1

wine_test$NoChlorides  <- 0
wine_test$NoChlorides [is.na(wine_test$Chlorides)] <- 1

wine_test$NoFreeSulfurDioxide <- 0
wine_test$NoFreeSulfurDioxide[is.na(wine_test$FreeSulfurDioxide)] <- 1

wine_test$NoTotalSulfurDioxide <- 0
wine_test$NoTotalSulfurDioxide[is.na(wine_test$TotalSulfurDioxide)] <- 1

wine_test$NopH <- 0
wine_test$NopH[is.na(wine_test$pH)] <- 1

wine_test$NoSulphates <- 0
wine_test$NoSulphates [is.na(wine_test$Sulphates)] <- 1

wine_test$NoResidualSugar <- 0
wine_test$NoResidualSugar [is.na(wine_test$ResidualSugar)] <- 1

wine_test$NoAlcohol <- 0
wine_test$NoAlcohol [is.na(wine_test$Alcohol)] <- 1

wine_test$NoSTARS<- 0
wine_test$NoSTARS [is.na(wine_test$STARS)] <- 1

str(wine_test)

#Run imputation
tempData <- mice(wine_test,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

#Check N/A values have been removed
wine3 <- complete(tempData,1)
apply(wine3,2,pMiss)
summary(wine3)

densityplot(tempData)

#Straighten Relationships - Create transformed variables that we can look at later
wine3$logFixedAcidity <- log(wine3$FixedAcidity)
wine3$logVolatileAcidity <- log(wine3$VolatileAcidity)
wine3$logCitricAcid <- log(wine3$CitricAcid)
wine3$logResidualSugar <- log(wine3$ResidualSugar)
wine3$logChlorides <- log(wine3$Chlorides)
wine3$logFreeSulfurDioxide <- log(wine3$FreeSulfurDioxide)
wine3$logTotalSulfurDioxide <- log(wine3$TotalSulfurDioxide)
wine3$logDensity <- log(wine3$Density)
wine3$logpH <- log(wine3$pH)
wine3$logSulphates <- log(wine3$Sulphates)
wine3$logAlcohol <- log(wine3$Alcohol)
wine3$logLabelAppeal <- log(wine3$LabelAppeal)
wine3$logAcidIndex <- log(wine3$AcidIndex)
wine3$logSTARS <- log(wine3$STARS)

#Create SQRT Transformations of Some of the Variables
wine3$SQRT_STARS <- sqrt(wine3$ STARS)
wine3$SQRT_AcidIndex <- sqrt(wine3$AcidIndex)

#Trim Data
wine3$FixedAcidity [(wine3$FixedAcidity >= 20)] = 20
wine3$FixedAcidity [(wine3$FixedAcidity <= -5)] = -5

wine3$VolatileAcidity [(wine3$VolatileAcidity >= 2)] = 2
wine3$VolatileAcidity [(wine3$VolatileAcidity  <= -1.5)] = -1.5

wine3$CitricAcid [(wine3$CitricAcid >= 2)] = 2
wine3$CitricAcid [(wine3$CitricAcid <= -1.5)] = -1.5

wine3$ResidualSugar [(wine3$ResidualSugar >= 65)] = 65
wine3$ResidualSugar [(wine3$ResidualSugar <= -65)] = -65

wine3$Chlorides [(wine3$Chlorides >= 0.7)] = 0.7
wine3$Chlorides [(wine3$Chlorides <= -0.6)] = -0.6

wine3$FreeSulfurDioxide [(wine3$FreeSulfurDioxide >= 350)] = 350
wine3$FreeSulfurDioxide [(wine3$FreeSulfurDioxide <= -275)] = -275

wine3$TotalSulfurDioxide [(wine3$TotalSulfurDioxide >= 725)] = 725
wine3$TotalSulfurDioxide [(wine3$TotalSulfurDioxide <= -400)] = -400

wine3$Density [(wine3$Density >= 1.06)] = 1.06
wine3$Density [(wine3$Density <= 0.93)] = 0.93

wine3$pH [(wine3$pH >= 4.75)] = 4.75
wine3$pH [(wine3$pH <= 1.5)] = 1.5

wine3$Sulphates [(wine3$Sulphates >= 2.5)] = 2.5
wine3$Sulphates [(wine3$Sulphates <= -1.5)] = -1.5

wine3$Alcohol [(wine3$Alcohol >= 20)] = 20
wine3$Alcohol [(wine3$Alcohol <= 3)] = 3

wine3$AcidIndex [(wine3$AcidIndex >= 11)] = 11
wine3$AcidIndex [(wine3$AcidIndex <= 5)] = 5

summary(wine3)

# Stand Alone Scoring

ZIP_Model<-zeroinfl(TARGET ~ VolatileAcidity + FreeSulfurDioxide + TotalSulfurDioxide + Chlorides + Alcohol + LabelAppeal + logAcidIndex + logSTARS + NoSTARS, data=wine2)

wine3$P_TARGET <- predict(ZIP_Model, newdata = wine3, type = "response")

summary(wine3)

select <- dplyr::select

# Scored Data File
scores <- wine3[c("INDEX","P_TARGET")]
write.csv(scores, file = "U3_Scored2.csv", row.names = FALSE)
write.csv(as.data.frame(scores), file = "WINE_TEST.csv", 
          sheetName = "Scored Data File", row.names = FALSE)








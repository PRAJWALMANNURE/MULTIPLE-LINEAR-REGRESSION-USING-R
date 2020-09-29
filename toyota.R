#load data set
library(readr)
toyota  <- read.csv(file.choose())
View(toyota)
toyota1 <- toyota[,-c(1,2,5,6,8,10,11,12,15,19,20,21,21:38)]
View(toyota1)
attach(toyota1)

##EXPLORATORY DATA ANALYSIS####
summary(toyota1)

#skewness
install.packages("fBasics")
library(fBasics)

colSkewness(toyota1)
##Results######################################################################
#     Price     Age_08_04            KM            HP            cc         Doors         Gears 
#1.70032698   -0.82497558    1.01379080    0.95383970   27.37451064   -0.07623547    2.27919045 

#Quarterly_Tax        Weight 
#1.98967041    3.10214804 
#############################################################################################

#Kurtosis
colKurtosis(toyota1)
###Results########################################
#     Price     Age_08_04            KM            HP            cc         Doors         Gears 
#3.71124703   -0.08460596    1.66851098    8.78509003  926.17410359   -1.87398881   37.51166726 

#Quarterly_Tax        Weight 
#4.26908336   19.26033651 
##############################################################################################

library(ggplot2)
ggplot(stack(toyota1),aes(x = ind, y = values)) +
  geom_boxplot()
#outliers present in KM ,Price & cc varialbles.

barplot(Price)
barplot(Age_08_04)
barplot(KM)
barplot(HP)
barplot(cc)
barplot(Doors)
barplot(Gears)
barplot(Quarterly_Tax)
barplot(Weight)

hist(Price)      #positively skewed
hist(Age_08_04)  #negatively skewed
hist(KM)         #positively skewed
hist(HP)         #positively skewed 
hist(cc)         #positively skewed 
hist(Doors)      #positively skewed 
hist(Gears)      #positively skewed 
hist(Quarterly_Tax)#positively skewed 
hist(Weight)     #positively skewed 

is.na(toyota1)

#finding the correlation between ip variable and op variables
pairs(toyota1)

#corelation coeficient matrix
cor(toyota1)

#partial correlation matrix-pure corelation between variable
library(corpcor)
cor2pcor(cor(toyota1))

#linear model of all the interested variables
model <- lm(Price~., data = toyota1)
summary(model) # Doors & CC are insignificant with low p-value


#Results###############################################################
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9366.4  -793.3   -21.3   799.7  6444.0 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -5.573e+03  1.411e+03  -3.949 8.24e-05 ***
#  Age_08_04     -1.217e+02  2.616e+00 -46.512  < 2e-16 ***
#  KM            -2.082e-02  1.252e-03 -16.622  < 2e-16 ***
#  HP             3.168e+01  2.818e+00  11.241  < 2e-16 ***
#  cc            -1.211e-01  9.009e-02  -1.344  0.17909    
#Doors         -1.617e+00  4.001e+01  -0.040  0.96777    
#Gears          5.943e+02  1.971e+02   3.016  0.00261 ** 
#  Quarterly_Tax  3.949e+00  1.310e+00   3.015  0.00262 ** 
#  Weight         1.696e+01  1.068e+00  15.880  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 1342 on 1427 degrees of freedom
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863 
#F-statistic:  1131 on 8 and 1427 DF,  p-value: < 2.2e-16

#############################################################################################

#checking the colinearity problem
model1 <- lm(Price~cc,data = toyota1)
summary(model1) # its significant 
 
model2 <- lm(Price~Doors,data = toyota1)
summary(model2) # its significant

model3 <- lm(Price~cc+Doors,data = toyota1)
summary(model3) #its significant

#variable inflation factor (vif) to check the colinearity problem
library(car)
vif(model)
######################################################################################
#Age_08_04            KM            HP            cc         Doors       Gears   Quarterly_Tax 
#1.884620      1.756905      1.419422      1.163894      1.156575      1.098723      2.311431 

#Weight 
#2.516420 
#############################################################################################

#added variable plots to check colinearity problem
avPlots(model)
#From the graph,doors is contributing least to the price.

windows()
influence.measures(model)
influenceIndexPlot(model)
influencePlot(model)
#Results###################################################3
#influencePlot(model)
#StudRes       Hat      CookD
#81   8.164500 0.9182368 79.5201062
#222 -7.673262 0.1397116  1.0210312
#961 -5.456195 0.1572484  0.6049996
#################################################################

### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(toyota1,lower.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")


##########################################################################

reg <- lm(Price~.-Doors,data = toyota1[-81,])
summary(reg)

residual <- reg$residuals

sqrt(sum(residual^2)/nrow(toyota1)) #RMSE value is 1308.256

##Results###########################################
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11372.3   -759.9    -26.4    743.5   6777.4 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -6.314e+03  1.382e+03  -4.569 5.32e-06 ***
#  Age_08_04     -1.205e+02  2.561e+00 -47.031  < 2e-16 ***
#  KM            -1.789e-02  1.275e-03 -14.029  < 2e-16 ***
#  HP             3.916e+01  2.898e+00  13.512  < 2e-16 ***
#  cc            -2.507e+00  3.062e-01  -8.188 5.83e-16 ***
#  Gears          5.497e+02  1.892e+02   2.905  0.00373 ** 
#  Quarterly_Tax  9.076e+00  1.424e+00   6.374 2.47e-10 ***
#  Weight         1.996e+01  1.076e+00  18.547  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 1312 on 1427 degrees of freedom
#Multiple R-squared:  0.8693,	Adjusted R-squared:  0.8687 
#F-statistic:  1356 on 7 and 1427 DF,  p-value: < 2.2e-16

################################################################

reg1 <- lm(Price~.-Doors,data = toyota1[c(-81,-222),])
summary(reg1) #Multiple R-squared:  0.8776,	Adjusted R-squared:  0.877

reg2 <- lm(Price~.-Doors,data = toyota1[c(-81,-222,-961),])
summary(reg2) #Multiple R-squared:  0.8843,	Adjusted R-squared:  0.8838

reg3 <- lm(log(Price)~.-Doors,data = toyota1[c(-81,-222,-961),])
summary(reg3) #Multiple R-squared:  0.856,	Adjusted R-squared:  0.8553 

reg4 <- lm(log(Price)~(Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)+
             I(Age_08_04*Age_08_04+KM*KM+HP*HP+cc*cc+Gears*Gears+
                 Quarterly_Tax*Quarterly_Tax+Weight*Weight),data = toyota1[c(-81,-222,-961),])
summary(reg4)

#Multiple R-squared:  0.9042,	Adjusted R-squared:  0.9023

logp <- predict(reg4)
expp <- exp(logp)

error <- Price-expp

sqrt(sum(error^2)/nrow(toyota1))#RMSE value is 6050.793

##With highest r squared value reg4 is a better fit model
##########################################################################################

#load data set
library(readr)
startup <- read.csv(file.choose())
View(startup)
attach(startup)

####EXPLORATORY DATA ANALYSIS#########
summary(startup)
var(Administration)
var(R.D.Spend)
var(Profit)
var(Marketing.Spend)

sd(Administration)
sd(R.D.Spend)
sd(Profit)
sd(Marketing.Spend)

#meassures of skewness
library(moments)
skewness(Administration) # -0.4742301, negative skewness
skewness(R.D.Spend) # 0.1590405, positive skewness
skewness(Profit) # 0.02258638, positive skewness
skewness(Marketing.Spend) # -0.04506632, negative skewness

#meassures of kurtosis
kurtosis(Administration) # 3.085538 positive kurtosis
kurtosis(R.D.Spend) # 2.194932 positive kurtosis
kurtosis(Profit)    # 2.824704 positive kurtosis
kurtosis(Marketing.Spend) # 2.275967 positive kurtosis


##graphical representation
boxplot(Administration,horizontal = T,col = 'green') # there are no outliers
boxplot(R.D.Spend,horizontal = T,col = 'blue') # there are no outliers
boxplot(Profit,horizontal = T,col = 'red') # there are no outliers
boxplot(Marketing.Spend,horizontal = T,col = 'pink') # there are no outliers

hist(Administration,col = 'green') # its a negatively skewed data
hist(R.D.Spend,col = 'blue') # its a positively skewed data
hist(Profit,col = 'red',border = 'black') # it shows negatively skewed data
hist(Marketing.Spend,col = 'pink') # shows positively skewed data

barplot(Administration,col = 'green')
barplot(R.D.Spend,col = 'blue')
barplot(Profit,col = 'red')
barplot(Marketing.Spend,col = 'pink')

library(ggpubr)
ggdensity(Administration) # its a bell shaped curve
ggdensity(R.D.Spend) # partially positively skewed bell shape
ggdensity(Profit) # bell shaped curve with high peak
ggdensity(Marketing.Spend) # positively skewed bell shape

#scatter-plot for finding correlation between output profit and inputs
pairs(startup) # there is a moderate corelation/colinearity between marketing spend and R.D.spend


#correlation coeffiecent matrix of inputs and output
cor(Administration,Profit) # 0.2007166, weak correlation 
cor(R.D.Spend,Profit) # 0.9729005 strong correlation
cor(Marketing.Spend,Profit) # 0.7477657, moderate correlation

startup$State <-as.integer(startup$State)

cor(startup) # there is a strong corelarion between profit and R.D.spend

###partial correlation matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup))

##linear model of interest with all columns
reg <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup)
summary(reg)

####Results#############################################################
#Residuals:
#  Min     1Q Median     3Q    Max 
#-33534  -4795     63   6606  17275 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      5.012e+04  6.572e+03   7.626 1.06e-09 ***
#  R.D.Spend        8.057e-01  4.515e-02  17.846  < 2e-16 ***
#  Administration  -2.682e-02  5.103e-02  -0.526    0.602    
#Marketing.Spend  2.723e-02  1.645e-02   1.655    0.105    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 9232 on 46 degrees of freedom
#  Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9475 
#F-statistic:   296 on 3 and 46 DF,  p-value: < 2.2e-16


# The p-value of Adminstration and Marketing spend are insignificant.
###############################################################################



## Multico-linearity check #########
reg1 <- lm(Profit~R.D.Spend)
summary(reg1) # the r.d.spend alone is significant with high r-squared value

######Results#################################
#Residuals:
#  Min     1Q Median     3Q    Max 
#-34351  -4626   -375   6249  17188 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 4.903e+04  2.538e+03   19.32   <2e-16 ***
#  R.D.Spend   8.543e-01  2.931e-02   29.15   <2e-16 ***
#  ---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 9416 on 48 degrees of freedom
#Multiple R-squared:  0.9465,	Adjusted R-squared:  0.9454 
#F-statistic: 849.8 on 1 and 48 DF,  p-value: < 2.2e-16



#######################################################################

reg2 <- lm(Profit~Administration)
summary(reg2) # adminstration variable alone is insignificant with high p-value

####Results########################################
#Residuals:
#  Min     1Q Median     3Q    Max 
#-96072 -23426  -3564  25438  84870 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)    7.697e+04  2.532e+04   3.040  0.00382 **
#  Administration 2.887e-01  2.034e-01   1.419  0.16222   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 39900 on 48 degrees of freedom
#Multiple R-squared:  0.04029,	Adjusted R-squared:  0.02029 
#F-statistic: 2.015 on 1 and 48 DF,  p-value: 0.1622


#########################################################################

reg3 <- lm(Profit~Marketing.Spend)
summary(reg3) # the marketing spend variable is significant with less p-value

####Results#########################################
#Residuals:
#  Min     1Q Median     3Q    Max 
#-83739 -18802   4925  15879  64642 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     6.000e+04  7.685e+03   7.808 4.29e-10 ***
#  Marketing.Spend 2.465e-01  3.159e-02   7.803 4.38e-10 ***
#  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 27040 on 48 degrees of freedom
#Multiple R-squared:  0.5592,	Adjusted R-squared:   0.55 
#F-statistic: 60.88 on 1 and 48 DF,  p-value: 4.381e-10



####################################################################################

reg4 <- lm(Profit~Administration+Marketing.Spend)
summary(reg4) #Both are significant with p-value less than 0.05

#####Results#################################
#Residuals:
#  Min     1Q Median     3Q    Max 
#-82155 -12168   2836  13650  56472 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     2.022e+04  1.770e+04   1.143   0.2589    
#Administration  3.237e-01  1.312e-01   2.468   0.0173 *  
#  Marketing.Spend 2.488e-01  3.005e-02   8.281 9.73e-11 ***
#  ---  
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 25710 on 47 degrees of freedom
#Multiple R-squared:  0.6097,	Adjusted R-squared:  0.5931 
#F-statistic: 36.71 on 2 and 47 DF,  p-value: 2.496e-10


#######################################################################

reg5 <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(reg5) # Marketing spend is insignificant with high p-value

###Results#########################################################
#Residuals:
#  Min     1Q Median     3Q    Max 
#-33645  -4632   -414   6484  17097 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     4.698e+04  2.690e+03  17.464   <2e-16 ***
#  R.D.Spend       7.966e-01  4.135e-02  19.266   <2e-16 ***
#  Marketing.Spend 2.991e-02  1.552e-02   1.927     0.06 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 9161 on 47 degrees of freedom
#Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9483 
#F-statistic: 450.8 on 2 and 47 DF,  p-value: < 2.2e-16



####################################################################################


#Applying vif function built on all input model to check colinearity problem
#variance inflation factor to check coliearity between variables
library(car)
vif(reg) #vif value is less than 10 for all varibles

#added variable plot to check correlation between variables
avPlots(reg)

#there is no change in the adminstration with profit,
#So avplot indicates to delete adminstration 

#scatter plot matrix along with correlation values

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
pairs(startup,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

#here also we can see colinearity problem of r.d.spend and marketng spend

#its better to delete influential observation rather than deleting entire coumn

#identifying influential observations
library(car)
influence.measures(reg)
windows()
influenceIndexPlot(reg)

#Regression after deleting the influential observation 50

reg.inf <- lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[-50,])
summary(reg.inf)

reg.inf1 <- lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[c(-49,-50),])
summary(reg.inf1)

reg.inf3 <- lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[c(-47,-49,-50),])              
summary(reg.inf3)

##final model
plot(lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[-50,]))
plot( lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[c(-49,-50),]))
plot(lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[c(-47,-49,-50),]))


final.model <- lm(Profit~R.D.Spend+Marketing.Spend-Administration,data = startup[c(-47,-49,-50),]) 
summary(final.model)

#evaluate model line assuption
plot(final.model)

hist(residuals(final.model)) # Its equal to normal distribution

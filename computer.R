#load data set
library(readr)
computer <- read.csv(file.choose())
View(computer)
computer <- computer[-1]
View(computer)

##EXPLORATORY DATA ANALYSIS###

computer$cd      <- ifelse(computer$cd=='yes',1,0)
computer$multi   <- ifelse(computer$multi=='yes',1,0)
computer$premium <- ifelse(computer$premium=='yes',1,0)
View(computer)
summary(computer)

attach(computer)

#skewness
library(moments)

skewness(price)  #0.7115542
skewness(speed)  #0.6568505
skewness(hd)     #1.377689
skewness(ram)    #1.38587
skewness(screen) #1.633616
skewness(ads)    #-0.5531955
skewness(trend)  #0.2366127

#kurtosis
kurtosis(price)  # 3.728875 positive kurtosis
kurtosis(speed)  # 2.723809 positive kurtosis 
kurtosis(hd)     # 5.449539 positive kurtosis
kurtosis(ram)    # 4.460124 positive kurtosis
kurtosis(screen) # 4.849387 positive kurtosis
kurtosis(ads)    # 2.459629 positive kurtosis
kurtosis(trend)  # 2.325446 positive kurtosis

#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}



getmode(computer$cd)       # number of NO are maximum
getmode(computer$multi)    # number of NO are maximum
getmode(computer$premium)  # number of yes are maximum

boxplot(computer$price,horizontal = T)  # many outliers are present
boxplot(computer$speed,horizontal = T)  # no outliers
boxplot(computer$hd,horizontal = T)     # many outliers
boxplot(computer$ram,horizontal = T)    # many outliers
boxplot(computer$screen,horizontal = T) # there is a outlier
boxplot(computer$ads,horizontal =T)     # no outliers
boxplot(computer$trend,horizontal = T)  # no outliers

hist(computer$price)  # normal data
hist(computer$speed)  
hist(computer$hd)     # positively skewed data
hist(computer$ram)    #positively skewed data
hist(computer$screen) # positively skewed data
hist(computer$ads)    #negatively skewed data
hist(computer$trend)  # normal data

library(ggpubr)
ggdensity(computer$price) #normal data with long tail on right
ggdensity(computer$speed) #not normal
ggdensity(computer$hd)    #not normal
ggdensity(computer$ram)   #not normal
ggdensity(computer$screen)#not normal
ggdensity(computer$ads)   #not normal
ggdensity(computer$trend) #not normal

#barplot
barplot(price)
barplot(speed)
barplot(hd)
barplot(ram)
barplot(screen)
barplot(ads)
barplot(trend)

is.na(computer)


#Find the corelation between o/p(price) & ip variables
pairs(computer)

#correlation coefficent matrix
cor(computer)

#partial correlation matrix-pure corelation btw variables
library(corpcor)
cor2pcor(cor(computer))

#linear model of all the intrested variables
model <- lm(price~.,data = computer)
summary(model) 
# the variables premium & trend seems to be insignificant with negative estimate value

##Results################################################
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1150.58  -175.94   -13.36   148.11  2005.38 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  397.12939   61.09457   6.500 8.64e-11 ***
#  X              0.11499    0.01447   7.945 2.28e-15 ***
#  speed          9.41656    0.18454  51.027  < 2e-16 ***
#  hd             0.80727    0.02766  29.181  < 2e-16 ***
#  ram           47.96454    1.06145  45.188  < 2e-16 ***
#  screen       122.63226    3.98018  30.811  < 2e-16 ***
#  cd            50.28807    9.56267   5.259 1.50e-07 ***
#  multi        105.40549   11.35718   9.281  < 2e-16 ***
#  premium     -510.21053   12.28198 -41.541  < 2e-16 ***
#  ads            0.54500    0.05298  10.287  < 2e-16 ***
#  trend        -78.53759    3.41668 -22.987  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 274 on 6248 degrees of freedom
#Multiple R-squared:  0.7778,	Adjusted R-squared:  0.7775 
#F-statistic:  2187 on 10 and 6248 DF,  p-value: < 2.2e-16


#################################################################

model1 <- lm(price~premium)
summary(model1)
#still the value is negative

model2 <- lm(price~trend)
summary(model2)

model3 <- lm(price~premium+trend)
summary(model3)



#still the estimate value is negative
###Results##################################################
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1322.76  -421.36   -79.75   366.74  3121.49 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2454.5238    16.2330  151.21   <2e-16 ***
#  trend        -14.7515     0.9137  -16.14   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 569.1 on 6257 degrees of freedom
#Multiple R-squared:  0.03999,	Adjusted R-squared:  0.03984 
#F-statistic: 260.7 on 1 and 6257 DF,  p-value: < 2.2e-16
################################################################

#variable inflation factor(vif) to check colinearity between variable
library(car)

vif(model) # trend is having value > 10

###############################################################################
#       X     speed        hd       ram    screen        cd     multi   premium       ads     trend 
#57.005017  1.270844  4.264755  2.978184  1.081869  1.896482  1.290754  1.109501  1.310459 60.333789 
######################################################################################################


#Added variable plot to check colinearity between variable
avPlots(model)
#there is no change at all in cd and multi, but there is no colinearity problem

#identifying influential observations
influence.measures(model)

windows()
influenceIndexPlot(model)
influencePlot(model)

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
pairs(computer,lower.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

#############################################################################################33

reg <- lm(price~.,data = computer[-4478,])
summary(reg)

reg1 <- lm(price~., data = computer[c(-4478,-3784),])
summary(reg1)

reg2 <- lm(price~., data = computer[c(-4478,-3784,-1701),])
summary(reg2)

## Logarthemic Model####################################3
reg3 <- lm(log(price)~., data = computer[c(-4478,-3784,-1701,-1441),])
summary(reg3)

##Results##
#Multiple R-squared:  0.7836,	Adjusted R-squared:  0.7833
###################################################################################3

#####polynomial model with degree 2#########################

reg4 <- lm(log(price)~(speed+hd+ram+screen+cd+multi+premium+ads+trend)+
        I(speed*speed+hd*hd+ram*ram+screen*screen+cd*cd+multi*multi+premium*premium+ads*ads+trend*trend),
        data = computer[c(-4478,-3784,-1701,-1441),])
summary(reg4)

logp <- predict(reg4)
expp <- exp(logp)

error <- price-expp

sqrt(sum(error^2)/nrow(computer)) #RMSE value is 255.501

#Hence this model is better model
##Result#############################
#Multiple R-squared:  0.8021,	Adjusted R-squared:  0.8017
###################################################



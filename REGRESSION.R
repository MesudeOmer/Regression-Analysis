###libraries
library(readxl)
library(nortest)
library(Hmisc)
library(caret)
library(dplyr)
library(car)
library(perturb)
library(MASS)

###import data
regresyon <- read_excel("C:/Users/SUDE/Desktop/21936225.MesudeOmer.xlsx")
names(regresyon) = c("y","x1","x2","x3","x4")
x4 = as.factor(x4)
regresyon$y = as.double(regresyon$y)
regresyon$x2 = as.double(regresyon$x2)

###describetive statistics
describe(regresyon)
summary(regresyon)
var(regresyon$y)
var(regresyon$x1)
var(regresyon$x2)
var(regresyon$x3)
sd(regresyon$y)
sd(regresyon$x1)
sd(regresyon$x2)
sd(regresyon$x3)

###linearity test
qqnorm(regresyon$y)
qqline(regresyon$y)
pairs(regresyon)

###normality test
lillie.test(regresyon$y)

###normalization
yy <- 1/(regresyon$y)
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}
reg <- as.data.frame(lapply(regresyon, norm_minmax))

reg2 <- slice(reg,c(-12:-57),)
shapiro.test(reg2$y)

###new data
reg2$x4 = as.factor(reg2$x4)
reg2$y = as.double(reg2$y)
reg2$x2 = as.double(reg2$x2)
pairs(reg2)
qqnorm(reg2$y)
qqline(reg2$y)

###regression analysis
sonuc<-lm(reg2$y~reg2$x1+reg2$x2+reg2$x3+reg2$x4)
sonuc
summary(sonuc)

confint(sonuc, level = .99)

cor(reg2)

sonuc1 <-lm(y~x1+x2+x3+x4+x1*x4+x2*x4+x3*x4,data=reg2)
summary(sonuc1)

###Residuals
predict(sonuc)
inf<-ls.diag(sonuc)
inf

influence.measures(sonuc)

par(mfrow=c(2,2))
plot(predict(sonuc), abs(inf$stud.res), ylab="Studentized Residuals", xlab="Predicted Value")

###outliers
###cook method
cooksd <- cooks.distance(sonuc)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")

n = 104
k = 4
abline(h = if (n>50) 4/n else 4/(n-k-1) , col="red")

text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""), col="red")

###extreme value metdod
library(zoo)

hat<-inf$hat

plot(hat, pch="*", cex=2, main="Leverage Value by Hat value")

abline(h = 2*(k+1)/n , col="red")

text(x=1:length(hat)+1, y=hat, labels=ifelse(hat>2*(k+1)/n,index(hat),""), col="red")

###heteroscedasticity
summary(lm(abs(residuals(sonuc)) ~ fitted(sonuc)))


###heteoscedasticity test
library(lmtest)
bptest(sonuc)


# squere of residuals
res = residuals(sonuc)

sqres = res^2

# regression model with squere of residuals
BP = lm(sqres ~ reg2$x1 + reg2$x2+reg2$x3+reg2$x4)
BPs = summary(BP)

# Lagrange product
BPts = BPs$r.squared*length(BP$residuals)

# Qhi-squere distribition
BPpv = 1-pchisq(BPts,df=BP$rank-1)

# confidince interval

if (BPpv < 0.05) {
  
  cat("We reject the null hypothesis of homoskedasticity.\n",
      
      "BP = ",BPts,"\n","p-value = ",BPpv)
  
} else {
  
  cat("We fail to reject the null hypothesis; implying homoskedasticity.\n",
      
      "BP = ",BPts,"\n","p-value = ",BPpv)
  
}

###durbin watson testi
dwtest(sonuc)

###multiconnection
vif(sonuc)


##ridge regression
ridge <- lm.ridge(reg2$y~reg2$x1+reg2$x2+reg2$x3+reg2$x4 ,lambda = seq(0,1,0.05))

matplot(ridge$lambda,t(ridge$coef),type="l",xlab=expression(lambda),
        
        ylab=expression(hat(beta)))

abline(h=0,lwd=2)

ridge$coef
ridge$coef[,ridge$lam == 0.4]

#estimation
reg2[1,]
estimation = data.frame(x1=0.739,x2=0.804,x3=0,07,x4=as.factor(0))
predict(sonuc1,newdata = estimation,interval = "confidence")

#prediction
prediction = data.frame(x1=0.4,x2=0.7,x3=0.1,x4=as.factor(1))
predict(sonuc1,newdata = önkestirimi,interval = "confidence")

#forward 
lm.null = lm(reg2$y~1)
forward = step(lm.null,reg2$y~reg2$x1+reg2$x2+reg2$x3+reg2$x4,direction = "forward")
summary(forward)

#backward
backward = step(lm.null,reg2$y~reg2$x1+reg2$x2+reg2$x3+reg2$x4,direction = "backward")
summary(backward)

#stepwise
step.sonuc = stepAIC(sonuc,direction ="both", trace = F)
summary(step.sonuc)




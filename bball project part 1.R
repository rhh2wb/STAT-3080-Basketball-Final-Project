library(data.table)
library(xlsx)
library(psych)

setwd("C:/Users/owner/Downloads")

bb = read.xlsx(file = 'basketball.xlsx', sheetName = 'basketball', header = T)
setnames(bb, c('X1', 'X2', 'X3', 'X4', 'X5'), c('ht',  'wt', 'fg', 'ft', 'pt'))

#convert ht in feet to inches
bb$ht <- bb$ht*12

#Basic statistics
summary(bb)
describe(bb)

#Assess normality assumptions

#Ht
hist(bb$ht)
shapiro.test(bb$ht)
qqnorm(bb$ht);qqline(bb$ht)

#Wt
hist(bb$wt)
shapiro.test(bb$wt)
qqnorm(bb$wt);qqline(bb$wt)

#fg
hist(bb$fg)
shapiro.test(bb$fg)
qqnorm(bb$fg);qqline(bb$fg)

#ft
hist(bb$ft)
shapiro.test(bb$ft)
qqnorm(bb$ft);qqline(bb$ft)
#transform by x^3 because downward skew
hist((bb$ft)^3)
shapiro.test((bb$ft)^3)
qqnorm((bb$ft)^3);qqline((bb$ft)^3)

#pt
hist(bb$pt)
shapiro.test(bb$pt)
qqnorm(bb$pt);qqline(bb$pt)
#transform by sqrt(x) because downward skew
hist((bb$pt)^0.5)
shapiro.test((bb$pt)^0.5)
qqnorm((bb$pt)^0.5);qqline((bb$pt)^0.5)


######################################################################################
#One sample parametric

#H0: mu = 0.5
t.test(bb$fg, alternative = 'less', mu = 0.5)
#t = -6.6127, df = 53, p-value = 9.468e-09

######################################################################################
#2 sample independent parametric

#BMI
bb$bmi <- bb$wt*703/(bb$ht^2)
mid_bmi <- median(bb$bmi)

t.test(bb$bmi[which(bb$bmi>=mid_bmi)],bb$bmi[which(bb$bmi<mid_bmi)],alternative = 'greater')
#t = 6.7236, df = 39.352, p-value = 2.464e-08

#Height
avg_ht <- mean(bb$ht)
t.test(bb$pt[which(bb$ht>=avg_ht)],bb$pt[which(bb$ht<avg_ht)],alternative = 'greater')
#t = -1.3114, df = 50.135, p-value = 0.9021

######################################################################################
#2 sample dependent parametric

#Good ft = 80%, good fg = 50%
t.test(bb$fg,bb$ft, mu = 0.3)
#t = -37.873, df = 83.681, p-value < 2.2e-16

######################################################################################
#2 sample categorical

third_wt <- quantile(bb$wt,1/3)
two_third_wt <- quantile(bb$wt,2/3)
third_ht <- quantile(bb$ht,1/3)
two_third_ht <- quantile(bb$ht,2/3)





######################################################################################
#Correlation parametric

cor(bb$pt, bb$ft)
#0.2448517
library(NHANES)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(visreg)
library(scatterplot3d)
library(UsingR)
small.nhanes <- na.omit(NHANESraw[NHANESraw$SurveyYr=="2011_12"& NHANESraw$Age > 17,c(1,3,4,8:11,13,24,25,61,77)])
small.nhanes <- small.nhanes %>%group_by(ID) %>% filter(row_number()==1)
set.seed(1004149361)
#setting randomizer as student number
sample_500 <- sample(1:4581, 500, replace = F)
#sampling 500 entries of the small nhanes data set without replacement
smaller.nhanes <- small.nhanes[sample_500, ]
boxplot(smaller.nhanes$BPSysAve, ylab = "BPSysAve", main = "Blood Pressure Systolic", col = "gray")
#univariate boxplot of all the blood pressures within our 500 sample
dev.off()
hist(smaller.nhanes$BPSysAve, col = "gray", border = "black" , xlab = "BPSysAve", main = "Histogram of BPS (Blood Pressure Systolic)")
dev.off
#plotting univariate histogram of all the blood pressures
boxplot(BPSysAve ~ Smoke100, data = smaller.nhanes, xlab = "Smoke100", ylab = "BPSysAve", main = "BPSysAve in smokers vs nonsmokers")
#plotting bivariate boxplot of our blood pressure in smokers vs non smokers
dev.off()
Smokers <- filter(smaller.nhanes, Smoke100 == "Yes")
Non_Smokers <- filter(smaller.nhanes,Smoke100 == "No")
#separating smokers and non smokers
ggplot(smaller.nhanes, aes(x = BPSysAve)) +
  geom_histogram(fill = "gray", colour = "black") +
  facet_grid(Smoke100 ~ .) + labs(title = "Histogram of Blood Pressure in those who have smoked 100 times")
#plot of bivariate histogram of Blood pressure in smokers vs non smokers (figure1)
dev.off()
x <- Smokers$BPSysAve
y <- Non_Smokers$BPSysAve
var.test(x,y)
#complete variance test conclusion non equal variance
t.test(x,y, var.equal = F)
#complete t-test we reject the null hypothesis.
model.smoke <- lm(BPSysAve ~ Smoke100, data = smaller.nhanes)
#running linear regression for smoke and blood pressure
Anova(model.smoke)
model.age <- lm(BPSysAve ~ Age, data = smaller.nhanes)
#running linear regression for age and blood pressure
summary(model.age)
model.edu <- lm(BPSysAve ~ Education, data = smaller.nhanes)
#running linear regression for education and blood pressure
summary(model.edu)
model.inc <- lm(BPSysAve ~ HHIncomeMid, data = smaller.nhanes)
#running linear regression for HHIncomeMid and blood pressure
summary(model.inc)
model.age_inc <- lm(BPSysAve ~ Age + HHIncomeMid, data = smaller.nhanes)
summary(model.age_inc)
pdf("scatter3d.pdf", height = 8, width =12)
s3d <- scatterplot3d(cbind(s$Age, s$HHIncomeMid, s$BPSysAve), type = "h", color = "blue", angle = 55, pch = 16, main = "Regression Plane", xlab = "Age", ylab = "HHIncomeMid",zlab = "Blood Pressure")
s3d$plane3d(model.age_inc2, col = "red")
#3d model of linear regression between HHIncomeMid + age vs blood pressure (Figure 4)
dev.off()
strata1 <- filter(small.nhanes, SDMVSTRA == 90)
strata2 <- filter(small.nhanes, SDMVSTRA == 91)
strata3 <- filter(small.nhanes, SDMVSTRA == 92)
strata4 <- filter(small.nhanes, SDMVSTRA == 93)
strata5 <- filter(small.nhanes, SDMVSTRA == 94)
strata6 <- filter(small.nhanes, SDMVSTRA == 95)
strata7 <- filter(small.nhanes, SDMVSTRA == 96)
strata8 <- filter(small.nhanes, SDMVSTRA == 97)
strata9 <- filter(small.nhanes, SDMVSTRA == 98)
strata10 <- filter(small.nhanes, SDMVSTRA == 99)
strata11 <- filter(small.nhanes, SDMVSTRA == 100)
strata12 <- filter(small.nhanes, SDMVSTRA == 101)
strata13 <- filter(small.nhanes, SDMVSTRA == 102)
strata14 <- filter(small.nhanes, SDMVSTRA == 103)
#assigning each strata based on the SDMVSTRA which will make up 14 differnet strata
sig1 <- (max(strata1$BPSysAve) - min(strata1$BPSysAve))/4
sig2 <- (max(strata2$BPSysAve) - min(strata2$BPSysAve))/4
sig3 <- (max(strata3$BPSysAve) - min(strata3$BPSysAve))/4
sig4 <- (max(strata4$BPSysAve) - min(strata4$BPSysAve))/4
sig5 <- (max(strata5$BPSysAve) - min(strata5$BPSysAve))/4
sig6 <- (max(strata6$BPSysAve) - min(strata6$BPSysAve))/4
sig7 <- (max(strata7$BPSysAve) - min(strata7$BPSysAve))/4
sig8 <- (max(strata8$BPSysAve) - min(strata8$BPSysAve))/4
sig9 <- (max(strata9$BPSysAve) - min(strata9$BPSysAve))/4
sig10 <- (max(strata10$BPSysAve) - min(strata10$BPSysAve))/4
sig11 <- (max(strata11$BPSysAve) - min(strata11$BPSysAve))/4
sig12 <- (max(strata12$BPSysAve) - min(strata12$BPSysAve))/4
sig13 <- (max(strata13$BPSysAve) - min(strata13$BPSysAve))/4
sig14 <- (max(strata14$BPSysAve) - min(strata14$BPSysAve))/4
#taking the range(max(strata)-min(strata)) and dividing by 4 to find the sigma value
sig <- c(sig1,sig2,sig3,sig4,sig5,sig6,sig7,sig8,sig9,sig10,sig11,sig12,sig13,sig14)
# making a row vector for all the sigma values
ME <- 4
alph <- 0.01
Z <- qnorm(1-alph/2)
D <- ME^2/Z^2
#assigning some given variables
N1 <- sum(small.nhanes$SDMVSTRA == 90)
N2 <- sum(small.nhanes$SDMVSTRA == 91)
N3 <- sum(small.nhanes$SDMVSTRA == 92)
N4 <- sum(small.nhanes$SDMVSTRA == 93)
N5 <- sum(small.nhanes$SDMVSTRA == 94)
N6 <- sum(small.nhanes$SDMVSTRA == 95)
N7 <- sum(small.nhanes$SDMVSTRA == 96)
N8 <- sum(small.nhanes$SDMVSTRA == 97)
N9 <- sum(small.nhanes$SDMVSTRA == 98)
N10 <- sum(small.nhanes$SDMVSTRA == 99)
N11 <- sum(small.nhanes$SDMVSTRA == 100)
N12 <- sum(small.nhanes$SDMVSTRA == 101)
N13 <- sum(small.nhanes$SDMVSTRA == 102)
N14 <- sum(small.nhanes$SDMVSTRA == 103)
NL <- c(N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14)
#taking the totaln number of rows in each data set and compiling that into a single row vector
N <- sum(NL)
#total population size
cost <- c( 52, 50, 46, 53, 48, 48, 47, 57, 53, 47, 54, 40, 43, 44)
num <- sum(NL * sig / sqrt(cost)) * sum(NL * sig * sqrt(cost))
#calculating the numerator of the population size estimate equation 
den <- N^2 * D + sum(NL * sig^2)
#calculating the denominator of the population size estimate equation
n <- num/den
n
#calculating and showing the population size estimate
nj <- n * (NL * sig/ sqrt(cost))/(sum(NL * sig/sqrt(cost)))
round(nj,0)
set.seed(1004149361)
index1 <- sample(1:nrow(strata1), 25, replace = F)
index2 <- sample(1:nrow(strata2), 40, replace = F)
index3 <- sample(1:nrow(strata3), 37,  replace = F)
index4 <- sample(1:nrow(strata4), 26, replace = F)
index5 <- sample(1:nrow(strata5), 23, replace = F)
index6 <- sample(1:nrow(strata6), 35, replace = F)
index7 <- sample(1:nrow(strata7), 24, replace = F)
index8 <- sample(1:nrow(strata8), 15, replace = F)
index9 <- sample(1:nrow(strata9), 23, replace = F)
index10 <- sample(1:nrow(strata10), 18, replace = F)
index11 <- sample(1:nrow(strata11), 21, replace = F)
index12 <- sample(1:nrow(strata12), 38, replace = F)
index13 <- sample(1:nrow(strata13), 16, replace = F)
index14 <- sample(1:nrow(strata14), 9, replace = F)
#sampling random numbers between 1 and each strata size where the sample size is our previously calculated nj
s1 <- strata1[index1, ]
s2 <- strata2[index2, ]
s3 <- strata3[index3, ]
s4 <- strata4[index4, ]
s5 <- strata5[index5, ]
s6 <- strata6[index6, ]
s7 <- strata7[index7, ]
s8 <- strata8[index8, ]
s9 <- strata9[index9, ]
s10 <- strata10[index10, ]
s11 <- strata11[index11, ]
s12 <- strata12[index12, ]
s13 <- strata13[index13, ]
s14 <- strata14[index14, ]
s <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14)
#using our number index to find the actual samples of each strata and compiling them into a new data set
model.smoke2 <- lm(BPSysAve ~ Smoke100, data = s)
#running the same linear model for stratified sample set
Anova(model.smoke2)
model.age2 <- lm(BPSysAve ~ Age, data = s)
#running the same linear model for stratified sample set
summary(model.age2)
model.edu2 <- lm(BPSysAve ~ Education, data = s)
summary(model.edu2)
boxplot(BPSysAve ~ Education, data = s, xlab = "Education", ylab = "BPSysAve", main = "Boxplot of Blood Pressure in different stages of education")
#boxplot of blood pressure vs educatrion for stratified data set (figure 5)
dev.off()
model.inc2 <- lm(BPSysAve ~ HHIncomeMid, data = s)
summary(model.inc2)
model.age_inc2 <- lm(BPSysAve ~ Age + HHIncomeMid, data = s)
summary(model.age_inc2)
visreg(model.age2, "Age", gg = TRUE) +
  labs(title = "Linear model between Age and Blood Pressure",
       caption = "Stratafied sample(350) of NHANES Data",
       y = "Blood Pressure",
       x = "Age")
#plot of linear regression model for age vs blood pressure in the stratified sample set (figure 2)
dev.off()
visreg(model.inc2, "HHIncomeMid", gg = TRUE) +
  labs(title = "Linear model of Median Household Income and Blood Pressure",
       caption = "Stratafied sample(350) of NHANES Data",
       y = "Blood Pressure",
       x = "HHIncomeMid")
#plot of linear regression model for HHIncomeMid vs blood pressure in the stratified sample set(figure 3)
dev.off()

library(ggplot2)
library(dplyr)
# Question to be answered
# ???Is an automatic or manual transmission better for MPG???
# "Quantify the MPG difference between automatic and manual transmissions"

trans_aut <- mtcars %>% filter(am==0)
# trans_aut <- mtcars %>% filter(am==0) %>% select(mpg)
# subset(mtcars, am==0)
trans_man <- mtcars %>% filter(am==1)
# subset(mtcars, am==1)

#Mean and SD can be calculated for each
trans_aut_mean <- mtcars %>% filter(am==0) %>% summarise(mean(mpg))
trans_man_mean <- mtcars %>% filter(am==1) %>% summarise(mean(mpg))
trans_aut_sd <- mtcars %>% filter(am==0) %>% summarise(sd(mpg))
trans_man_sd <- mtcars %>% filter(am==1) %>% summarise(sd(mpg))

trans_aut_mean
trans_man_mean
trans_aut_sd
trans_man_sd
# H0: Automatique & Manual mpg are = 0
# H1: Automatique & Manual mpg are difference <> 0
t.test(trans_aut$mpg,trans_man$mpg, alternative = 'two.sided')
t.test(trans_aut$mpg,trans_man$mpg, paired = FALSE, alternative = 'two.sided')

#Regression Model
#fit_1 <- lm(mpg ~ factor(am), data = mtcars)
#fit_2 <- lm(mpg ~ factor(am) + factor(cyl), data = mtcars)
#fit_3 <- lm(mpg ~ factor(am) + factor(cyl) + wt, data = mtcars)
#fit_4 <- lm(mpg ~ factor(am) + factor(cyl) + wt + hp, data = mtcars)
#fit_5 <- lm(mpg ~ factor(am) + factor(cyl) + wt + hp + disp, data = mtcars)

fit_0 <- lm(mpg ~ qt, data = mtcars)
fit_1 <- lm(mpg ~ wt + factor(am), data = mtcars)
fit_2 <- lm(mpg ~ wt + factor(am) + factor(cyl), data = mtcars)
fit_3 <- lm(mpg ~ wt + factor(am) + factor(cyl) + wt, data = mtcars)
fit_4 <- lm(mpg ~ wt + factor(am) + factor(cyl) + wt + hp, data = mtcars)
fit_5 <- lm(mpg ~ wt + factor(am) + factor(cyl) + wt + hp + disp, data = mtcars)
anova(fit,fit_1,fit_2,fit_3,fit_4,fit_5)

#Variable interaction
fit_6_inter <- lm(mpg ~ factor(am):wt + factor(cyl) + wt + hp, data = mtcars)
anova(fit_4,fit_6_inter)

#Plot
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit_4)

#Validation
boxplot(mpg ~ am, xlab="Transmission (0 = Automatic, 1 = Manual)", ylab="MPG", main="Boxplot of MPG vs. Transmission")
pairs(mtcars, panel=panel.smooth, main="Pair Graph of Motor Trend Car Road Tests")

#Automated modeling
#Model 1
fit <- lm(mpg ~ ., data = mtcars)
summary(fit) # the highest/most significant is wt!

fit.min <- lm(mpg ~ 1, data=mtcars)  # minimum possible regression model
fit <- step(fit.min, direction="both", scope=(~ factor(am):(cyl+disp+hp+drat+wt+qsec+vs+gear+carb))
summary(fit)

#Model 2
fit<-glm(mpg~as.factor(cyl) + as.factor(vs) + as.factor(am) + as.factor(gear) + as.factor(carb) + disp + hp + drat + wt + qsec, data=mtcars)
library(MASS)
step <- stepAIC(fit, direction="both")
step$anova

#Model 3
fit.full <- lm(mpg ~ factor(cyl) + disp + hp + drat + wt + qsec + factor(vs) + factor(am) + factor(gear) + factor(carb), mtcars)
fit.step.BIC <- step(fit.full, direction = "both", trace = 0, k = log(nrow(mtcars))) # BIC
coef(fit.step.BIC)
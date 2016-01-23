library(ggplot2)
library(dplyr)
library(glmulti)
data("mtcars")
# Question to be answered
# ???Is an automatic or manual transmission better for MPG???
# "Quantify the MPG difference between automatic and manual transmissions"

attach(mtcars)
#Exploratory
hist(mpg) #Normality of data
boxplot(mpg~am, names = c("Automatic", "Manual"),ylab="MPG", main="Boxplot of MPG transmission type")
summary(lm(mpg~factor(am)))

# H0: Automatique & Manual mpg are = 0
# H1: Automatique & Manual mpg are difference <> 0
trans_aut <- mtcars %>% filter(am==0) %>% select(mpg)
trans_man <- mtcars %>% filter(am==1) %>% select(mpg)
t.test(trans_aut,trans_man, alternative = 'two.sided')
#t.test(trans_aut$mpg,trans_man$mpg, paired = FALSE, alternative = 'two.sided')

# Select all continous variable (except AM)
mtcars_var <- mtcars %>% select(mpg,cyl,disp,hp,drat,wt,qsec,carb,am)
pairs(mtcars_var,panel = panel.smooth, main="Pair graph of selected* motor parmaters")

#Mean and SD can be calculated for each model Automatic and Manual
model_mean_sd <- mtcars %>% select(am,mpg) %>% group_by(am) %>% summarise(mean(mpg),sd(mpg))
model_mean_sd

# Manual Regression Model
summary(fit_1 <- lm(mpg ~.,data=mtcars))
summary(fit_2 <- lm(mpg ~ wt, data = mtcars))
summary(fit_3 <- lm(mpg ~ wt + qsec , data = mtcars))
summary(fit_4 <- lm(mpg ~ wt + qsec + hp , data = mtcars))
summary(fit_5 <- lm(mpg ~ wt + qsec + hp + factor(am), data = mtcars))
summary(fit_6 <- lm(mpg ~ wt + qsec + hp + factor(am) + factor(cyl), data = mtcars))
anova(fit_2,fit_3,fit_4,fit_5,fit_6)

summary(fit_7 <- lm(mpg ~ wt + qsec + factor(am), data = mtcars))
summary(fit_9 <- lm(formula = mpg ~ wt:factor(am) + qsec, data = mtcars))
summary(fit_10 <- lm(formula = mpg ~ wt + factor(am):qsec, data = mtcars))
summary(fit_final <- lm(formula = mpg ~ wt:factor(am) + qsec:factor(am), data = mtcars))


#Automated modeling
fit_all <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + factor(am) + gear + carb, mtcars)
fit_auto1_step <- step(fit_all, direction = "both", trace = 0, k = log(nrow(mtcars)))
summary(fit_auto_step)


fit_min <- lm(mpg ~ 1, data=mtcars)  # minimum possible regression model
fit_auto2_step <- step(fit_min, direction="both",
                       scope=(~ factor(am):(cyl+disp+hp+drat+wt+qsec+vs+gear+carb)))

#GLMulti automated modeling
fit_multi <- glmulti(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, fitfunction = lm, level = 1, method = "h", confsetsize = 1024)
fit_multi_final <- glmulti(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, fitfunction = lm, level = 1, method = "h", confsetsize = 1024)
print(fit_multi)
tmp <- weightable(fit_multi)
summary(fit_multi@objects[[1]])
plot(fit_multi, type = 's')
round(coef(fit_multi),4)

#All possible interaction between predictor
fit_multi_final <- glmulti(mpg ~ am + wt + qsec , fitfunction = lm, level = 2, method = "h")
print(fit_multi_final)
summary(fit_multi_final@objects[[1]])
plot(fit_multi_final, type = 's')
round(coef(fit_multi_final),4)

# Residual
fit_res <- rstandard(fit_final) # get standard residuals
par(mfrow=c(1,2))                  # two plots per line
par(mar=c(5.1,4.1,4.1,2.1))        # get back standard plot margin
plot(rstandard(fit_final),    # 1st plot standard residuals
     main="Standardized Residuals",
     ylab="Obs. - Est.")
abline(h=0, col="blue")            # add line in y = 0
qqnorm(fit_res)                    # 2nd plot normal qqplot
qqline(fit_res,col=2)


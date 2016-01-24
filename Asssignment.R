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
boxplot(mpg~am)
summary(lm(mpg~factor(am)))

# H0: Automatique & Manual mpg are = 0
# H1: Automatique & Manual mpg are difference <> 0
trans_aut <- mtcars %>% filter(am==0) %>% select(mpg)
trans_man <- mtcars %>% filter(am==1) %>% select(mpg)
t.test(trans_aut,trans_man, alternative = 'two.sided')
oneway.test(mpg~am)
#t.test(trans_aut$mpg,trans_man$mpg, paired = FALSE, alternative = 'two.sided')

# Select all continous variable (except AM)
mtcars_var <- mtcars %>% select(mpg,cyl,disp,hp,drat,wt,qsec,carb,am)
pairs(mtcars_var,panel = panel.smooth, main="Pair graph of selected* motor parmaters")

#Mean and SD can be calculated for each model Automatic and Manual
model_mean_sd <- mtcars %>% select(am,mpg) %>% group_by(am) %>% summarise(mean(mpg),sd(mpg))
model_mean_sd

# Manual Regression Model
fit_1 <- lm(mpg ~.,data=mtcars)
fit_2 <- update(fit_1, mpg ~ wt, data = mtcars)
fit_3 <- update(fit_2, mpg ~ wt + qsec , data = mtcars)
fit_4 <- update(fit_3, mpg ~ wt + qsec + hp , data = mtcars)
fit_5 <- update(fit_4, mpg ~ wt + qsec + hp + factor(am), data = mtcars)
fit_6 <- update(fit_5, mpg ~ wt + qsec + hp + factor(am) + factor(cyl), data = mtcars)
anova(fit_2,fit_3,fit_4,fit_5,fit_6)

fit_7 <- lm(mpg ~ wt + qsec + factor(am), data = mtcars)
fit_9 <- lm(formula = mpg ~ wt:factor(am) + qsec, data = mtcars)
fit_10 <- lm(formula = mpg ~ wt + factor(am):qsec, data = mtcars)
summary(fit_final <- lm(formula = mpg ~ wt:factor(am) + qsec:factor(am), data = mtcars))


#Automated modeling
#fit_all <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + factor(am) + gear + carb, mtcars)
#fit_auto1_step <- step(fit_all, direction = "both", trace = 0, k = log(nrow(mtcars)))
#summary(fit_auto_step)


#fit_min <- lm(mpg ~ 1, data=mtcars)  # minimum possible regression model
#fit_auto2_step <- step(fit_min, direction="both",
#                       scope=(~ factor(am):(cyl+disp+hp+drat+wt+qsec+vs+gear+carb)))

#GLMulti automated modeling
fit_auto <- glmulti(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, fitfunction = lm, level = 1, method = "h", confsetsize = 1024)
plot(fit_auto,type = "s")
tmp <- weightable(fit_auto)
summary(fit_auto@objects[[1]])
#round(coef(fit_auto),4)

#All possible interaction between predictor
fit_auto_final <- glmulti(mpg ~ am + wt + qsec , fitfunction = lm, level = 2, method = "h")
plot(fit_auto_final, type = 's')
#print(fit_auto_final)
summary(fit_auto_final@objects[[1]])
round(coef(fit_auto_final),4)

# Residual
fit_res <- rstandard(fit_final) # get standard residuals
par(mfrow=c(1,2))                  # two plots per line
par(mar=c(5.1,4.1,4.1,2.1))        # get back standard plot margin
plot(rstandard(fit_final),    # 1st plot standard residuals
     main="Standardized residuals",
     ylab="Observation")
abline(h=0, col="blue")            # add line in y = 0
qqnorm(fit_res)                    # 2nd plot normal qqplot
qqline(fit_res,col=2)

#Prediction
model_predict <- data_frame(prediction = predict(fit_final), transmission = mtcars$am)
arrange(model_predict,desc(prediction))
model_predict %>% group_by(transmission) %>% summarize(length(transmission))



### import packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(QuantPsyc)
library(car)
library(TSA)
library(olsrr)
library(dplyr)
library(stringr)
library(GGally)

#import data 
concrete <- read.table("Concrete_Data.csv", sep = ",", col.names = c("cement", "slag", "flyash", "water", "super", "coarseagg", "fineagg", "age", "compressive"),header = FALSE, skip = 1)
concrete <- concrete %>% filter(age == 28)
head(concrete)
concrete <- concrete[,-8]
summary(concrete)
head(concrete)
dim(concrete)

#plot histogram of compressive strength
hist(concrete$compressive)

#plot response vs super
ggplot(concrete, aes(x = super, y = compressive)) + geom_point()

# explore interaction between super and cement, flyash, slag on compressive strength
concrete$cement_binned <- cut(concrete$cement, 4)
ggplot(concrete, aes(x = super, y = compressive, color = cement_binned, group = cement_binned)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(colour = "Cement Content Levels (kg/m^3)", x = "Superplasticizer (kg/m^3)", y = "Compressive Strength (MPa)", title = "Concrete Compressive Strength vs Superplasticizer by Cement Content Levels") + theme(legend.position = "bottom")
concrete$flyash_binned <-cut(concrete$flyash, 4)
ggplot(concrete, aes(x = super, y = compressive, color = flyash_binned, group = flyash_binned)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(colour = "Fly Ash Content Levels (kg/m^3)", x = "Superplasticizer (kg/m^3)", y = "Compressive Strength (MPa)", title = "Concrete Compressive Strength vs Superplasticizer by Fly Ash Levels") + theme(legend.position = "bottom")
concrete$slag_binned <- cut(concrete$slag, 4)
ggplot(concrete, aes(x = super, y = compressive, color = slag_binned, group = slag_binned)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(colour = "Blast Furnace Slag Content Levels (kg/m^3)", x = "Superplasticizer (kg/m^3)", y = "Compressive Strength (MPa)", title = "Concrete Compressive Strength vs Superplasticizer by Blast Furnace Slag Levels") + theme(legend.position = "bottom")

# try z-normalisation
concrete[, c(1,2,3,4,5,6,7)] <- scale(concrete[, c(1,2,3,4,5,6,7)], center = TRUE, scale = FALSE)
head(concrete)
class(concrete)
concrete <- as.data.frame(concrete)

# try transforming response variable 
concrete$compressive = concrete$compressive^(1/2)
hist(concrete$compressive)
head(concrete)

# specifying model_1
model_1 <- lm(compressive ~ super + cement + flyash + slag + super:cement + super:flyash + super:slag, data = concrete)
summary(model_1)

#residual plots
plot(model_1)

#checking multicollinearity
vif(model_1)

# checking constant variance
ncvTest(model_1)

# checking autocorrelation
acf(model_1$residuals)
durbinWatsonTest(model_1)

#test for normally distributed errors
shapiro.test(model_1$residuals)
hist(concrete$compressive)

# try cochrane-orcutt method
ncvTest(model_1)
acf(model_1$residuals)
durbinWatsonTest(model_1)
p = durbinWatsonTest(model_1)$r
print(p)

head(concrete)
for (i in 2:length(concrete$compressive)) 
  concrete$compressive[i-1] = concrete$compressive[i] - p*concrete$compressive[i-1]
  concrete$cement[i-1] = concrete$cement[i] - p*concrete$cement[i-1]
  concrete$water[i-1] = concrete$water[i] - p*concrete$water[i-1]
  concrete$super[i-1] = concrete$super[i] - p*concrete$super[i-1]
  concrete$flyash[i-1] = concrete$flyash[i] - p*concrete$flyash[i-1]
  concrete$slag[i-1] = concrete$slag[i] - p*concrete$slag[i-1]

  concrete <- concrete[-425,]

# run the model again after cochrane-orcutt method
model_1 = lm(compressive ~ super + cement + flyash + slag + super:cement + super:flyash + super:slag, data = concrete)
summary(model_1)

#checking multicollinearity again
vif(model_1)

# checking constant variance again
ncvTest(model_1)

# checking autocorrelation again
acf(model_1$residuals)
durbinWatsonTest(model_1)

#test for normally distributed errors again
shapiro.test(model_1$residuals)
hist(concrete$compressive)

#residual plots
model_1_aug <- augment(model_1)
head(model_1_aug)
ggplot(model_1_aug, aes(x = .resid^2, y = .fitted)) + geom_point()
concrete_diag <- cbind(concrete, model_1_aug)
qqPlot(model_1$residuals, dist = "norm")
qplot(concrete_diag$.resid, geom = "histogram")

# plotting distribution of variables
qplot(concrete$compressive, geom = "histogram")
qplot(concrete$cement, geom = "histogram")
qplot(concrete$super, geom = "histogram")
qplot(concrete$flyash, geom = "histogram")
qplot(concrete$slagh, geom = "histogram")

#all possible regressions
all_models<-ols_step_all_possible(model_1)
all_models
str(all_models)
all_models$predictors
plot(all_models)

# choosing models that contain super term
final_models <-  all_models[, c(3, 7, 8, 11)] %>% filter(str_detect(predictors, "\\bsuper\\b(?!:)"))
str(final_models)
colnames(final_models)

# arrange the top 8 models based on cp statistics
final_8 <- head(final_models %>% arrange(cp), 8)
final_8$p <- sapply(strsplit(final_8$predictors, " "), length)
final_8

# final three models chosen are
final_model_1 <- lm(compressive ~ super + cement + super:flyash + super:slag, data = concrete) 
ols_regress(compressive ~ super + cement + super:flyash + super:slag, data = concrete) 

final_model_2 <- lm(compressive ~ super + super:flyash + super:slag, data = concrete) 
ols_regress(compressive ~ super + super:flyash + super:slag, data = concrete) 

final_model_3 <- lm(compressive ~ super + slag + super:flyash + super:slag, data = concrete) 
ols_regress(compressive ~ super + slag + super:flyash + super:slag, data = concrete) 

#residual analysis of final_model_1
par(mfrow = c(2,2))
plot(final_model_1)

  #checking multicollinearity
  vif(final_model_1)

  # checking constant variance
  ncvTest(final_model_1)

  # checking autocorrelation
  acf(final_model_1$residuals)
  durbinWatsonTest(final_model_1)

  #test for normally distributed errors
  shapiro.test(final_model_1$residuals)
  hist(concrete$compressive)

#residual analysis of final_model_2
plot(final_model_2)

  #checking multicollinearity
  vif(final_model_2)

  # checking constant variance
  ncvTest(final_model_2)

  # checking autocorrelation
  acf(final_model_2$residuals)
  durbinWatsonTest(final_model_2)

  #test for normally distributed errors
  shapiro.test(final_model_2$residuals)
  hist(concrete$compressive)

#residual analysis of final_model_3
plot(final_model_3)

  #checking multicollinearity
  vif(final_model_3)

  #checking constant variance
  ncvTest(final_model_3)

  #checking autocorrelation
  acf(final_model_3$residuals)
  durbinWatsonTest(final_model_3)

  #test for normally distributed errors
  shapiro.test(final_model_3$residuals)
  hist(concrete$compressive)

# summary of the best model
summary(final_model_1)

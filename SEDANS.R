pacman::p_load(tidyverse, caret, corrplot, broom, ggpubr, MASS,relaimpo, car, e1071,interplot)
setwd("~/Dropbox/EBAC/workshop-data/PA/data")
sedans <- read.csv("drive_time_sedans.csv")
head(sedans)
str(sedans)
summary(sedans)
colnames(sedans)
unique(sedans$data.set)
unique(sedans$vehicle.type)
unique(sedans$vehicle.age.group)
unique(sedans$color.set)
unique(sedans$makex)
unique(sedans$make.model)

# how is the Target variable distributed
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggpubr)
options(repr.plot.width=6, repr.plot.height=4)
theme_set(theme_bw() + theme(legend.position = "top"))
options(repr.plot.width=9, repr.plot.height=6)
p1 = sedans %>%
  ggplot(aes(y=lot.sale.days)) + geom_boxplot()
p2 = sedans %>%
  ggplot(aes(lot.sale.days)) + geom_histogram()
p3 = sedans %>%
  ggplot(aes(log(lot.sale.days))) + geom_histogram()
ggarrange(p1, p2, p3 + rremove("x.text"),
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
install.packages("propagate")
library(propagate)
skewness(sedans$lot.sale.days)
skewness(log(sedans$lot.sale.days), na.rm = TRUE)
any(sedans$lot.sale.days == 0)

# outlier values
outlier = sedans %>%
  filter(abs(lot.sale.days - median(lot.sale.days)) > 2*sd(lot.sale.days))
dim(outlier)

# approach: remove outlier values
sedans_new = sedans %>%
  filter(!abs(lot.sale.days - median(lot.sale.days)) > 2*sd(lot.sale.days))
dim(sedans)
dim(sedans_new)
skewness(sedans_new$lot.sale.days)

# how is my Target variable distributed now
options(repr.plot.width=6, repr.plot.height=4)
p4 = sedans_new %>%
  ggplot(aes(y=lot.sale.days)) + geom_boxplot()
p5 = sedans_new %>%
  ggplot(aes(lot.sale.days)) + geom_histogram()
ggarrange(p4, p5,
          labels = c("A", "B"),
          ncol = 2, nrow = 2)

# correlation plots
options(repr.plot.width=4, repr.plot.height=3)
library(corrplot)
cor(sedans_new[, sapply(sedans_new, is.numeric)], use="complete.obs")
corrplot(cor(sedans_new[, sapply(sedans_new, is.numeric)], use="complete.obs"),
         method = "number", type = 'lower')

# test the linearity with the continuous independent variables (IV)
# total.cost, mileage, vehicle.age
options(repr.plot.width=8, repr.plot.height=4)
p6 = sedans_new %>%
  ggplot(aes(x = total.cost, y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
p7 = sedans_new %>%
  ggplot(aes(x = mileage, y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
p8 = sedans_new %>%
  ggplot(aes(x = vehicle.age, y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
ggarrange(p6, p7, p8,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

# transformations - log 
options(repr.plot.width=8, repr.plot.height=4) 
p9 = sedans_new %>%
  ggplot(aes(x = log(mileage), y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
p10 = sedans_new %>%
  ggplot(aes(x = log(vehicle.age), y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
p11 = sedans_new %>%
  ggplot(aes(x = log(total.cost), y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
ggarrange(p9, p10, p11,
          labels = c("A","B","C"),
          ncol = 2, nrow = 2)

# transformations - sqrt
options(repr.plot.width=8, repr.plot.height=4) 
p12 = sedans_new %>%
  ggplot(aes(x = sqrt(mileage), y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
p13 = sedans_new %>%
  ggplot(aes(x = sqrt(vehicle.age), y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
p14 = sedans_new %>%
  ggplot(aes(x = sqrt(total.cost), y = lot.sale.days)) + geom_point() + geom_smooth(method = "loess")
ggarrange(p12, p13, p14,
          labels = c("A","B","C"),
          ncol = 2, nrow = 2)

# put the transformations in the data frame
cols_old = c('mileage','vehicle.age','total.cost')
cols_s = c('mileage_s','vehicle.age_s','total.cost_s')
cols_l = c('mileage_l','vehicle.age_l','total.cost_l')
sedans_new[,cols_s] = sapply(sedans_new[, cols_old], sqrt)
sedans_new[,cols_l] = sapply(sedans_new[, cols_old], log)

cor(sedans_new[, sapply(sedans_new, is.numeric)], use="complete.obs")

# trainset and choose state FL
train_fl = sedans_new %>% 
  filter(data.set == 'TRAIN' & state == 'FL')
drop <- c("data.set","state")
train_fl = train_fl[,!(names(train_fl) %in% drop)]

# initial model without the transformed variables
model1 = lm(lot.sale.days ~.-mileage_s-vehicle.age_s-total.cost_s-mileage_l-vehicle.age_l-total.cost_l-overage, data = train_fl)
summary(model1)

options(repr.plot.width=8, repr.plot.height=6)
par(mfrow = c(2, 2))
plot(model1)

model_stats = augment(model1)
head(model_stats)

# histogram of residuals
options(repr.plot.width=4, repr.plot.height=3)
ggplot(model_stats, aes(.resid)) + geom_histogram()

# more detailed importance values
imp = as.data.frame(varImp(model1))
imp = data.frame(overall = imp$Overall, names = rownames(imp))
imp[order(imp$overall,decreasing = T),]

# remove vehicle.age.group & make.model
model2 = lm(lot.sale.days ~.-mileage_s-vehicle.age_s-total.cost_s-mileage_l-vehicle.age_l-total.cost_l-overage-vehicle.age.group-make.model, data = train_fl)
summary(model2)

# relative importance of various predictors in predicting price
options(repr.plot.width=12, repr.plot.height=4)
plot(calc.relimp(model2,rela=TRUE))

# VIF
vif(model2)

# remove domestic.import
model3 = lm(lot.sale.days ~.-mileage_s-vehicle.age_s-total.cost_s-mileage_l-vehicle.age_l-total.cost_l-overage-vehicle.age.group-make.model-domestic.import, data = train_fl)
summary(model3)

# relative importance of various predictors in predicting price
options(repr.plot.width=12, repr.plot.height=4)
plot(calc.relimp(model2,rela=TRUE))

# VIF
vif(model3)

# remove vehicle.type
model4 = lm(lot.sale.days ~.-mileage_s-vehicle.age_s-total.cost_s-mileage_l-vehicle.age_l-total.cost_l-overage-vehicle.age.group-make.model-domestic.import-vehicle.type, data = train_fl)
summary(model4)

# relative importance of various predictors in predicting price
options(repr.plot.width=12, repr.plot.height=4)
plot(calc.relimp(model2,rela=TRUE))

# VIF
vif(model4)

options(repr.plot.width=8, repr.plot.height=6)
par(mfrow = c(2, 2))
plot(model4)

# include vehicle.type
model5 = lm(lot.sale.days ~.-mileage_s-vehicle.age_s-total.cost_s-mileage_l-vehicle.age_l-total.cost_l-overage-vehicle.age.group-make.model-domestic.import, data = train_fl)
summary(model5)

# interaction: 

# test the learned model on test data
test_set = sedans_new %>%
  filter(data.set == 'TEST' & state == 'FL' & make.model != 'CHEVROLET.METRO' & make.model !='MAZDA.929')
FL = sedans_new %>%
  filter(state == 'FL')
predictTest = predict(model5, newdata=test_set)
SSE = sum((test_set$lot.sale.days - predictTest)^2)
SST = sum((test_set$lot.sale.days - mean(FL$lot.sale.days))^2)
R_Sq= 1 - (SSE / SST)
round(R_Sq,4)

# predict on validate set
validate_set = sedans_new %>%
  filter(data.set == 'VALIDATE' & state == 'FL' & make.model != 'CHEVROLET.METRO' & make.model !='MAZDA.929' & make.model != 'CADILLAC.FLEETWOOD')
predictValidate = predict(model5, newdata = validate_set)
validate_set$lot.sale.days = predictValidate
summary(validate_set)

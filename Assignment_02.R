# Andrea Bruckner
# Predict 454
# Assignment 2

#######################################################
# Workspace Setup
#######################################################

# Reset plot display
par(mfrow=c(1,1))

# Install Packages if they don't current exist
list.of.packages <- c("doBy"
                      ,"lazyeval"
                      ,"psych"
                      ,"lars"
                      ,"GGally"
                      ,"ggplot2"
                      ,"grid"
                      ,"gridExtra"
                      ,"corrgram"
                      ,"corrplot"
                      ,"leaps"
                      ,"glmnet"
                      ,"MASS"
                      ,"gbm"
                      ,"tree"
                      ,"rpart"
                      ,"rpart.plot"
                      ,"rattle"
                      ,"gam"
                      ,"class"
                      ,"e1071"
                      ,"randomForest"
                      ,"doParallel"
                      ,"iterators"
                      ,"foreach"
                      ,"parallel"
                      ,"lattice"
                      ,"caret"
                      ,"plyr"
                      ,"dplyr"
                      ,"car")

#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

# Load all packages
lapply(list.of.packages, require, character.only = TRUE)

# Read data
data <- read.csv(file.path("/Users/annie/Desktop/Northwestern/PREDICT_454/Assignment_02","two_months_salary.csv"), sep=",",header=TRUE)
head(data)

#######################################################
# Data Quality Check
#######################################################

# Explore the data -- how big is it, what types of variables included, distributions and missing values.
class(data) # data.frame
dim(data) # 425   7
nrow(data) # 425 rows
ncol(data) # 7 variables
names(data)
str(data) # all int or numeric
summary(data) # no missing data

# Convert to numeric
data$price <- as.numeric(data$price)

# Convert to factor
data$color <- as.factor(data$color)
data$clarity <- as.factor(data$clarity)

summary(data$color) # 1 to 9
summary(data$clarity) # 2 to 10

# Code factor levels (per two_months_salary.pdf)
levels(data$color) <- c("D", "E", "F", "G", "H", "I", "J", "K", "L")
levels(data$clarity) <- c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1", "I2")

# Fix data values so they have no spaces
summary(data$cut)
levels(data$cut) <- c("Ideal", "Not_Ideal")

summary(data$store)
levels(data$store) <- c("Ashford", "Ausmans", "Blue_Nile", "Chalmers", 
                      "Danford", "Fred_Meyer", "Goodmans", "Kay", 
                      "R_Holland", "Riddles", "University", "Zales")
str(data)

# Create histograms for all predictor variables
for (i in 1:6){
  toPlot = paste0("price ~ ", names(data)[i])
  p <- histogram(as.formula(toPlot), data = data, col = "steelblue",  xlab = names(data)[i])
  print(p)
}

# Create boxplots for all predictor variables except carat
for (i in 2:6){
  toPlot = paste0("price ~ ", names(data)[i])
  p <- bwplot(as.formula(toPlot), data = data, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")),
    xlab = names(data)[i])
  print(p)
}

clarityb <- bwplot(price~clarity, data = data,
             par.settings = list(
               box.umbrella=list(col= "black"), 
               box.dot=list(col= "black"), 
               box.rectangle = list(col= "black", fill = "steelblue")),
             strip = strip.custom(bg="lightgrey"),
             xlab = "clarity")
clarityb

cutb <- bwplot(price~cut, data = data,
                   par.settings = list(
                     box.umbrella=list(col= "black"), 
                     box.dot=list(col= "black"), 
                     box.rectangle = list(col= "black", fill = "steelblue")),
                   strip = strip.custom(bg="lightgrey"),
               xlab = "cut")
cutb

channelb <- bwplot(price~channel, data = data,
                   par.settings = list(
                     box.umbrella=list(col= "black"), 
                     box.dot=list(col= "black"), 
                     box.rectangle = list(col= "black", fill = "steelblue")),
                   strip = strip.custom(bg="lightgrey"),
                   xlab = "channel")
channelb

grid.arrange(clarityb, cutb, channelb, ncol=3)

#######################################################
# EDA
#######################################################

# Disable scientific notation for labeling plots
# options(scipen = 10)

# Create tree plot -- How to plot without scientific notation?
fancyRpartPlot(rpart(price ~ ., data = data), sub = "")

# Create scatter plot for price~carat
xyplot(price~carat, data = data, col = "steelblue")

# Calculate correlation between price and carat
cor(data$price, data$carat) # 0.8796315

# Examine quantiles of carat
quantile((data$carat), probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))

# Plot carat
ch <- histogram(~carat, data = data,
                col = "steelblue", strip = strip.custom(bg="lightgrey"))
ch
cb <- bwplot(~carat, data = data,
             par.settings = list(
               box.umbrella=list(col= "black"), 
               box.dot=list(col= "black"), 
               box.rectangle = list(col= "black", fill = "steelblue")),
             strip = strip.custom(bg="lightgrey"))
cb

cq <- qqmath(~carat, data = data, col = "steelblue")
cq

grid.arrange(ch, cb, cq, ncol=3)

# Create plots of price by factor predictor variables
# Plots function for color, clarity, cut, and store

## NEED TO MAKE FUNCTION THAT PLOTS BOTH TYPES OF PLOT
myPlots <- function(variable){
h1 <- histogram(~price | variable, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"),
          main = variable)
b1 <- bwplot(~price | variable, data = data,
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"),
       main = variable)

grid.arrange(h1, b1, ncol=2)
}

lapply(data[c(2:4,6)],FUN=myPlots)

# Plot channel separately to specify layout
h2 <- histogram(~price | channel, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"),
          layout = c(3, 1))
b2 <- bwplot(~price | channel, data = data,
       layout = c(3, 1),
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

grid.arrange(h2, b2, ncol=2)

# Examine mean price given various predictors
mean(data$price[data$carat>1]) # 8568.149
mean(data$price[data$carat<1]) # 3038.145
mean(data$price[data$carat>2]) # 15167.07

mean(data$price[data$color=="D"]) # 7378.833
mean(data$price[data$color=="E"]) # 6389.983
mean(data$price[data$color=="L"]) # 3349.2

mean(data$price[data$cut=="Ideal"]) # 5767.357
mean(data$price[data$cut=="Not_Ideal"]) # 6690.494

mean(data$price[data$channel=="Internet"]) # 6721.05
mean(data$price[data$channel=="Mall"]) # 4848.492
mean(data$price[data$channel=="Independent"]) # 5790.458

mean(data$price[data$store=="Kay"]) # 6274.071
mean(data$price[data$store=="Fred_Meyer"]) # 5318.933

mean(data$price[data$clarity=="IF"]) # 5608.909
mean(data$price[data$clarity=="I2"]) # 2949.5

mean(data$price[data$clarity=="IF" & data$color=="D"]) # 15909

#######################################################
# Model Build -- Exploration Part 1
#######################################################

# Create train/test sets (70/30)
dim(data) # 425: 0.70*425 = 297.5 (train should have 297 or 298 observations)

set.seed(123)
train <- sample_frac(data, 0.70)
train_id <- as.numeric(rownames(train)) 
test <- data[-train_id,]

head(train)
dim(train) # 298   7
head(test)
dim(test) # 127   7

# Fit model with all predictors to have a baseline
model.all <- regsubsets(price ~ ., data = train, nvmax = 29)
reg.summary <- summary(model.all)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2

which.max(reg.summary$adjr2) # 23: adjr2 = 0.9149147
which.min(reg.summary$cp) # 23: cp = 17.69444
which.min(reg.summary$bic) # 18: bic = -629.6917

model.all.lm <- lm(price ~ ., data = train)
model.all.lm.summary <- summary(model.all.lm)
names(model.all.lm.summary)
model.all.lm.summary$adj.r.squared # 0.9135605

vif(model.all.lm) # won't calculate because "there are aliased coefficients in the model"
alias(model.all.lm)
# store$University and store$Zales are aliased--maybe good to leave them out, or maybe store entirely

# Fit a naïve regression model using backwards variable selection (nvmax = 29 to capture all factor levels)
model.1.bwd <- regsubsets(price ~ ., data = train, nvmax = 29, method="backward")
summary(model.1.bwd) # the first level of each factorized variable doesn't appear in the output
# store produces this warning:
# In leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in,  :
# 2  linear dependencies found
# I found this out by adding in each of the predictors separately
# model.1.bwd <- regsubsets(price ~ carat + color + clarity + cut + channel + store, data = train, nvmax=6, method="backward")

reg.summary.bwd <- summary(model.1.bwd)
names(reg.summary.bwd)
reg.summary.bwd$rsq
reg.summary.bwd$adjr2

# Why did setting method="backward" cause the best models based on adjr2 and cp to change?
which.max(reg.summary.bwd$adjr2) # 24: adjr2 = 0.9146926
which.min(reg.summary.bwd$cp) # 22: cp = 18.46462
which.min(reg.summary.bwd$bic) # 18: bic = -629.6917
# 22 variables is good compromise based on metrics

# Create naïve model using top 5 predictors
m1 <- lm(price ~ carat + (color=="I") + (color=="J") + (color=="K") + (color=="L"), data = train)
# + (channel=="Mall") + (store=="Goodmans") + (clarity=="SI2") + (color=="H") + (clarity=="SI1")
#resid(m1) # list of residuals
par(mfrow=c(1,1))
plot(density(resid(m1)))
par(mfrow=c(1,4))
plot(m1)
par(mfrow=c(1,1))
AIC(m1) # 5376.918
vif(m1) # all close to 1 - GOOD

# Should you make transformations to the response variable or some predictor variables?
par(mfrow=c(2,2))
hist(data$price) # right-skewed
hist(train$price) # right-skewed
hist(data$carat)
hist(train$carat)
# The distributions appear similar regardless of full or training data set

# Transform price
par(mfrow=c(1,1))
log_price_d <- log(data$price)
hist(log_price_d)
log_price <- log(train$price)
hist(log_price)
#log_carat <- log(train$carat) # doesn't help
#hist(log_carat)

# Plot price and log_price
ph <- histogram(~price, data = train,
                col = "steelblue", strip = strip.custom(bg="lightgrey"))
ph_log <- histogram(~log(price), data = train,
                col = "steelblue", strip = strip.custom(bg="lightgrey"))

grid.arrange(ph, ph_log, ncol=2)

# Create log naïve model using top 5 predictors
m1_log <- lm(log(price) ~ carat + (color=="I") + (color=="J") + (color=="K") + (color=="L"), data = train)
# + (channel=="Mall") + (store=="Goodmans") + (clarity=="SI2") + (color=="H") + (clarity=="SI1")
#resid(m1) # list of residuals
plot(density(resid(m1_log)))
par(mfrow=c(1,4))
plot(m1_log)
par(mfrow=c(1,1))
AIC(m1_log) # 117.9585 -- can't compare AICs between log and nonlog models 
vif(m1_log) # same as m1

# Compare residual plots of m1 and m1_log -- the log transformation makes it a little better?
par(mfrow=c(2,4))
plot(m1)
plot(m1_log)
par(mfrow=c(1,1))

# Log backward variable selection (nvmax = 29 to capture all factor levels)
model.2.bwd <- regsubsets(log(price) ~ ., data = train, nvmax = 29, method="backward") # must list predictors so price isn't included
summary(model.2.bwd) # the first level of each factorized variable doesn't appear in the output
# store still products 2  linear dependencies found
# The top 5 variables change

reg.summary.bwd.2 <- summary(model.2.bwd)
names(reg.summary.bwd.2)
reg.summary.bwd.2$rsq
reg.summary.bwd.2$adjr2

# Why did setting method="backward" cause the best models based on adjr2 and cp to change?
which.max(reg.summary.bwd.2$adjr2) # 23
which.min(reg.summary.bwd.2$cp) # 19 
which.min(reg.summary.bwd.2$bic) # 16 
reg.summary.bwd.2$adjr2[23] # 0.8935387 # worse
reg.summary.bwd.2$cp[19] # 15.58155 # better
reg.summary.bwd.2$bic[16] # -576.2245 # worse
# 19 variables is good compromise based on metrics

# Comparing means of metrics show that log is way better in terms of cp but similar in adjr2 and bic
mean(reg.summary.bwd$adjr2) # 0.8846959
mean(reg.summary.bwd$cp) # 110.6188
mean(reg.summary.bwd$bic) # -582.26
mean(reg.summary.bwd.2$adjr2) # 0.8750849
mean(reg.summary.bwd.2$cp) # 58.17066
mean(reg.summary.bwd.2$bic) # -549.3467

#######################################################
# Model Build -- Exploration Part 2 Using log(price)
#######################################################

# Create tree plot - top number in each leaf shows log(price)
fancyRpartPlot(rpart(log(price) ~ ., data = train), sub = "") # must list predictors so price isn't included

# Backward Selection (copied from above section)
set.seed(123)
model.2.bwd <- regsubsets(log(price) ~ ., data = train, nvmax = 29, method="backward")
summary(model.2.bwd) # the first level of each factorized variable doesn't appear in the output

reg.summary.bwd.2 <- summary(model.2.bwd)
names(reg.summary.bwd.2)
reg.summary.bwd.2$rsq
reg.summary.bwd.2$adjr2

which.max(reg.summary.bwd.2$adjr2) # 23
which.min(reg.summary.bwd.2$cp) # 19 
which.min(reg.summary.bwd.2$bic) # 16 
reg.summary.bwd.2$adjr2[23] # 0.8935387 # worse
reg.summary.bwd.2$cp[19] # 15.58155 # better
reg.summary.bwd.2$bic[16] # -576.2245 # worse

# Forward Selection
set.seed(123)
model.fwd <- regsubsets(log(price) ~ ., data = train, nvmax = 29, method="forward") # must list predictors so price isn't included
summary(model.fwd) # the first level of each factorized variable doesn't appear in the output

reg.summary.fwd <- summary(model.fwd)
names(reg.summary.fwd)
reg.summary.fwd$rsq
reg.summary.fwd$adjr2

which.max(reg.summary.fwd$adjr2) # 23
which.min(reg.summary.fwd$cp) # 22 
which.min(reg.summary.fwd$bic) # 10 
reg.summary.fwd$adjr2[23] # 0.8935387
reg.summary.fwd$cp[22] # 16.79064
reg.summary.fwd$bic[10] # -575.4802

# Stepwise Selection
set.seed(123)
model.stepwise <- regsubsets(log(price) ~ ., data = train, nvmax = 26, method="seqrep") # nvmax > 26 causes R Studio session to crash
summary(model.stepwise) # the first level of each factorized variable doesn't appear in the output

reg.summary.stepwise <- summary(model.stepwise)
names(reg.summary.stepwise)
reg.summary.stepwise$rsq
reg.summary.stepwise$adjr2

which.max(reg.summary.stepwise$adjr2) # 22 
which.min(reg.summary.stepwise$cp) # 19 
which.min(reg.summary.stepwise$bic) # 14
reg.summary.stepwise$adjr2[22] # 0.8934076
reg.summary.stepwise$cp[19] # 15.58155
reg.summary.stepwise$bic[14] # -578.4617

# All Subsets Selection
set.seed(123)
model.allsub <- regsubsets(log(price) ~ ., data = train, nvmax = 29, method="exhaustive")
summary(model.allsub) # the first level of each factorized variable doesn't appear in the output

reg.summary.allsub <- summary(model.allsub)
names(reg.summary.allsub)
reg.summary.allsub$rsq
reg.summary.allsub$adjr2

which.max(reg.summary.allsub$adjr2) # 23
which.min(reg.summary.allsub$cp) # 19
which.min(reg.summary.allsub$bic) # 14
reg.summary.allsub$adjr2[23] # 0.8935387
reg.summary.allsub$cp[19] # 15.58155
reg.summary.allsub$bic[14] # -578.4617

# LASSO Model
# Set up grid and data matrix for lasso model
grid <- 10^seq(10, -2, length=100)
set.seed(123)
train.matrix  <- model.matrix(log(price) ~ ., data=train)[,-1]
ncol(train.matrix) # 31 -- first level of each factor is missing
head(train.matrix)

model.lasso <- glmnet(train.matrix, log(price), alpha=1, lambda=grid)

# Use cross-validation to select lambda.
set.seed(123)
cv.out.lasso <- cv.glmnet(train.matrix, log_price, alpha=1)
plot(cv.out.lasso) # 12 predictors ideal

bestlamlasso <- cv.out.lasso$lambda.min
bestlamlasso # 0.000288169

coef(model.lasso, s=bestlamlasso)
#varImp(model.lasso, lambda = bestlamlasso)

#######################################################
# Model Build -- Fit Model Suite
#######################################################

# Although Backward, Stepwise, and All Subset Selection chose the same 19 variable model,
# I will use the 14 variable model that both Stepwise and All Subset chose since it is simpler.
# carat + channelMall + clarityI1 + claritySI2 + clarityVVS2 + colorF + colorG + colorH + colorI + colorJ + colorK + colorL + storeAusmans + storeGoodmans

# (1) Create a linear regression model with no interactions using the lm() function
set.seed(123)
fit1 <- lm(log(price) ~ carat + (channel=="Mall") + (clarity=="I1") + (clarity=="SI2") + 
             (clarity=="VVS2") + (color=="F") + (color=="G") + (color=="H") + (color=="I") + 
             (color=="J") + (color=="K") + (color=="L") + (store=="Ausmans") + (store=="Goodmans"),
           data = train)

summary(fit1) # Adjusted R-squared:  0.8869
AIC(fit1) # 21.70701
vif(fit1) # all less than 2
fit1.train.exp <- exp(fit1$fitted.values)
head(fit1.train.exp)
head(train$price)

par(mfrow=c(1,4))
plot(fit1)
par(mfrow=c(1,1))

# (2) Create a linear regression model including some interaction terms

# Stepwise Selection with Interaction (All Subset Selection has too long of processing time)
set.seed(123)
model.stepwise.I <- regsubsets(log(price) ~ .*., data = train, method="seqrep") # use all possible interactions
summary(model.stepwise.I) # the first level of each factorized variable doesn't appear in the output

# In order of stepwise variable selection
# carat
# clarityI1:storeRiddles
# colorI:channelInternet
# carat:colorK
# carat:colorJ
# storeGoodmans
# clarityVS2:channelMall
# carat:cutNot_Ideal

fit2 <- lm(log(price) ~ carat + (clarity=="I1")*(store=="Riddles") + (color=="I")*(channel=="Internet") + carat*(color=="K") +
             carat*(color=="J") + (store=="Goodmans") + (clarity=="VS2")*(channel=="Mall") + carat*(cut=="Not_Ideal"),
           data = train)

summary(fit2) # Adjusted R-squared:  0.8939
AIC(fit2) # 5.50042
vif(fit2) # range from 1.25 to 14.5
fit2.train.exp <- exp(fit2$fitted.values)
head(fit2.train.exp)
head(train$price)

par(mfrow=c(1,4))
plot(fit2) # Warning: In sqrt(crit * p * (1 - hh)/hh) : NaNs produced
par(mfrow=c(1,1))

# (3) Create a tree model
set.seed(123)
fit3 <- rpart(log(price) ~ ., data = train)
fit3
# fit3.train.exp <- exp(fit3$y)
# head(fit3.train.exp) # same as train$price
# head(train$price) 
fit3_plot <- fancyRpartPlot(rpart(log(price) ~ ., data = train), sub = "") # must list predictors so price isn't included
fit3_plot

### DELETE?
# ("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1", "I2")
set.seed(123)
fit3sub <- rpart(log(price) ~ carat + (clarity=="VVS1") + (clarity=="VVS2") + (clarity=="VS2") + (clarity=="SI1") + (clarity=="SI2") + (clarity=="I1") + (clarity=="I2"), data = train)
fit3sub
fit3sub <- rpart(log(price) ~ carat, data = train)
fit3sub

fit3.train.exp <- exp(fit3sub$y)
head(fit3.train.exp) # same as train$price
head(train$price) 
fit3_plot <- fancyRpartPlot(rpart(log(price) ~ ., data = train), sub = "") # must list predictors so price isn't included
fit3_plot
###

# (4) Create a Random Forest model
set.seed(123)
# mtry can be 31 at most because of number of columns in train.matrix
fit4_baseline <- randomForest(train.matrix, log_price, mtry=floor(sqrt(ncol(train.matrix))), importance=TRUE)
fit4_baseline
# Number of trees: 500
# No. of variables tried at each split: 5
# Mean of squared residuals: 0.1239849
# % Var explained: 76.38

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(123)
mtry <- sqrt(ncol(train.matrix)) # 5.567764
rf_random <- train(train.matrix, log_price, method="rf", metric="RMSE", tuneLength=15, trControl=control)
print(rf_random)
# 298 samples
# 6 predictor

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 267, 267, 268, 267, 268, 269, ... 
# Resampling results across tuning parameters:
  
#   mtry  RMSE       Rsquared 
# 2    0.5517078  0.6860846
# 5    0.3583647  0.8508695
# 10    0.2376679  0.9077693
# 12    0.2219911  0.9137399
# 13    0.2164336  0.9161902
# 14    0.2125060  0.9177249
# 16    0.2089451  0.9185207
# 18    0.2074204  0.9183267
# 20    0.2080204  0.9170165
# 21    0.2090051  0.9161637
# 30    0.2192674  0.9078217

# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was mtry = 18.
plot(rf_random)
set.seed(123)
fit4 <- randomForest(log(train$price) ~ ., data = train.matrix, mtry=18, importance=TRUE)
fit4

# Number of trees: 500
# No. of variables tried at each split: 18
# Mean of squared residuals: 0.04484394
# % Var explained: 91.46

# sqrt(0.04484394) # 0.2117639 -- Why doesn't this match the RMSE calculated in rf_random?

names(fit4)
fit4.train.exp <- exp(fit4$predicted)
head(fit4.train.exp)

plot(fit4, main = "Random Forest Plot")
importance(fit4) # the higher number, the more important for %IncMSE
varImpPlot(fit4, main = "Random Forest Model: \n Variable Importance") # How to do in Lattice?

#######################################################
# Model Comparison
#######################################################

# Prediction Functions
# https://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/
rmse <- function(error){
  sqrt(mean(error^2))
}

mae <- function(error){
  mean(abs(error))
}

#######
# fit1
#######

# Calculate fit1 test predictions
set.seed(123)
fit1.pred <- predict(fit1, newdata = test)
fit1.pred.exp <- exp(fit1.pred) # returns log(price) to price for interpretability
head(fit1.pred.exp)

# Calculate fit1 test errors
actual <- test$price
predicted <- fit1.pred.exp
error <- actual - predicted
fit1.test.rmse <- rmse(error)
fit1.test.rmse # 1790.202
mae(error) # 940.274

# Calculate fit1 train predictions
set.seed(123)
fit1.train <- predict(fit1, newdata = train)
fit1.train.exp <- exp(fit1.train) # returns log(price) to price for interpretability
head(fit1.train.exp)

# Calculate fit1 train errors
actual <- train$price
predicted <- fit1.train.exp
error <- actual - predicted
fit1.train.rmse <- rmse(error)
fit1.train.rmse # 2616.139
mae(error) # 1263.496

# Long way of doing the test prediction functions (function check)
MSE <- mean((test$price - fit1.pred.exp)^2)
RMSE <- sqrt(MSE)
RMSE # 1790.202
MAE <- mean(abs(test$price - fit1.pred.exp))
MAE # 940.274

# Does predict() values match just changing data = test in lm() for fit1? -- NO -- Why?
set.seed(123)
fit1.test <- lm(log(price) ~ carat + (channel=="Mall") + (clarity=="I1") + (clarity=="SI2") + 
                  (clarity=="VVS2") + (color=="F") + (color=="G") + (color=="H") + (color=="I") + 
                  (color=="J") + (color=="K") + (color=="L") + (store=="Ausmans") + (store=="Goodmans"),
                data = test)

fit1.test.exp <- exp(fit1.test$fitted.values)
head(fit1.test.exp)
head(fit1.pred.exp)
head(test$price)

actual <- test$price
predicted <- fit1.test.exp

# Calculate test error
error <- actual - predicted

# Calculate fit1.test errors
rmse(error) # 2216.292
mae(error) # 941.8554

#######
# fit2
#######

# Calculate fit2 test predictions
set.seed(123)
fit2.pred <- predict(fit2, newdata = test)
fit2.pred.exp <- exp(fit2.pred) # returns log(price) to price for interpretability

# Calculate fit2 test errors
actual <- test$price
predicted <- fit2.pred.exp
error <- actual - predicted
fit2.test.rmse <- rmse(error)
fit2.test.rmse # 1383.583
mae(error) # 955.6749

# Calculate fit1 train predictions
set.seed(123)
fit2.train <- predict(fit2, newdata = train)
fit2.train.exp <- exp(fit2.train) # returns log(price) to price for interpretability
head(fit2.train.exp)

# Calculate fit2 train errors
actual <- train$price
predicted <- fit2.train.exp
error <- actual - predicted
fit2.train.rmse <- rmse(error)
fit2.train.rmse # 2353.941
mae(error) # 1198.39

# Does predict() values match just changing data = test in lm() for fit1? -- NO -- Why?
fit2.test <- lm(log(price) ~ carat + (clarity=="I1")*(store=="Riddles") + (color=="I")*(channel=="Internet") + carat*(color=="K") +
             carat*(color=="J") + (store=="Goodmans") + (clarity=="VS2")*(channel=="Mall") + carat*(cut=="Not_Ideal"),
           data = test)

fit2.test.exp <- exp(fit2.test$fitted.values)
head(fit2.test.exp)
head(fit2.pred.exp)
head(train$price)

actual <- test$price
predicted <- fit2.test.exp

# Calculate test error
error <- actual - predicted

# Calculate fit2.test errors
rmse(error) # 1268.552
mae(error) # 837.5475

#######
# fit3
#######

# Calculate fit3 test predictions
set.seed(123)
fit3.pred <- predict(fit3, newdata = test)
fit3.pred.exp <- exp(fit3.pred) # returns log(price) to price for interpretability

# Calculate fit3 test errors
actual <- test$price
predicted <- fit3.pred.exp
error <- actual - predicted
fit3.pred.rmse <- rmse(error)
fit3.pred.rmse # 1478.038
mae(error) # 956.3601

# Calculate fit3 train predictions
set.seed(123)
fit3.train <- predict(fit3, newdata = train)
fit3.train.exp <- exp(fit3.train) # returns log(price) to price for interpretability
head(fit3.train.exp)

# Calculate fit3 train errors
actual <- train$price
predicted <- fit3.train.exp
error <- actual - predicted
fit3.train.rmse <- rmse(error)
fit3.train.rmse # 1859.317
mae(error) # 1150.792

#######
# fit4
#######

# For fit4 test, must create test.matrix
set.seed(123)
test.matrix <- model.matrix(log(price) ~ ., data=test)[,-1]
ncol(test.matrix) # 31 -- first level of each factor is missing
head(test.matrix)

# Calculate fit4 test predictions
set.seed(123)
fit4.pred <- predict(fit4, newdata = test.matrix)
fit4.pred.exp <- exp(fit4.pred) # returns log_price to price for interpretability
head(fit4.pred.exp)

# Calculate fit4 test errors
actual <- test$price
predicted <- fit4.pred.exp
error <- actual - predicted
fit4.pred.rmse <- rmse(error)
fit4.pred.rmse # 1001.25
mae(error) # 634.5892

# Calculate fit4 train predictions
set.seed(123)
fit4.train <- predict(fit4, newdata = train.matrix)
fit4.train.exp <- exp(fit4.train) # returns log(price) to price for interpretability
head(fit4.train.exp)

# Calculate fit4 train errors
actual <- train$price
predicted <- fit4.train.exp
error <- actual - predicted
fit4.train.rmse <- rmse(error)
fit4.train.rmse # 1582.471
mae(error) # 866.4515








### Discarded Code ###

trainIndex <- createDataPartition(data$price, p = 0.70, list = F)
head(trainIndex)

train <- data[ trainIndex,]
test  <- data[-trainIndex,]

summary(model.lda.bwd)

predictors <- data[1:6]
predictors.train <- train[1:6]
class(predictors.train)

train.matrix <- model.matrix(~ ., data=train, 
                             contrasts.arg=list(color=diag(nlevels(train$color)), 
                                                clarity=diag(nlevels(train$clarity)),
                                                cut=diag(nlevels(train$cut)),
                                                channel=diag(nlevels(train$channel)),
                                                store=diag(nlevels(train$store))))

varImp(model.lasso, lambda = bestlamlasso)

mat.train <- data.matrix(train)
mat.train <- mat.train[,-7]
# Remove price to so that the response is not on both sides of equation

model.lasso <- glmnet(mat.train, log_price, alpha=1, lambda=grid)

# Use cross-validation to select lambda.
set.seed(123)
cv.out.lasso <- cv.glmnet(mat.train, log_price, alpha=1)
plot(cv.out.lasso)

bestlamlasso <- cv.out.lasso$lambda.min
bestlamlasso # 0.002448726

coef(model.lasso, s=bestlamlasso)
varImp(model.lasso, lambda = bestlamlasso)

###

MPE <- mean((test$price - fit1.pred.exp)^2)
StandardError <- sd((test$price - fit1.pred.exp)^2)/sqrt(n.test)
StandardError # 7974789

n.test <- length(test)

#fit4 <- randomForest(log(price) ~ ., data = train, mtry=6, importance = TRUE)
#fit4 <- randomForest(log(price) ~ ., data = train, mtry=6, importance = TRUE)

fit3 <- fancyRpartPlot(rpart(log_price ~ carat + color + clarity + cut + channel + store, data = train), sub = "") # must list predictors so price isn't included
fit3

Y <- log_price
train.matrix <- model.matrix(~ ., data=train, 
                             contrasts.arg=list(color=contrasts(train$color, contrasts = F),
                                                clarity=contrasts(train$clarity, contrasts = F), 
                                                cut=contrasts(train$cut, contrasts = F), 
                                                channel=contrasts(train$channel, contrasts = F),
                                                store=contrasts(train$store, contrasts = F)))
# Code Reference: http://stackoverflow.com/questions/4560459/all-levels-of-a-factor-in-a-model-matrix-in-r
train.matrix <- train.matrix[,-1] # remove intercept
train.matrix <- train.matrix[,-38] # remove price since log_price is response

# rf_random <- train(log(price) ~ ., data=train, method="rf", metric="RMSE", tuneLength=15, trControl=control)
# print(rf_random)

fit4 <- randomForest(train.matrix, log(train$price), mtry=18, importance=TRUE)
fit4


rf_random <- train(log(price) ~ ., data=train, method="rf", metric="RMSE", tuneLength=15, trControl=control)
#


fit4.test <- randomForest(test.matrix, log(test$price), mtry=18, importance=TRUE)
fit4.test

set.seed(123)
fit4.test1 <- randomForest(log(test$price) ~ ., data = test.matrix, mtry=18, importance=TRUE)
fit4.test1.exp <- exp(fit4.test1$y)
head(fit4.test1.exp)
head(test$price)


set.seed(123)
fit3a <- tree(log(price) ~ ., data = train)
fit3a
summary(fit3)
summary(fit3a)

# Create PCA model
model.pca <- prcomp(log(price) ~ ., data = data, scale = T) # prcomp is preferred to princomp for accuracy
summary(model.pca)
par(mfrow=c(1,2))
screeplot(model.pca, type = c("lines"), main = "PCA Model", sub = "Number of Components") # 4 components explain most of variability in the data
biplot(model.pca, xlabs = wine[, "Class"], xlim=c(-0.20, 0.20))


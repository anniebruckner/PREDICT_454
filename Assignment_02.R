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

#######################################################
# EDA
#######################################################

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
          col = "steelblue", strip = strip.custom(bg="lightgrey"))
b1 <- bwplot(~price | variable, data = data,
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

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
#resid(m1) # list of residuals
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
ph_log <- histogram(~log_price, data = train,
                col = "steelblue", strip = strip.custom(bg="lightgrey"))

grid.arrange(ph, ph_log, ncol=2)

# Create log naïve model using top 5 predictors
m1_log <- lm(log_price ~ carat + (color=="I") + (color=="J") + (color=="K") + (color=="L"), data = train)
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
model.2.bwd <- regsubsets(log_price ~ carat + color + clarity + cut + channel + store, data = train, nvmax = 29, method="backward") # must list predictors so price isn't included
summary(model.2.bwd) # the first level of each factorized variable doesn't appear in the output
# store still products 2  linear dependencies found
# The top 5 variables change

reg.summary.bwd.2 <- summary(model.2.bwd)
names(reg.summary.bwd.2)
reg.summary.bwd.2$rsq
reg.summary.bwd.2$adjr2

# Why did setting method="backward" cause the best models based on adjr2 and cp to change?
which.max(reg.summary.bwd.2$adjr2) # 23: adjr2 = 0.8935387 # worse
which.min(reg.summary.bwd.2$cp) # 19: cp = 15.58155 # better
which.min(reg.summary.bwd.2$bic) # 16: bic = -576.2245 # worse
# 19 variables is good compromise based on metrics

# Comparing means of metrics show that log is way better in terms of cp but similar in adjr2 and bic
mean(reg.summary.bwd$adjr2) # 0.8846959
mean(reg.summary.bwd$cp) # 110.6188
mean(reg.summary.bwd$bic) # -582.26
mean(reg.summary.bwd.2$adjr2) # 0.8750849
mean(reg.summary.bwd.2$cp) # 58.17066
mean(reg.summary.bwd.2$bic) # -549.3467

#######################################################
# Model Build -- Exploration Part 2 (using log_price)
#######################################################

# Create tree plot
fancyRpartPlot(rpart(price ~ ., data = data), sub = "")

# Backward Selection (copied from above section)
model.2.bwd <- regsubsets(log_price ~ carat + color + clarity + cut + channel + store, data = train, nvmax = 29, method="backward") # must list predictors so price isn't included
summary(model.2.bwd) # the first level of each factorized variable doesn't appear in the output

reg.summary.bwd.2 <- summary(model.2.bwd)
names(reg.summary.bwd.2)
reg.summary.bwd.2$rsq
reg.summary.bwd.2$adjr2

which.max(reg.summary.bwd.2$adjr2) # 23: adjr2 = 0.8935387
which.min(reg.summary.bwd.2$cp) # 19: cp = 15.58155
which.min(reg.summary.bwd.2$bic) # 16: bic = -576.2245
# 19 variables is good compromise based on metrics

# Forward Selection
model.fwd <- regsubsets(log_price ~ carat + color + clarity + cut + channel + store, data = train, nvmax = 29, method="forward") # must list predictors so price isn't included
summary(model.fwd) # the first level of each factorized variable doesn't appear in the output

reg.summary.fwd <- summary(model.fwd)
names(reg.summary.fwd)
reg.summary.fwd$rsq
reg.summary.fwd$adjr2

which.max(reg.summary.fwd$adjr2) # 23: adjr2 = 0.8935387
which.min(reg.summary.fwd$cp) # 22: cp = 16.79064
which.min(reg.summary.fwd$bic) # 10: bic = -575.4802

# Stepwise Selection
model.stepwise <- regsubsets(log_price ~ carat + color + clarity + cut + channel + store, data = train, nvmax = 26, method="seqrep") # nvmax > 26 causes R Studio session to crash
summary(model.stepwise) # the first level of each factorized variable doesn't appear in the output

reg.summary.stepwise <- summary(model.stepwise)
names(reg.summary.stepwise)
reg.summary.stepwise$rsq
reg.summary.stepwise$adjr2

which.max(reg.summary.stepwise$adjr2) # 22: adjr2 = 0.8934076
which.min(reg.summary.stepwise$cp) # 19: cp = 15.58155
which.min(reg.summary.stepwise$bic) # 14: bic = -578.4617



### Discarded Code ###

trainIndex <- createDataPartition(data$price, p = 0.70, list = F)
head(trainIndex)

train <- data[ trainIndex,]
test  <- data[-trainIndex,]

summary(model.lda.bwd)

predictors <- data[1:6]
predictors.train <- train[1:6]
class(predictors.train)

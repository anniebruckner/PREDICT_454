# Andrea Bruckner
# Predict 454
# Assignment 3

#######################################################
# Workspace Setup
#######################################################

# Reset plot display
par(mfrow=c(1,1))

# Install Packages if they don't currently exist
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
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=F)
head(data)

#######################################################
# Data Quality Check
#######################################################

# Explore the data -- how big is it, what types of variables included, distributions and missing values.
class(data) # data.frame
dim(data) # 4601   58
nrow(data) # 4601 rows
ncol(data) # 58 variables
names(data) # need to be renamed
str(data) # all numeric except last 3 are integer
summary(data) # no missing data, no invalid data (negative values, in this case)

# Check variable classes
sapply(data, class) # V58 = response since it's the only integer variable with only 0 or 1 values

# List of all column names
all.col.names <- c("word_freq_make",
                   "word_freq_address",
                   "word_freq_all",
                   "word_freq_3d",
                   "word_freq_our",
                   "word_freq_over",
                   "word_freq_remove",
                   "word_freq_internet",
                   "word_freq_order",
                   "word_freq_mail",
                   "word_freq_receive",
                   "word_freq_will",
                   "word_freq_people",
                   "word_freq_report",
                   "word_freq_addresses",
                   "word_freq_free",
                   "word_freq_business",
                   "word_freq_email",
                   "word_freq_you",
                   "word_freq_credit",
                   "word_freq_your",
                   "word_freq_font",
                   "word_freq_000",
                   "word_freq_money",
                   "word_freq_hp",
                   "word_freq_hpl",
                   "word_freq_george",
                   "word_freq_650",
                   "word_freq_lab",
                   "word_freq_labs",
                   "word_freq_telnet",
                   "word_freq_857",
                   "word_freq_data",
                   "word_freq_415",
                   "word_freq_85",
                   "word_freq_technology",
                   "word_freq_1999",
                   "word_freq_parts",
                   "word_freq_pm",
                   "word_freq_direct",
                   "word_freq_cs",
                   "word_freq_meeting",
                   "word_freq_original",
                   "word_freq_project",
                   "word_freq_re",
                   "word_freq_edu",
                   "word_freq_table",
                   "word_freq_conference",
                   "char_freq_semicolon",
                   "char_freq_l_paren",
                   "char_freq_l_bracket",
                   "char_freq_exclamation",
                   "char_freq_usd",
                   "char_freq_pound",
                   "capital_run_length_average",
                   "capital_run_length_longest",
                   "capital_run_length_total",
                   "y")

# Rename all columns
colnames(data) <- all.col.names
names(data)

# Convert to numeric
data$capital_run_length_longest <- as.numeric(data$capital_run_length_longest)
data$capital_run_length_total  <- as.numeric(data$capital_run_length_total )

# Convert to factor
data$y <- as.factor(data$y)
levels(data$y) <- c("Not_Spam", "Spam")
summary(data$y)
# Not_Spam     Spam 
# 2788     1813
barchart(data$y, col = "steelblue")
str(data)
1813/4601 # 0.3940448 are spam

# Create log transformations of all predictors
#pred.log1 <- lapply(data[1:57], log)
#pred.log1 <- data.frame(pred.log1, y = data$y)
#head(pred.log1) # has -Inf, doesn't work well in plots

pred.log <- lapply(data[1:57], log1p)
pred.log <- data.frame(pred.log, y = data$y)
head(pred.log)

summary(data)

# Plot histograms of the variables--How can I do this using lattice?
plot_vars <- function (data, column){
  ggplot(data = data, aes_string(x = column)) +
    geom_histogram(color =I("black"), fill = I("steelblue"))+
    xlab(column) + theme_bw() + theme(axis.title=element_text(size=8, face="bold"))
}

#plotsAll <- lapply(colnames(data[1:57]), plot_vars, data = data)
#length(plotsAll)
#do.call("grid.arrange", c(plotsAll, nrow=19)) # unreadable

plotsA <- lapply(colnames(data[1:24]), plot_vars, data = data)
length(plotsA)
do.call("grid.arrange", c(plotsA, ncol=4))

plotsB <- lapply(colnames(data[25:48]), plot_vars, data = data)
length(plotsB)
do.call("grid.arrange", c(plotsB, ncol=4))

plotsC <- lapply(colnames(data[49:54]), plot_vars, data = data)
length(plotsC)
do.call("grid.arrange", c(plotsC, ncol=3))

plotsD <- lapply(colnames(data[55:57]), plot_vars, data = data)
length(plotsD)
do.call("grid.arrange", c(plotsD, ncol=3))

# Create histograms for all predictor variables
myHist <- function(variable, df){
  toPlot = paste0("~ ", variable)
  histogram(as.formula(toPlot), data = df, col = "steelblue",  xlab = paste0(variable))
}

hist.overview <- lapply(colnames(pred.log[1:24]), myHist, pred.log) # [1:57] is unreadable
length(hist.overview)
do.call("grid.arrange", c(hist.overview, ncol=6))

# Create box plots of each predictor according to response
myBoxplots <- function(variable, df){
  toPlot = paste0("~ ", variable, " | y")
  bwplot(as.formula(toPlot), data = df,
         layout = c(2, 1),
         par.settings = list(
           box.umbrella=list(col= "black"), 
           box.dot=list(col= "black"), 
           box.rectangle = list(col= "black", fill = "steelblue")),
         strip = strip.custom(bg="lightgrey"),
         xlab = paste0(variable))}

# easier to see patterns in pred.log than data
plots.overview <- lapply(colnames(pred.log[1:24]), myBoxplots, pred.log) # [1:57] is unreadable
length(plots.overview)
do.call("grid.arrange", c(plots.overview, ncol=6))

# Interesting Spam v. Not_Span differences: capital_run_length_total, usd, exclamation, re, 1999, george, hpl, hp, money, 000, your, email, business, free, people, receive, mail, order, internet, remove, over, our, all, address, make
# spam: make, address, all, our, over, remove, internet, order, mail, receive, people, free, business, email, your, you, 000, money, exclamation, usd
# not_spam: hp, hpl, george, 1999, re
# for plots in report:
# spam: free, money, exclamation
# not_spam: hp, george, re
# your, capital_run_length_average, capital_run_length_longest

plots.report <- lapply(colnames(pred.log[c(16,24,52,25,27,45,21,55,56)]), myBoxplots, pred.log)
length(plots.report)
do.call("grid.arrange", c(plots.report, ncol=3))

#######################################################
# EDA -- Make sure to talk about a few interesting box plots
#######################################################

# Create naive tree models
set.seed(123)
fancyRpartPlot(rpart(y ~ ., data = data), sub = "")
#fancyRpartPlot(rpart(y ~ ., data = pred.log), sub = "")

# Examine correlations among just numeric variables
c <- cor(data[1:57], use="complete.obs")

correlations <- data.frame(c)

significant.correlations <- data.frame(
  var1 = character(),
  var2 = character(),
  corr = numeric())

for (i in 1:nrow(correlations)){
  for (j in 1:ncol(correlations)){
    tmp <- data.frame(
      var1 = as.character(colnames(correlations)[j]),
      var2 = as.character(rownames(correlations)[i]),
      corr = correlations[i,j])
    
    if (!is.na(correlations[i,j])) {
      if (correlations[i,j] > .5 & as.character(tmp$var1) != as.character(tmp$var2)
          | correlations[i,j] < -.5 & as.character(tmp$var1) != as.character(tmp$var2) ) {
        significant.correlations <- rbind(significant.correlations,tmp) }
    }
  }
}

significant.correlations <- significant.correlations[order(abs(significant.correlations$corr),decreasing=TRUE),] 
significant.correlations <- significant.correlations[which(!duplicated(significant.correlations$corr)),]
significant.correlations

# Create object containing names of only predictor variables
pred <- colnames(data[1:57])

# Visualize correlations between predictors and response
corrplot(cor(data[data$y == "Not_Spam", pred]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         mar=c(1,1,2,2), type = "lower", main = "Correlations for Not_Spam")

corrplot(cor(data[data$y == "Spam", pred]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         mar=c(1,1,2,2), type = "lower", main = "Correlations for Spam")

#######################################################
# Model Build - Set Up
#######################################################

# Create train/test sets (70/30)
nrow(data) # 4601: 0.70*4601 = 3220.7 (train should have 3220 or 3221 observations)

set.seed(123)
train <- sample_frac(data, 0.70)
train_id <- as.numeric(rownames(train)) 
test <- data[-train_id,]

head(train)
dim(train) # 3221   58
head(test)
dim(test) # 1380   58

#######################################################
# Model Build -- Fit Model Suite
#######################################################

# First set completely naive logistic model without using variable selection
#set.seed(123)
#model.logit <- glm(y ~ ., data = train, family=binomial("logit"))
#Warning message:
#  glm.fit: fitted probabilities numerically 0 or 1 occurred 
#summary(model.logit)

# (1) a logistic regression model using variable selection

# Model A
# Use backward subset selection on model.logit
model.logit.bwd <- regsubsets(y ~ ., data = train, nvmax=57, method="backward")
options(max.print=1000000)
summary(model.logit.bwd)
names(model.logit.bwd) # Is there a metric that lets me see which is best?
summary.regsubsets(model.logit.bwd)
#which.min(model.logit.bwd$rss) # 58 
#model.logit.bwd$rss[58] # 330.9772

# Model B
# Use stepwise subset selection on model.logit
model.logit.step <- regsubsets(y ~ ., data = train, nvmax=NULL, method="seqrep")
options(max.print=1000000)
summary(model.logit.step)
names(model.logit.step) # Is there a metric that lets me see which is best?
#which.min(model.logit.step$rss) # 58 
#model.logit.step$rss[58] # 330.9772

# Model C
logit.control <- trainControl(classProbs = T, savePred = T , verboseIter = T)

# Another stepwise method that identifies which is best:
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
set.seed(123)
model.logit.stepAIC <- train(y ~ ., data = train, method = "glmStepAIC",
                             direction = "forward", trControl = logit.control)

names(model.logit.stepAIC)
model.logit.stepAIC$finalModel
length(model.logit.stepAIC$finalModel$coefficients)-1 # find number of predictors used: 37
AIC(model.logit.stepAIC$finalModel) # 1330.207

# Model D
mod_fit <- train(y ~ word_freq_your + word_freq_000 + word_freq_remove + word_freq_free + capital_run_length_total + word_freq_money + char_freq_exclamation + word_freq_our + word_freq_hp + char_freq_usd,  data=train, method="glm", family="binomial")
mod_fit$finalModel
#Coefficients:
#  (Intercept)            word_freq_your             word_freq_000          word_freq_remove  
#-2.093421                  0.336930                  3.336319                  3.345913  
#word_freq_free  capital_run_length_total           word_freq_money     char_freq_exclamation  
#0.868948                  0.001193                  2.260708                  0.591771  
#word_freq_our              word_freq_hp             char_freq_usd  
#0.549758                 -3.260988                  5.444820  

#Degrees of Freedom: 3220 Total (i.e. Null);  3210 Residual
#Null Deviance:	    4314 
#Residual Deviance: 1949 	AIC: 1971

# Model E
set.seed(123)
model.logit.1 <- glm(y ~ word_freq_your + word_freq_000 + word_freq_remove + word_freq_free + capital_run_length_total + word_freq_money + char_freq_exclamation + word_freq_our + word_freq_hp + char_freq_usd,  data=train, family=binomial("logit"))
summary(model.logit.1)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              -2.0934207  0.0895554 -23.376  < 2e-16 ***
#  word_freq_your            0.3369296  0.0442133   7.621 2.53e-14 ***
#  word_freq_000             3.3363191  0.5431417   6.143 8.12e-10 ***
#  word_freq_remove          3.3459128  0.3705193   9.030  < 2e-16 ***
#  word_freq_free            0.8689480  0.1129557   7.693 1.44e-14 ***
#  capital_run_length_total  0.0011929  0.0001555   7.669 1.73e-14 ***
#  word_freq_money           2.2607075  0.3790107   5.965 2.45e-09 ***
#  char_freq_exclamation     0.5917706  0.1045152   5.662 1.50e-08 ***
#  word_freq_our             0.5497582  0.0817822   6.722 1.79e-11 ***
#  word_freq_hp             -3.2609878  0.4612506  -7.070 1.55e-12 ***
#  char_freq_usd             5.4448198  0.7180941   7.582 3.39e-14 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (2) a tree model
# Model A
set.seed(123)
fit2 <- rpart(y ~ ., data = train)
fit2
dev.new(width=10, height=8) # This fixes the tiny font issue
fit2_plot <- fancyRpartPlot(fit2, sub = "") # Why is the font so small?
dev.off()

# Model B
control.tree <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T, savePred = T, verboseIter = T)
set.seed(123)
tree.CV <- train(y ~ ., data = train, method = "rpart", trControl = control.tree)
# Fitting cp = 0.0435 on full training set
tree.CV$finalModel
fancyRpartPlot(tree.CV$finalModel, sub = "")

# (3) a Support Vector Machine
# Model A
set.seed(123)
model.svm <- svm(y~., data=train, kernel ="linear", cost =1)
summary(model.svm)
plot(model.svm , train$y)

set.seed(123)
svm.tune <- tune(svm,y~.,data=train, kernel ="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
svm.tune$best.model
#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  5 
#gamma:  0.01754386 
#Number of Support Vectors:  828

summary(svm.tune)
#Parameter tuning of ‘svm’:
#- sampling method: 10-fold cross validation 
#- best parameters:
# cost: 5
#- best performance: 0.06612599 
#- Detailed performance results:
#  cost      error  dispersion
#1 1e-03 0.39212834 0.031195773
#2 1e-02 0.32568506 0.035421299
#3 1e-01 0.09934908 0.013102509
#4 1e+00 0.07326693 0.010143842
#5 5e+00 0.06612599 0.009470988
#6 1e+01 0.06612695 0.008272670
#7 1e+02 0.07916851 0.009176701

# Model B
svm.control <- trainControl(method = "cv", classProbs = T, savePred = T, verboseIter = T)
processing.time <- proc.time()
#names(getModelInfo())
set.seed(123)
model.svm.CV <- train(y ~ ., data = train, method = "svmRadial", # or svmRadialWeights? -- same results
                    trControl = svm.control) # metric="ROC" doesn't do anything for this model
#+ Fold01: sigma=0.02897, C=0.25 
#- Fold01: sigma=0.02897, C=0.25 
#+ Fold01: sigma=0.02897, C=0.50 
#- Fold01: sigma=0.02897, C=0.50 
#+ Fold01: sigma=0.02897, C=1.00 
#- Fold01: sigma=0.02897, C=1.00 
#+ Fold02: sigma=0.02897, C=0.25 
#- Fold02: sigma=0.02897, C=0.25 
#+ Fold02: sigma=0.02897, C=0.50 
#- Fold02: sigma=0.02897, C=0.50 
#+ Fold02: sigma=0.02897, C=1.00 
#- Fold02: sigma=0.02897, C=1.00 
#+ Fold03: sigma=0.02897, C=0.25 
#- Fold03: sigma=0.02897, C=0.25 
#+ Fold03: sigma=0.02897, C=0.50 
#- Fold03: sigma=0.02897, C=0.50 
#+ Fold03: sigma=0.02897, C=1.00 
#- Fold03: sigma=0.02897, C=1.00 
#+ Fold04: sigma=0.02897, C=0.25 
#- Fold04: sigma=0.02897, C=0.25 
#+ Fold04: sigma=0.02897, C=0.50 
#- Fold04: sigma=0.02897, C=0.50 
#+ Fold04: sigma=0.02897, C=1.00 
#- Fold04: sigma=0.02897, C=1.00 
#+ Fold05: sigma=0.02897, C=0.25 
#- Fold05: sigma=0.02897, C=0.25 
#+ Fold05: sigma=0.02897, C=0.50 
#- Fold05: sigma=0.02897, C=0.50 
#+ Fold05: sigma=0.02897, C=1.00 
#- Fold05: sigma=0.02897, C=1.00 
#+ Fold06: sigma=0.02897, C=0.25 
#- Fold06: sigma=0.02897, C=0.25 
#+ Fold06: sigma=0.02897, C=0.50 
#- Fold06: sigma=0.02897, C=0.50 
#+ Fold06: sigma=0.02897, C=1.00 
#- Fold06: sigma=0.02897, C=1.00 
#+ Fold07: sigma=0.02897, C=0.25 
#- Fold07: sigma=0.02897, C=0.25 
#+ Fold07: sigma=0.02897, C=0.50 
#- Fold07: sigma=0.02897, C=0.50 
#+ Fold07: sigma=0.02897, C=1.00 
#- Fold07: sigma=0.02897, C=1.00 
#+ Fold08: sigma=0.02897, C=0.25 
#- Fold08: sigma=0.02897, C=0.25 
#+ Fold08: sigma=0.02897, C=0.50 
#- Fold08: sigma=0.02897, C=0.50 
#+ Fold08: sigma=0.02897, C=1.00 
#- Fold08: sigma=0.02897, C=1.00 
#+ Fold09: sigma=0.02897, C=0.25 
#- Fold09: sigma=0.02897, C=0.25 
#+ Fold09: sigma=0.02897, C=0.50 
#- Fold09: sigma=0.02897, C=0.50 
#+ Fold09: sigma=0.02897, C=1.00 
#- Fold09: sigma=0.02897, C=1.00 
#+ Fold10: sigma=0.02897, C=0.25 
#- Fold10: sigma=0.02897, C=0.25 
#+ Fold10: sigma=0.02897, C=0.50 
#- Fold10: sigma=0.02897, C=0.50 
#+ Fold10: sigma=0.02897, C=1.00 
#- Fold10: sigma=0.02897, C=1.00 
#Aggregating results
#Selecting tuning parameters
#Fitting sigma = 0.029, C = 1 on full training set

model.svm.CV$finalModel
#Support Vector Machine object of class "ksvm" 
#SV type: C-svc  (classification) 
#parameter : cost C = 1 
#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.0289667211129033 
#Number of Support Vectors : 1082 
#Objective Function Value : -603.77 
#Training error : 0.044396 
#Probability model included. 


# (4) Random Forest
# Model A
set.seed(123)
train.matrix  <- model.matrix(y ~ ., data=train)[,-58] # create predictor matrix (subtract last column, the response variable)
ncol(train.matrix) # 57
head(train.matrix)

set.seed(123)
# mtry can be 57 at most because of number of columns in train.matrix
fit4_baseline <- randomForest(train.matrix, train$y, mtry=floor(sqrt(ncol(train.matrix))), importance=TRUE)
fit4_baseline
#Number of trees: 500
#No. of variables tried at each split: 7
#OOB estimate of  error rate: 4.75%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1901   57  0.02911134
#Spam           96 1167  0.07600950

# Random Search
#control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
#set.seed(123)
#mtry <- sqrt(ncol(train.matrix)) # 7.549834
#rf_random <- train(train.matrix, train$y, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
#print(rf_random)
#3221 samples
#57 predictor
#2 classes: 'Not_Spam', 'Spam'
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 2898, 2900, 2899, 2898, 2900, 2899, ... 
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa    
#1    0.9086204  0.8020479
#6    0.9528087  0.9004815
#11    0.9517696  0.8983503
#12    0.9510456  0.8968460
#15    0.9501143  0.8949502
#19    0.9498037  0.8943007
#21    0.9498066  0.8943489
#33    0.9476317  0.8897121
#38    0.9469074  0.8882671
#39    0.9469064  0.8881823
#41    0.9471138  0.8886953
#42    0.9475282  0.8895919
#50    0.9459751  0.8862731
#56    0.9457684  0.8858360

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 6.

#plot(rf_random)

set.seed(123)
fit4 <- randomForest(train.matrix, train$y, mtry=6, importance=TRUE)
fit4 # run rf_random again when I have time since the fit4 output changed
# results on 2/3
#Number of trees: 500
#No. of variables tried at each split: 6
#OOB estimate of  error rate: 4.59%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1901   57  0.02911134
#Spam           91 1172  0.07205067

# results on 2/4
#Number of trees: 500
#No. of variables tried at each split: 6
#OOB estimate of  error rate: 4.75%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1901   57  0.02911134
#Spam           96 1167  0.07600950

set.seed(123)
fit4a <- randomForest(train.matrix, train$y, mtry=24, importance=TRUE)
fit4a # OOB estimate of  error rate: 5.06%

# Model B
rf.control <- trainControl(method = "cv", classProbs = T, savePred = T, verboseIter = T)
rf.control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T, savePred = T, verboseIter = T)

set.seed(123)
model.rf <- train(y ~ ., data = train, method = "rf", trControl = rf.control)
#Fitting mtry = 29 on full training set

set.seed(123)
model.rf2 <- train(y ~ ., data = train, method = "rf", trControl = rf.control2)
#Fitting mtry = 29 on full training set














#--------#
# Code for one lattice boxplot
crla <- bwplot(~ capital_run_length_average | y, data = data,
       layout = c(2, 1),
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))
class(crla)

#--------# Discard #--------#
install.packages("bestglm")
library(bestglm)
res.best.logistic <- bestglm(Xy = train, 
                             family = binomial,          # binomial family for logistic
                             IC = "AIC",                 # Information criteria for
                             method = "seqrep")
res.best.logistic$BestModels

#--------# Discarded Functions #--------#
plot_box <- function (df){
  for (i in 1:57){
    toPlot = paste0("~ ", names(df)[i], " | y")
    p <- bwplot(as.formula(toPlot), data = df, par.settings = list(
      box.umbrella=list(col= "black"), 
      box.dot=list(col= "black"), 
      box.rectangle = list(col= "black", fill = "steelblue")),
      strip = strip.custom(bg="lightgrey"),
      xlab = names(df)[i])
    print(p)}
}

plot_box(data)

lat.box <- function(variable){
  toPlot = paste0("~ ", variable, " | y")
  bwplot(as.formula(toPlot), data = pred.log,
         layout = c(2, 1),
         par.settings = list(
           box.umbrella=list(col= "black"), 
           box.dot=list(col= "black"), 
           box.rectangle = list(col= "black", fill = "steelblue")),
         strip = strip.custom(bg="lightgrey"),
         xlab = paste0(variable))}

plotsA.box <- lapply(colnames(pred.log[c(16,24,52,25,27,45,21,55,56)]), lat.box)
length(plotsA.box)
do.call("grid.arrange", c(plotsA.box, ncol=3))

plotsB.box <- lapply(colnames(pred.log[1:57]), lat.box)
length(plotsB.box)
do.call("grid.arrange", c(plotsB.box, ncol=3))


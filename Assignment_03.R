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
                      ,"car"
                      ,"pROC"
                      ,"bestglm")

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

pred.log <- lapply(data[1:57], log1p) # log1p works better than log when x is small
pred.log <- data.frame(pred.log, y = data$y)
head(pred.log)

summary(data)

# Plot histograms of the variables--See how to do this in lattice after PlotsD
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

#######################################################
# EDA
#######################################################

# Interesting boxplots for discussion
plots.report <- lapply(colnames(pred.log[c(16,24,52,25,27,45,21,55,56)]), myBoxplots, pred.log)
length(plots.report)
do.call("grid.arrange", c(plots.report, ncol=3))
# spam: free, money, exclamation
# not_spam: hp, george, re
# your, capital_run_length_average, capital_run_length_longest

# Create naive tree models
set.seed(123)
fancyRpartPlot(rpart(y ~ ., data = data), sub = "")
fancyRpartPlot(rpart(y ~ ., data = pred.log), sub = "")

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
significant.correlations # this is not helpful enough to include

# Create object containing names of only predictor variables
pred <- colnames(data[1:57])
pred2 <- colnames(pred.log[1:57])

# Visualize correlations between predictors and response classes -- these are too hard to read
corrplot(cor(data[data$y == "Not_Spam", pred]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         mar=c(1,1,2,2), type = "lower", main = "Correlations for Not_Spam")

corrplot(cor(data[data$y == "Spam", pred]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         mar=c(1,1,2,2), type = "lower", main = "Correlations for Spam")

#######################################################
# Model Build - Set Up par(mfrow=c(1,1))
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

# Create log train/test sets (70/30)
set.seed(123)
train.log <- sample_frac(pred.log, 0.70)
train_id.log <- as.numeric(rownames(train.log)) 
test.log <- pred.log[-train_id.log,]

head(train.log)
dim(train.log) # 3221   58
head(test.log)
dim(test.log) # 1380   58

#######################################################
# Model Build -- Fit Model Suite
#######################################################

# Add for each model to calculate processing time and set same seed
#ptm <- proc.time() # Start the clock!
#set.seed(123)
#proc.time() - ptm # Stop the clock

# (1) a logistic regression model using variable selection -- how to determine best model from regsubsets()?

# First set completely naive logistic model without using variable selection
#set.seed(123)
#model.logit <- glm(y ~ ., data = train, family=binomial("logit"))
#Warning message:
#  glm.fit: fitted probabilities numerically 0 or 1 occurred 
#summary(model.logit)

install.packages("bestglm")
library(bestglm)

# Backward train
set.seed(123)
model.logit.bwd <- regsubsets(y ~ ., data = train, nvmax=NULL, method="backward")
options(max.print=1000000)
summary(model.logit.bwd)
names(model.logit.bwd) # Is there a metric that lets me see which is best?
#which.min(model.logit.bwd$rss) # 58 
#model.logit.bwd$rss[58] # 330.9772
#plot(model.logit.bwd, scale="bic")

#args(bestglm)
#bestglm(train, IC = "AIC", method = "backward")

# Forward train
set.seed(123)
model.logit.fwd <- regsubsets(y ~ ., data = train, nvmax=NULL, method="forward")
options(max.print=1000000)
summary(model.logit.fwd) # chooses the same top 15+ variables as bwd

# Stepwise train
set.seed(123)
model.logit.step <- regsubsets(y ~ ., data = train, nvmax=NULL, method="seqrep")
options(max.print=1000000)
summary(model.logit.step)

# All Subsets train -- very long process time (overnight rec)
set.seed(123)
model.logit.allsub <- regsubsets(y ~ ., data = train, nvmax=NULL, method="exhaustive", really.big = TRUE)
options(max.print=1000000)
summary(model.logit.allsub)

# Model using top 15 predictors from Stepwise
set.seed(123)
model.logit.step.fit <- train(y ~ word_freq_your + word_freq_000 + word_freq_remove + word_freq_free + capital_run_length_total + word_freq_money + char_freq_exclamation + word_freq_our + word_freq_hp + char_freq_usd,  data=train, method="glm", family="binomial")
model.logit.step.fit$finalModel
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

# Model using top 15 predictors from Stepwise -- same as model.logit.step.fit even though left out glm and using family=binomial("logit")
set.seed(123)
model.logit.step.fit2 <- glm(y ~ word_freq_your + word_freq_000 + word_freq_remove + word_freq_free + capital_run_length_total + word_freq_money + char_freq_exclamation + word_freq_our + word_freq_hp + char_freq_usd,  data=train, family=binomial("logit"))
summary(model.logit.step.fit2)

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

#(Dispersion parameter for binomial family taken to be 1)
#Null deviance: 4314.1  on 3220  degrees of freedom
#Residual deviance: 1949.1  on 3210  degrees of freedom
#AIC: 1971.1
#Number of Fisher Scoring iterations: 9

#--------#

# Method using train() on train -- long process time (1+ hours)
# Another stepwise method that identifies which is best:
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
names(getModelInfo()) # lots of glm options
#model.logit.step2 <- train(y ~ ., data = train, method = "leapSeq") # wrong model type for classification

# Timing code: http://www.ats.ucla.edu/stat/r/faq/timing_code.htm
ptm <- proc.time() # Start the clock!

logit.control <- trainControl(classProbs = T, savePred = T , verboseIter = T)
set.seed(123)
model.logit.step2 <- train(y ~ ., data = train, method = "glmStepAIC",
                           direction = "forward", trControl = logit.control)
proc.time() - ptm # Stop the clock

names(model.logit.step2)
model.logit.step2$finalModel
summary(model.logit.step2$finalModel)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.7669  -0.1719   0.0000   0.1227   5.1694  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                -1.456e+00  1.292e-01 -11.262  < 2e-16 ***
#  char_freq_usd               4.300e+00  7.265e-01   5.919 3.25e-09 ***
#  word_freq_hp               -2.737e+00  5.748e-01  -4.763 1.91e-06 ***
#  word_freq_remove            1.910e+00  3.345e-01   5.710 1.13e-08 ***
#  capital_run_length_longest  1.073e-02  2.052e-03   5.231 1.69e-07 ***
#  word_freq_george           -1.129e+01  2.035e+00  -5.548 2.89e-08 ***
#  word_freq_free              1.036e+00  1.639e-01   6.320 2.62e-10 ***
#  word_freq_edu              -1.596e+00  3.502e-01  -4.557 5.18e-06 ***
#  word_freq_business          1.181e+00  2.801e-01   4.218 2.47e-05 ***
#  word_freq_meeting          -2.866e+00  1.074e+00  -2.670 0.007585 ** 
#  word_freq_000               2.037e+00  4.457e-01   4.569 4.90e-06 ***
#  word_freq_money             1.858e+00  4.623e-01   4.019 5.86e-05 ***
#  word_freq_our               5.602e-01  1.225e-01   4.571 4.85e-06 ***
#  word_freq_order             1.116e+00  3.426e-01   3.257 0.001125 ** 
#  word_freq_project          -1.821e+00  6.322e-01  -2.880 0.003973 ** 
#  word_freq_credit            1.512e+00  6.357e-01   2.378 0.017405 *  
#  word_freq_over              9.780e-01  3.387e-01   2.887 0.003885 ** 
#  word_freq_data             -1.024e+00  4.218e-01  -2.429 0.015155 *  
#  word_freq_conference       -3.486e+00  1.621e+00  -2.150 0.031520 *  
#  word_freq_re               -7.424e-01  1.895e-01  -3.918 8.92e-05 ***
#  char_freq_exclamation       3.254e-01  1.007e-01   3.232 0.001228 ** 
#  word_freq_internet          5.097e-01  1.767e-01   2.885 0.003912 ** 
#  word_freq_cs               -3.749e+01  2.632e+01  -1.425 0.154207    
#capital_run_length_total    7.734e-04  2.571e-04   3.008 0.002629 ** 
#  word_freq_your              2.162e-01  5.968e-02   3.623 0.000291 ***
#  word_freq_hpl              -2.049e+00  8.476e-01  -2.417 0.015634 *  
#  char_freq_pound             3.442e+00  9.622e-01   3.577 0.000347 ***
#  char_freq_semicolon        -9.616e-01  3.452e-01  -2.786 0.005343 ** 
#  word_freq_address          -1.467e-01  7.465e-02  -1.966 0.049345 *  
#  word_freq_415              -1.394e+01  4.195e+00  -3.322 0.000892 ***
#  word_freq_650               4.755e-01  2.526e-01   1.882 0.059814 .  
#word_freq_3d                1.334e+00  1.512e+00   0.882 0.377569    
#word_freq_85               -1.544e+00  8.025e-01  -1.924 0.054394 .  
#word_freq_make             -5.405e-01  2.624e-01  -2.060 0.039391 *  
#  word_freq_pm               -8.879e-01  4.448e-01  -1.996 0.045934 *  
#  word_freq_lab              -1.679e+00  1.266e+00  -1.327 0.184666    
#word_freq_technology        8.158e-01  3.859e-01   2.114 0.034506 *  
#  word_freq_original         -1.321e+00  1.059e+00  -1.247 0.212279    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#(Dispersion parameter for binomial family taken to be 1)
#Null deviance: 4314.1  on 3220  degrees of freedom
#Residual deviance: 1254.2  on 3183  degrees of freedom
#AIC: 1330.2
#Number of Fisher Scoring iterations: 13

length(model.logit.step2$finalModel$coefficients)-1 # find number of predictors used: 37
AIC(model.logit.step2$finalModel) # 1330.207
head(model.logit.step2$pred)
model.logit.step2$metric # Accuracy
head(model.logit.step2$results)
#parameter  Accuracy     Kappa  AccuracySD    KappaSD
#1      none 0.9164162 0.8237562 0.007529417 0.01584371

# Predict train
set.seed(123)
model.logit.step2.pred <- predict(model.logit.step2, newdata = train, 
                               type = "prob")[,2]
length(model.logit.step2.pred) # 3221
head(model.logit.step2.pred)

# Plot ROC curve
set.seed(123)
model.logit.step2.roc <- plot.roc(train$y, model.logit.step2.pred)
model.logit.step2.auc <- model.logit.step2.roc$auc
model.logit.step2.auc # Area under the curve: 0.9781

par(pty = "s") # "s" generates a square plotting region
plot(model.logit.step2.roc, col = "steelblue", main = "ROC Curve for Stepwise Logistic Regression Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
model.logit.step2.pred2 <- predict(model.logit.step2, newdata = train) # no type = "prob"
dim(model.logit.step2.pred2)
head(model.logit.step2.pred2)
set.seed(123)
model.logit.step2.cmat <- confusionMatrix(model.logit.step2.pred2, train$y)
model.logit.step2.cmat
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1866  137
#Spam           92 1126

#Accuracy : 0.9289          
#95% CI : (0.9195, 0.9375)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.8499          
#Mcnemar's Test P-Value : 0.003642        

#Sensitivity : 0.9530          
#Specificity : 0.8915          
#Pos Pred Value : 0.9316          
#Neg Pred Value : 0.9245          
#Prevalence : 0.6079          
#Detection Rate : 0.5793          
#Detection Prevalence : 0.6219          
#Balanced Accuracy : 0.9223          
#'Positive' Class : Not_Spam

# Predict test
set.seed(123)
model.logit.step2.pred.test <- predict(model.logit.step2, newdata = test)

set.seed(123)
model.logit.step2.cmat.test <- confusionMatrix(model.logit.step2.pred.test, test$y)
model.logit.step2.cmat.test
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam      801   66
#Spam           29  484

#Accuracy : 0.9312         
#95% CI : (0.9165, 0.944)
#No Information Rate : 0.6014         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.8548         
#Mcnemar's Test P-Value : 0.0002212      

#Sensitivity : 0.9651         
#Specificity : 0.8800         
#Pos Pred Value : 0.9239         
#Neg Pred Value : 0.9435         
#Prevalence : 0.6014         
#Detection Rate : 0.5804         
#Detection Prevalence : 0.6283         
#Balanced Accuracy : 0.9225         

#'Positive' Class : Not_Spam


### Do above code using log train and log test sets?


# -------------------------------------------------------------------------#
# (2) a tree model -- model.tree does way better than model.tree2
ptm <- proc.time() # Start the clock!
set.seed(123)
model.tree <- rpart(y ~ ., data = train)
model.tree
proc.time() - ptm # Stop the clock

dev.new(width=10, height=8) # This fixes the tiny font issue
model.tree_plot <- fancyRpartPlot(model.tree, sub = "") # The font is very small without the above code
dev.off()

# Predict train
set.seed(123)
model.tree.pred <- predict(model.tree, newdata = train, 
                                  type = "prob")[,2]
length(model.tree.pred) # 3221
head(model.tree.pred)

# Plot ROC curve
set.seed(123)
model.tree.roc <- plot.roc(train$y, model.tree.pred)
model.tree.auc <- model.tree.roc$auc
model.tree.auc # Area under the curve: 0.9223

par(pty = "s") # "s" generates a square plotting region
plot(model.tree.roc, col = "steelblue", main = "ROC Curve for Tree Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
model.tree.pred2 <- predict(model.tree, newdata = train)
length(model.tree.pred2) # 3221
head(model.tree.pred2) # This needs to show just Spam or Not_Spam--shows matrix with probabilities of each prediction
#######
# Make predictions just Spam or Not_Spam
model.tree.pred2.binary <- data.frame(model.tree.pred2)

model.tree.pred2.binary$y <- NA
head(model.tree.pred2.binary)
model.tree.pred2.binary[,1]

# Convert to factor
model.tree.pred2.binary$y <- as.factor(model.tree.pred2.binary$y)
levels(model.tree.pred2.binary$y) <- c("Not_Spam", "Spam")
summary(model.tree.pred2.binary$y)

for (i in 1:nrow(model.tree.pred2.binary)){
  model.tree.pred2.binary[i,3] <- ifelse(model.tree.pred2.binary[i,1] > model.tree.pred2.binary[i,2], "Not_Spam", "Spam")
}

head(model.tree.pred2.binary)
summary(model.tree.pred2.binary$y)
#Not_Spam     Spam 
#2067     1154
#######
set.seed(123)
model.tree.cmat <- confusionMatrix(model.tree.pred2.binary$y, train$y)
model.tree.cmat
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1873  194
#Spam           85 1069

#Accuracy : 0.9134          
#95% CI : (0.9031, 0.9229)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.8155          
#Mcnemar's Test P-Value : 1.008e-10       

#Sensitivity : 0.9566          
#Specificity : 0.8464          
#Pos Pred Value : 0.9061          
#Neg Pred Value : 0.9263          
#Prevalence : 0.6079          
#Detection Rate : 0.5815          
#Detection Prevalence : 0.6417          
#Balanced Accuracy : 0.9015          

#'Positive' Class : Not_Spam

# Predict test
set.seed(123)
model.tree.pred.test <- predict(model.tree, newdata = test)
length(model.tree.pred.test) # 3221
head(model.tree.pred.test) # This needs to show just Spam or Not_Spam
#######
# Make predictions just Spam or Not_Spam
model.tree.pred.test.binary <- data.frame(model.tree.pred.test)

model.tree.pred.test.binary$y <- NA
head(model.tree.pred.test.binary)

# Convert to factor
model.tree.pred.test.binary$y <- as.factor(model.tree.pred.test.binary$y)
levels(model.tree.pred.test.binary$y) <- c("Not_Spam", "Spam")
summary(model.tree.pred.test.binary$y)

for (i in 1:nrow(model.tree.pred.test.binary)){
  model.tree.pred.test.binary[i,3] <- ifelse(model.tree.pred.test.binary[i,1] > model.tree.pred.test.binary[i,2], "Not_Spam", "Spam")
}

head(model.tree.pred.test.binary)
summary(model.tree.pred.test.binary$y)
#Not_Spam     Spam 
#895      485
#######
set.seed(123)
model.tree.cmat.test <- confusionMatrix(model.tree.pred.test.binary$y, test$y)
model.tree.cmat.test
#Reference
#Prediction Not_Spam Spam
#Not_Spam      794  101
#Spam           36  449

#Accuracy : 0.9007         
#95% CI : (0.8837, 0.916)
#No Information Rate : 0.6014         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.7887         
#Mcnemar's Test P-Value : 4.554e-08      

#Sensitivity : 0.9566         
#Specificity : 0.8164         
#Pos Pred Value : 0.8872         
#Neg Pred Value : 0.9258         
#Prevalence : 0.6014         
#Detection Rate : 0.5754         
#Detection Prevalence : 0.6486         
#Balanced Accuracy : 0.8865         

#'Positive' Class : Not_Spam


# Model tree with train() 10-fold CV x 3
ptm <- proc.time() # Start the clock!
control.tree <- trainControl(method = "cv", number = 10, classProbs = T, savePred = T, verboseIter = T)
set.seed(123)
model.tree2 <- train(y ~ ., data = train, method = "rpart", trControl = control.tree)
proc.time() - ptm # Stop the clock
#user  system elapsed 
#2.955   0.953   4.037 

# Fitting cp = 0.0435 on full training set
model.tree2$finalModel
fancyRpartPlot(model.tree2$finalModel, sub = "")

# Predict train
set.seed(123)
model.tree2.pred <- predict(model.tree2, newdata = train, 
                           type = "prob")[,2]
length(model.tree2.pred) # 3221
head(model.tree2.pred)

# Plot ROC curve
set.seed(123)
model.tree2.roc <- plot.roc(train$y, model.tree2.pred)
model.tree2.auc <- model.tree2.roc$auc
model.tree2.auc # Area under the curve: 0.8298

par(pty = "s") # "s" generates a square plotting region
plot(model.tree2.roc, col = "steelblue", main = "ROC Curve for Tree Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
model.tree2.pred2 <- predict(model.tree2, newdata = train)
length(model.tree2.pred2) # 3221
head(model.tree2.pred2) # This shows just Spam or Not_Spam

set.seed(123)
model.tree2.cmat <- confusionMatrix(model.tree2.pred2, train$y)
model.tree2.cmatc
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1818  345
#Spam          140  918

#Accuracy : 0.8494          
#95% CI : (0.8366, 0.8616)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.6748          
#Mcnemar's Test P-Value : < 2.2e-16       

#Sensitivity : 0.9285          
#Specificity : 0.7268          
#Pos Pred Value : 0.8405          
#Neg Pred Value : 0.8677          
#Prevalence : 0.6079          
#Detection Rate : 0.5644          
#Detection Prevalence : 0.6715          
#Balanced Accuracy : 0.8277          

#'Positive' Class : Not_Spam

# Predict test
set.seed(123)
model.tree2.pred.test <- predict(model.tree2, newdata = test)
length(model.tree2.pred.test) # 3221
head(model.tree2.pred.test) # This shows just Spam or Not_Spam

set.seed(123)
model.tree2.cmat.test <- confusionMatrix(model.tree2.pred.test, test$y)
model.tree2.cmat.test
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam      783  156
#Spam           47  394

#Accuracy : 0.8529          
#95% CI : (0.8331, 0.8712)
#No Information Rate : 0.6014          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.6826          
#Mcnemar's Test P-Value : 3.453e-14       

#Sensitivity : 0.9434          
#Specificity : 0.7164          
#Pos Pred Value : 0.8339          
#Neg Pred Value : 0.8934          
#Prevalence : 0.6014          
#Detection Rate : 0.5674          
#Detection Prevalence : 0.6804          
#Balanced Accuracy : 0.8299          

#'Positive' Class : Not_Spam

#--------# Using train and test log of predictors #--------# 
# model.tree and model.tree.log have the exact same results...

ptm <- proc.time() # Start the clock!
set.seed(123)
model.tree.log <- rpart(y ~ ., data = train.log)
model.tree.log
proc.time() - ptm # Stop the clock

dev.new(width=10, height=8) # This fixes the tiny font issue
model.tree_plot.log <- fancyRpartPlot(model.tree.log, sub = "") # The font is very small without the above code
dev.off()

# Predict train
set.seed(123)
model.tree.pred.log <- predict(model.tree.log, newdata = train.log, 
                           type = "prob")[,2]
length(model.tree.pred.log) # 3221
head(model.tree.pred.log)

# Plot ROC curve
set.seed(123)
model.tree.roc.log <- plot.roc(train.log$y, model.tree.pred.log)
model.tree.auc.log <- model.tree.roc.log$auc
model.tree.auc.log # Area under the curve: 0.9223

par(pty = "s") # "s" generates a square plotting region
plot(model.tree.roc.log, col = "steelblue", main = "ROC Curve for log Tree Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
model.tree.pred2.log <- predict(model.tree.log, newdata = train.log)
length(model.tree.pred2.log) # 3221
head(model.tree.pred2.log) # This needs to show just Spam or Not_Spam--shows matrix with probabilities of each prediction
#######
# Make predictions just Spam or Not_Spam
model.tree.pred2.binary.log <- data.frame(model.tree.pred2.log)

model.tree.pred2.binary.log$y <- NA
head(model.tree.pred2.binary.log)
model.tree.pred2.binary.log[,1]

# Convert to factor
model.tree.pred2.binary.log$y <- as.factor(model.tree.pred2.binary.log$y)
levels(model.tree.pred2.binary.log$y) <- c("Not_Spam", "Spam")
summary(model.tree.pred2.binary.log$y)

for (i in 1:nrow(model.tree.pred2.binary.log)){
  model.tree.pred2.binary.log[i,3] <- ifelse(model.tree.pred2.binary.log[i,1] > model.tree.pred2.binary.log[i,2], "Not_Spam", "Spam")
}

head(model.tree.pred2.binary.log)
summary(model.tree.pred2.binary.log$y)
#Not_Spam     Spam 
#2067     1154
#######
set.seed(123)
model.tree.cmat.log <- confusionMatrix(model.tree.pred2.binary.log$y, train.log$y)
model.tree.cmat.log
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1873  194
#Spam           85 1069

#Accuracy : 0.9134          
#95% CI : (0.9031, 0.9229)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.8155          
#Mcnemar's Test P-Value : 1.008e-10       

#Sensitivity : 0.9566          
#Specificity : 0.8464          
#Pos Pred Value : 0.9061          
#Neg Pred Value : 0.9263          
#Prevalence : 0.6079          
#Detection Rate : 0.5815          
#Detection Prevalence : 0.6417          
#Balanced Accuracy : 0.9015          

#'Positive' Class : Not_Spam

# Predict test
set.seed(123)
model.tree.pred.test.log <- predict(model.tree.log, newdata = test.log)
length(model.tree.pred.test.log) # 3221
head(model.tree.pred.test.log) # This needs to show just Spam or Not_Spam
#######
# Make predictions just Spam or Not_Spam
model.tree.pred.test.binary.log <- data.frame(model.tree.pred.test.log)

model.tree.pred.test.binary.log$y <- NA
head(model.tree.pred.test.binary.log)

# Convert to factor
model.tree.pred.test.binary.log$y <- as.factor(model.tree.pred.test.binary.log$y)
levels(model.tree.pred.test.binary.log$y) <- c("Not_Spam", "Spam")
summary(model.tree.pred.test.binary.log$y)

for (i in 1:nrow(model.tree.pred.test.binary.log)){
  model.tree.pred.test.binary.log[i,3] <- ifelse(model.tree.pred.test.binary.log[i,1] > model.tree.pred.test.binary.log[i,2], "Not_Spam", "Spam")
}

head(model.tree.pred.test.binary.log)
summary(model.tree.pred.test.binary.log$y)
#Not_Spam     Spam 
#895      485
#######
set.seed(123)
model.tree.cmat.test.log <- confusionMatrix(model.tree.pred.test.binary.log$y, test.log$y)
model.tree.cmat.test.log
#Reference
#Prediction Not_Spam Spam
#Not_Spam      794  101
#Spam           36  449

#Accuracy : 0.9007         
#95% CI : (0.8837, 0.916)
#No Information Rate : 0.6014         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.7887         
#Mcnemar's Test P-Value : 4.554e-08      

#Sensitivity : 0.9566         
#Specificity : 0.8164         
#Pos Pred Value : 0.8872         
#Neg Pred Value : 0.9258         
#Prevalence : 0.6014         
#Detection Rate : 0.5754         
#Detection Prevalence : 0.6486         
#Balanced Accuracy : 0.8865         

#'Positive' Class : Not_Spam



# -------------------------------------------------------------------------#
# (3) a Support Vector Machine
# Wasn't sure how to create prediction from model using svm()

# Model using train()
ptm <- proc.time() # Start the clock!
svm.control <- trainControl(method = "cv", classProbs = T, savePred = T, verboseIter = T)
#names(getModelInfo())
set.seed(123)
model.svm.CV <- train(y ~ ., data = train, method = "svmRadial", # or svmRadialWeights? -- same results
                    trControl = svm.control) # metric="ROC" doesn't do anything for this model
proc.time() - ptm # Stop the clock
#user  system elapsed 
#56.205   3.504  59.877

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

# Predict train
set.seed(123)
model.svm.CV.pred <- predict(model.svm.CV, newdata = train, 
                          type = "prob")[,2]
length(model.svm.CV.pred) # 3221
head(model.svm.CV.pred)

# Plot ROC curve
set.seed(123)
model.svm.CV.roc <- plot.roc(train$y, model.svm.CV.pred)
model.svm.CV.auc <- model.svm.CV.roc$auc
model.svm.CV.auc # Area under the curve: 0.9857

par(pty = "s") # "s" generates a square plotting region
plot(model.svm.CV.roc, col = "steelblue", main = "ROC Curve for SVM Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
model.svm.CV.pred2 <- predict(model.svm.CV, newdata = train) # no type = "prob"
dim(model.svm.CV.pred2)
head(model.svm.CV.pred2)
set.seed(123)
svm.tune.cmat <- confusionMatrix(model.svm.CV.pred2, train$y)
svm.tune.cmat
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1900   92
#Spam           58 1171

#Accuracy : 0.9534          
#95% CI : (0.9456, 0.9604)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9018          
#Mcnemar's Test P-Value : 0.007051        

#Sensitivity : 0.9704          
#Specificity : 0.9272          
#Pos Pred Value : 0.9538          
#Neg Pred Value : 0.9528          
#Prevalence : 0.6079          
#Detection Rate : 0.5899          
#Detection Prevalence : 0.6184          
#Balanced Accuracy : 0.9488          

#'Positive' Class : Not_Spam


# Predict test
set.seed(123)
model.svm.CV.pred.test <- predict(model.svm.CV, newdata = test)

set.seed(123)
model.svm.CV.cmat.test <- confusionMatrix(model.svm.CV.pred.test, test$y)
model.svm.CV.cmat.test
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam      803   62
#Spam           27  488

#Accuracy : 0.9355          
#95% CI : (0.9212, 0.9479)
#No Information Rate : 0.6014          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.864           
#Mcnemar's Test P-Value : 0.0003134       

#Sensitivity : 0.9675          
#Specificity : 0.8873          
#Pos Pred Value : 0.9283          
#Neg Pred Value : 0.9476          
#Prevalence : 0.6014          
#Detection Rate : 0.5819          
#Detection Prevalence : 0.6268          
#Balanced Accuracy : 0.9274          

#'Positive' Class : Not_Spam


### Do above code using log train and log test sets?


# -------------------------------------------------------------------------#
# (4) Random Forest -- rf_random.fit is best
# Create matrix
ptm <- proc.time() # Start the clock!
set.seed(123)
train.matrix  <- model.matrix(y ~ ., data=train)[,-58] # create predictor matrix (subtract last column, the response variable)
proc.time() - ptm # Stop the clock
ncol(train.matrix) # 57
head(train.matrix)

# Create base model
ptm <- proc.time() # Start the clock!
set.seed(123)
# mtry can be 57 at most because of number of columns in train.matrix
rf_baseline <- randomForest(train.matrix, train$y, mtry=floor(sqrt(ncol(train.matrix))), importance=TRUE)
proc.time() - ptm # Stop the clock
rf_baseline
#Number of trees: 500
#No. of variables tried at each split: 7
#OOB estimate of  error rate: 4.75%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1901   57  0.02911134
#Spam           96 1167  0.07600950

# Create random search RF model
set.seed(123)
control.rf <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
mtry <- sqrt(ncol(train.matrix)) # 7.549834
ptm <- proc.time() # Start the clock!
set.seed(123)
rf_random <- train(train.matrix, train$y, method="rf", metric="Accuracy", trControl=control.rf) # removed tuneLength=15
proc.time() - ptm # Stop the clock
#user  system elapsed 
#920.585   7.467 928.652 

print(rf_random)
#3221 samples
#57 predictor
#2 classes: 'Not_Spam', 'Spam' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 2898, 2900, 2899, 2898, 2900, 2899, ... 
#Resampling results across tuning parameters:

#  mtry  Accuracy   Kappa    
#11    0.9509428  0.8966325
#33    0.9477356  0.8899661
#56    0.9460780  0.8864958

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 11.

plot(rf_random)

# Fit rf_random--performs only slightly better than base model
ptm <- proc.time() # Start the clock!
set.seed(123)
rf_random.fit <- randomForest(train.matrix, train$y, mtry=11, importance=TRUE)
proc.time() - ptm # Stop the clock
#user  system elapsed 
#20.587   0.193  21.130

rf_random.fit
#Number of trees: 500
#No. of variables tried at each split: 11
#OOB estimate of  error rate: 4.72%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1894   64  0.03268641
#Spam           88 1175  0.06967538

varImpPlot(rf_random.fit, main = "Random Forest Model: \n Variable Importance") # How to do in Lattice?

# Predict train
set.seed(123)
rf_random.fit.pred <- predict(rf_random.fit, newdata = train.matrix, 
                         type = "prob")[,2]
length(rf_random.fit.pred) # 3221
head(rf_random.fit.pred)

# Plot ROC curve
set.seed(123)
rf_random.fit.roc <- plot.roc(train$y, rf_random.fit.pred)
rf_random.fit.auc <- rf_random.fit.roc$auc
rf_random.fit.auc # Area under the curve: 0.9993

par(pty = "s") # "s" generates a square plotting region
plot(rf_random.fit.roc, col = "steelblue", main = "ROC Curve for Random Forest Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
rf_random.fit.pred2 <- predict(rf_random.fit, newdata = train.matrix) # no type = "prob"
dim(rf_random.fit.pred2)
head(rf_random.fit.pred2)
set.seed(123)
rf_random.fit.cmat <- confusionMatrix(rf_random.fit.pred2, train$y)
rf_random.fit.cmat
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1957    3
#Spam            1 1260

#Accuracy : 0.9988          
#95% CI : (0.9968, 0.9997)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.9974          
#Mcnemar's Test P-Value : 0.6171          

#Sensitivity : 0.9995          
#Specificity : 0.9976          
#Pos Pred Value : 0.9985          
#Neg Pred Value : 0.9992          
#Prevalence : 0.6079          
#Detection Rate : 0.6076          
#Detection Prevalence : 0.6085          
#Balanced Accuracy : 0.9986          

#'Positive' Class : Not_Spam

# Predict test
# For test predictions, must create test.matrix
set.seed(123)
test.matrix <- model.matrix(y ~ ., data=test)[,-58]
ncol(test.matrix) # 57
head(test.matrix)

set.seed(123)
rf_random.fit.pred.test <- predict(rf_random.fit, newdata = test.matrix)

set.seed(123)
rf_random.fit.cmat.test <- confusionMatrix(rf_random.fit.pred.test, test$y)
rf_random.fit.cmat.test
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam      808   51
#Spam           22  499

#Accuracy : 0.9471          
#95% CI : (0.9339, 0.9583)
#No Information Rate : 0.6014          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.8887          
#Mcnemar's Test P-Value : 0.001049        

#Sensitivity : 0.9735          
#Specificity : 0.9073          
#Pos Pred Value : 0.9406          
#Neg Pred Value : 0.9578          
#Prevalence : 0.6014          
#Detection Rate : 0.5855          
#Detection Prevalence : 0.6225          
#Balanced Accuracy : 0.9404          

#'Positive' Class : Not_Spam 

#--------#

# Create RF train using train()
ptm <- proc.time() # Start the clock!
rf.control.cvr <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T, savePred = T, verboseIter = T)
proc.time() - ptm # Stop the clock

ptm <- proc.time() # Start the clock!
set.seed(123)
model.rf <- train(y ~ ., data = train, method = "rf", trControl = rf.control.cvr)
#Fitting mtry = 29 on full training set
proc.time() - ptm # Stop the clock
#user  system elapsed 
#893.115   9.903 903.818

model.rf$finalModel # performs worse than randomForest() method
#Number of trees: 500
#No. of variables tried at each split: 29

#OOB estimate of  error rate: 5.03%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1892   66  0.03370787
#Spam           96 1167  0.07600950

# Predict train
set.seed(123)
model.rf.pred <- predict(model.rf, newdata = train, 
                                  type = "prob")[,2]
length(model.rf.pred) # 3221
head(model.rf.pred)

# Plot ROC curve
set.seed(123)
model.rf.roc <- plot.roc(train$y, model.rf.pred)
model.rf.auc <- model.rf.roc$auc
model.rf.auc # Area under the curve: 1

par(pty = "s") # "s" generates a square plotting region
plot(model.rf.roc, col = "steelblue", main = "ROC Curve for Random Forest Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
model.rf.pred2 <- predict(model.rf, newdata = train) # no type = "prob"
dim(model.rf.pred2)
head(model.rf.pred2)
set.seed(123)
model.rf.cmat <- confusionMatrix(model.rf.pred2, train$y)
model.rf.cmat
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam     1957    1
#Spam            1 1262

#Accuracy : 0.9994          
#95% CI : (0.9978, 0.9999)
#No Information Rate : 0.6079          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.9987          
#Mcnemar's Test P-Value : 1               

#Sensitivity : 0.9995          
#Specificity : 0.9992          
#Pos Pred Value : 0.9995          
#Neg Pred Value : 0.9992          
#Prevalence : 0.6079          
#Detection Rate : 0.6076          
#Detection Prevalence : 0.6079          
#Balanced Accuracy : 0.9993          

#'Positive' Class : Not_Spam 

# Predict test
set.seed(123)
model.rf.pred.test <- predict(model.rf, newdata = test)

set.seed(123)
model.rf.cmat.test <- confusionMatrix(model.rf.pred.test, test$y)
model.rf.cmat.test
#Confusion Matrix and Statistics
#Reference
#Prediction Not_Spam Spam
#Not_Spam      805   58
#Spam           25  492

#Accuracy : 0.9399         
#95% CI : (0.926, 0.9518)
#No Information Rate : 0.6014         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.8733         
#Mcnemar's Test P-Value : 0.000444       

#Sensitivity : 0.9699         
#Specificity : 0.8945         
#Pos Pred Value : 0.9328         
#Neg Pred Value : 0.9516         
#Prevalence : 0.6014         
#Detection Rate : 0.5833         
#Detection Prevalence : 0.6254         
#Balanced Accuracy : 0.9322         

#'Positive' Class : Not_Spam


### Do above code using log train and log test sets?


#######################################################
# End
#######################################################

#--------# Discard #--------#
# Code for one lattice boxplot
crla <- bwplot(~ capital_run_length_average | y, data = data,
               layout = c(2, 1),
               par.settings = list(
                 box.umbrella=list(col= "black"), 
                 box.dot=list(col= "black"), 
                 box.rectangle = list(col= "black", fill = "steelblue")),
               strip = strip.custom(bg="lightgrey"))
class(crla)

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


# Create ROC curve
# https://www.r-bloggers.com/roc-curves-in-two-lines-of-r-code/
#simple_roc <- function(labels, scores){
#  labels <- labels[order(scores, decreasing=TRUE)]
#  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
#}
#simple_roc(train$y, model.logit.step2.pred2)
# Error in Math.factor(labels) : ‘cumsum’ not meaningful for factors 


my.roc <- function(df,actual,probability){
  # http://freakonometrics.hypotheses.org/9066
  roc.curve=function(t,print=FALSE){
    p=probability
    a=actual
    Pt=(p>t)*1
    FP=sum((Pt==1)*(a==0))/sum(a==0) # false positive
    TP=sum((Pt==1)*(a==1))/sum(a==1) # true positive
    if(print==TRUE){
      print(table(Observed=a,Predicted=Pt))
    }
    vect=c(FP,TP)
    names(vect)=c("FPR","TPR")
    return(vect)
  }
  threshold = 0.5
  roc.curve(threshold,print=TRUE)
  
  ROC.curve=Vectorize(roc.curve)
  
  M.ROC=ROC.curve(seq(0,1,by=.01))
  plot(M.ROC[1,],M.ROC[2,],type="l",xlab = "1 - Specificity", ylab = "Sensitivity")
  abline(0,1, col="gray", lty=2)
  
  # https://www.kaggle.com/c/GiveMeSomeCredit/forums/t/942/r-script-for-auc-calculation
  my.auc<-function(df,actual,probability){
    N=length(probability)
    N_pos=sum(actual)
    df=data.frame(out=actual,prob=probability)
    df=df[order(-df$prob),]
    df$above=(1:N)-cumsum(df$out)
    return(1-sum(df$above*df$out)/(N_pos*(N-N_pos)))
  }
  my.auc(df,actual,probability)
}

my.roc(train, train$y, model.logit.step2.pred)
#           Predicted
#Observed      0    1
#Not_Spam   1866   92
#Spam        137 1126

# Accuracy = (TP + TN)/(TP + FP + TN + FN)
accuracy <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  (TP + TN)/(TP + FP + TN + FN)
}

# Classification Error Rate = (FP + FN)/(TP + FP + TN + FN)
class.error.rate <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  (FP + FN)/(TP + FP + TN + FN)
}

# Verify that you get an accuracy and an error rate that sums to one.
accuracy.out <- accuracy(train, train$y, model.logit.step2.pred)
class.error.rate.out <- class.error.rate(train, train$y, model.logit.step2.pred)
accuracy.out + class.error.rate.out # 1

# Precision = TP/(TP + FP)
precision <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  TP/(TP + FP)
}

# Sensitivity = TP/(TP + FN)       ## aka True Positive Rate (TPR)
sensitivity <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  TP/(TP + FN)
}

# Specificity = TN/(TN + FP)       ## aka True Negative Rate (TNR)
specificity <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  TN/(TN + FP)
}

# F1 Score = (2*precision*sensitivity)/(precision + sensitivity)
f1.score <- function(df, actual, predicted){
  (2*precision(df, actual, predicted)*sensitivity(df, actual, predicted))/(precision(df, actual, predicted) + sensitivity(df, actual, predicted))
}

my.roc(train, train$y, model.logit.step2.pred) # 0.8503113

accuracy(train, train$y, model.logit.step2.pred) # 0.9971831
class.error.rate(train, train$y, model.logit.step2.pred) # 0.002816901
precision(train, train$y, model.logit.step2.pred) # 0
sensitivity(train, train$y, model.logit.step2.pred) # NaN
specificity(train, train$y, model.logit.step2.pred) # 0.9971831
f1.score(train, train$y, model.logit.step2.pred) # NaN


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

# rf_random.fit results on 2/3
#Number of trees: 500
#No. of variables tried at each split: 6
#OOB estimate of  error rate: 4.59%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1901   57  0.02911134
#Spam           91 1172  0.07205067

# rf_random.fit results on 2/4
#Number of trees: 500
#No. of variables tried at each split: 6
#OOB estimate of  error rate: 4.75%
#Confusion matrix:
#         Not_Spam Spam class.error
#Not_Spam     1901   57  0.02911134
#Spam           96 1167  0.07600950

#if else(model.tree.pred2.binary[i,1] > model.tree.pred2.binary[i,2], model.tree.pred2.binary[i,3]=="0", model.tree.pred2.binary[i,3]=="1")}

#######
model.tree.pred2.binary <- data.frame(model.tree.pred2)

model.tree.pred2.binary$y <- NA
head(model.tree.pred2.binary)
model.tree.pred2.binary[,1]

# Convert to factor
model.tree.pred2.binary$y <- as.factor(model.tree.pred2.binary$y)
levels(model.tree.pred2.binary$y) <- c("Not_Spam", "Spam")
summary(model.tree.pred2.binary$y)

for (i in 1:nrow(model.tree.pred2.binary)){
  model.tree.pred2.binary[i,3] <- ifelse(model.tree.pred2.binary[i,1] > model.tree.pred2.binary[i,2], "Not_Spam", "Spam")
}

head(model.tree.pred2.binary)
summary(model.tree.pred2.binary$y)
#Not_Spam     Spam 
#2067     1154
#######

#ptm <- proc.time() # Start the clock!
#rf.control.cv <- trainControl(method = "cv", classProbs = T, savePred = T, verboseIter = T)
#proc.time() - ptm # Stop the clock

#ptm <- proc.time() # Start the clock!
#set.seed(123)
#model.rf2 <- train(y ~ ., data = train, method = "rf", trControl = rf.control.cv)
#proc.time() - ptm # Stop the clock
#Fitting mtry = 29 on full training set

ptm <- proc.time() # Start the clock!
set.seed(123)
model.svm <- svm(y~., data=train, kernel ="linear")
proc.time() - ptm # Stop the clock
#user  system elapsed 
#1.675   0.047   1.723 
summary(model.svm)

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1 
#gamma:  0.01754386 
#Number of Support Vectors:  678
#( 326 352 )
#Number of Classes:  2 
#Levels: 
#  Not_Spam Spam

# Tune the model
ptm <- proc.time() # Start the clock!
set.seed(123)
svm.tune <- tune(svm,y~.,data=train, kernel ="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
proc.time() - ptm # Stop the clock
#   user  system elapsed 
#120.986   2.206 123.570
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


# Predict train
set.seed(123)
model.svm.pred <- predict(model.svm, newdata = train, 
                          type = "prob")[,2]
length(model.svm.pred) # 3221
head(model.svm.pred)

# Plot ROC curve
set.seed(123)
svm.tune.roc <- plot.roc(train$y, svm.tune.pred)
svm.tune.auc <- msvm.tune.roc$auc
svm.tune.auc # Area under the curve: 0.9781

par(pty = "s") # "s" generates a square plotting region
plot(svm.tune.roc, col = "steelblue", main = "ROC Curve for Stepwise Logistic Regression Model")
par(pty = "m") # "m" generates the maximal plotting region

# Predict train for confusion matrix
set.seed(123)
svm.tune.pred2 <- predict(svm.tune, newdata = train) # no type = "prob"
dim(svm.tune.pred2)
head(svm.tune.pred2)
set.seed(123)
svm.tune.cmat <- confusionMatrix(svm.tune.pred2, train$y)
svm.tune.cmat

# Predict test
set.seed(123)
svm.tune.pred.test <- predict(svm.tune, newdata = test)

set.seed(123)
svm.tune.cmat.test <- confusionMatrix(svm.tune.pred.test, test$y)
svm.tune.cmat.test

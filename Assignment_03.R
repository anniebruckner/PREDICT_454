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
                      ,"pROC")

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

# First set completely naive logistic model without using variable selection
#set.seed(123)
#model.logit <- glm(y ~ ., data = train, family=binomial("logit"))
#Warning message:
#  glm.fit: fitted probabilities numerically 0 or 1 occurred 
#summary(model.logit)

# (1) a logistic regression model using variable selection

# Backward train
set.seed(123)
model.logit.bwd <- regsubsets(y ~ ., data = train, nvmax=NULL, method="backward")
options(max.print=1000000)
summary(model.logit.bwd)
names(model.logit.bwd) # Is there a metric that lets me see which is best?
#which.min(model.logit.bwd$rss) # 58 
#model.logit.bwd$rss[58] # 330.9772
#plot(model.logit.bwd, scale="bic")

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

# Model using top 15 predictors from Stepwise -- left out glm and using family=binomial("logit")
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
# (2) a tree model
# Model A
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
model.tree.pred2 <- predict(model.tree, newdata = train, type = "class")
length(model.tree.pred) # 3221
head(model.tree.pred) # This needs to show just Spam or Not_Spam
set.seed(123)
model.tree.cmat <- confusionMatrix(model.tree.pred2, train$y)
model.tree.cmat
#Error in confusionMatrix.default(model.tree.pred.test, test$y) : 
#  the data cannot have more levels than the reference

# Predict test
set.seed(123)
model.tree.pred.test <- predict(model.tree, newdata = test)
set.seed(123)
model.tree.cmat.test <- confusionMatrix(model.tree.pred.test, test$y)
model.tree.cmat.test









# Model tree with 10-fold CV x 3
ptm <- proc.time() # Start the clock!
control.tree <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T, savePred = T, verboseIter = T)
set.seed(123)
model.tree2 <- train(y ~ ., data = train, method = "rpart", trControl = control.tree)
proc.time() - ptm # Stop the clock

# Fitting cp = 0.0435 on full training set
model.tree2$finalModel
fancyRpartPlot(model.tree2$finalModel, sub = "")

# -------------------------------------------------------------------------#
# (3) a Support Vector Machine
# Model A
ptm <- proc.time() # Start the clock!
set.seed(123)
model.svm <- svm(y~., data=train, kernel ="linear", cost =1)
proc.time() - ptm # Stop the clock
summary(model.svm)
plot(model.svm , train$y)

ptm <- proc.time() # Start the clock!
set.seed(123)
svm.tune <- tune(svm,y~.,data=train, kernel ="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
proc.time() - ptm # Stop the clock
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
ptm <- proc.time() # Start the clock!
svm.control <- trainControl(method = "cv", classProbs = T, savePred = T, verboseIter = T)
#names(getModelInfo())
set.seed(123)
model.svm.CV <- train(y ~ ., data = train, method = "svmRadial", # or svmRadialWeights? -- same results
                    trControl = svm.control) # metric="ROC" doesn't do anything for this model
proc.time() - ptm # Stop the clock

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

# -------------------------------------------------------------------------#
# (4) Random Forest
# Model A
ptm <- proc.time() # Start the clock!
set.seed(123)
train.matrix  <- model.matrix(y ~ ., data=train)[,-58] # create predictor matrix (subtract last column, the response variable)
proc.time() - ptm # Stop the clock
ncol(train.matrix) # 57
head(train.matrix)

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

# Random Search
ptm <- proc.time() # Start the clock!
set.seed(123)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
mtry <- sqrt(ncol(train.matrix)) # 7.549834
set.seed(123)
rf_random <- train(train.matrix, train$y, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
proc.time() - ptm # Stop the clock
print(rf_random)
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
ptm <- proc.time() # Start the clock!
set.seed(123)
fit4 <- randomForest(train.matrix, train$y, mtry=6, importance=TRUE)
proc.time() - ptm # Stop the clock
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

ptm <- proc.time() # Start the clock!
set.seed(123)
fit4a <- randomForest(train.matrix, train$y, mtry=24, importance=TRUE)
fit4a # OOB estimate of  error rate: 5.06%
proc.time() - ptm # Stop the clock

# Model B
ptm <- proc.time() # Start the clock!
rf.control <- trainControl(method = "cv", classProbs = T, savePred = T, verboseIter = T)
proc.time() - ptm # Stop the clock

ptm <- proc.time() # Start the clock!
rf.control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T, savePred = T, verboseIter = T)
proc.time() - ptm # Stop the clock

ptm <- proc.time() # Start the clock!
set.seed(123)
model.rf <- train(y ~ ., data = train, method = "rf", trControl = rf.control)
proc.time() - ptm # Stop the clock
#Fitting mtry = 29 on full training set

ptm <- proc.time() # Start the clock!
set.seed(123)
model.rf2 <- train(y ~ ., data = train, method = "rf", trControl = rf.control2)
proc.time() - ptm # Stop the clock
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




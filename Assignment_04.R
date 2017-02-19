# Andrea Bruckner
# Predict 454
# Assignment 4

# All data and attribute names for the wine data are located here:
# http://archive.ics.uci.edu/ml/machine-learning-databases/wine/

#######################################################
# Workspace Setup
#######################################################

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
                      ,"bestglm"
                      ,"neuralnet")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load all packages
lapply(list.of.packages, require, character.only = TRUE)

# Read data
wine <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header=F)
head(wine)

# Create column names
colnames(wine) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Ash_Alcalinity", 
                    "Magnesium", "Total_Phenols", "Flavanoids", 
                    "Nonflavanoid_Phenols", "Proanthocyanins", 
                    "Color_Intensity", "Hue", "OD280_OD315", "Proline")

head(wine)

#######################################################
# Data Quality Check
#######################################################

# Maybe normalize the data: https://medium.com/autonomous-agents/how-to-train-your-neuralnetwork-for-wine-tasting-1b49e0adff3a#.1tpn0wvzb

# Explore the data -- how big is it, what types of variables included, distributions and missing values.
class(wine) # data.frame
dim(wine) # 178  14
nrow(wine) # 178 rows
ncol(wine) # 14 columns/variables
names(wine)
str(wine) # all num except Class, Magnesium, and Proline are int
summary(wine) # no NA/missing values
# Perhaps Proline has some outliers--the difference between the 3rd quartile and max value is large
# and is the same as the amount between the 3rd quartile and the min value (~700)

# Make Magnesium and Proline numeric instead of integers (since it will be easier to work with all the same data type)
wine$Magnesium <- as.numeric(wine$Magnesium)
wine$Proline <-as.numeric(wine$Proline)

# Ensure changes to data types worked
str(wine)

# Plot the variables
plot_vars <- function (data, column){
  ggplot(data = wine, aes_string(x = column)) +
    geom_histogram(color =I("black"), fill = I("steelblue"))+
    xlab(column) + theme_bw() + theme(axis.title=element_text(size=8, face="bold"))
}

plots <- lapply(colnames(wine), plot_vars, data = wine)
length(plots)
do.call("grid.arrange", c(plots, nrow=2))

# Make Class a factor since it takes only 3 values (representing 3 different cultivars/orgins of wined)
## Must run above plot code before changing Class to a factor since geom_histogram can't plot factors--geom_bar can but looks worse.
wine$Class <- as.factor(wine$Class)

# Ensure changes to Class worked
str(wine)

# Examine quantiles of wine variables
#lapply(wine[2:14], quantile, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
sapply(wine[2:14], quantile, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)) # more readable

# Create plots of Proline to examine potential outliers
histogram(~ Proline, data = wine, col = "steelblue")

Proline_box <- bwplot(~ Proline, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

# Create more box plots of variables to examine potential outliers indicated in histogram
Ash_box <- bwplot(~ Ash, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

Flavanoids_box <- bwplot(~ Flavanoids, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

Hue_box <- bwplot(~ Hue, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

grid.arrange(Proline_box, Ash_box, Flavanoids_box, Hue_box, ncol=4)

# Create boxplots for all variables--need to figure out how to print variables on xlab               
for (i in 2:14){
  toPlot <- paste0("class ~ ", names(wine[i])
                   p <- bwplot(as.formula(toPlot), data = wine, par.settings = list(
                     box.umbrella=list(col= "black"), 
                     box.dot=list(col= "black"), 
                     box.rectangle = list(col= "black", fill = "steelblue")),
                     xlab = paste(names(i)))
                   print(p)
}

## Doesn't work still...
for (i in 2:14){
  toPlot = paste0("class ~ ",names(wine)[i])
  p <- bwplot(as.formula(toPlot), data = wine, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")))
  print(p)
}

# Hue, Color_Intensity, Proanthocyanins, Magnesium, Ash_Alcalinity, Ash, Malic_Acid all show potential outliers

# Plot additional variables with potential outliers
Color_Intensity_box <- bwplot(~ Color_Intensity, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

Proanthocyanins_box <- bwplot(~ Proanthocyanins, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

Magnesium_box <- bwplot(~ Magnesium, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

Ash_Alcalinity_box <- bwplot(~ Ash_Alcalinity, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

Malic_Acid_box <- bwplot(~ Malic_Acid, data = wine, par.settings = list(
  box.umbrella=list(col= "black"), 
  box.dot=list(col= "black"), 
  box.rectangle = list(col= "black", fill = "steelblue")))

grid.arrange(Malic_Acid_box, Ash_Alcalinity_box, Magnesium_box, Proanthocyanins_box, Color_Intensity_box, ncol=5)

#######################################################
# EDA
#######################################################

# Examine correlations among just numeric variables
c <- cor(wine[2:14], use="complete.obs")

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

# Create object containing names of only numeric variables
noClass <- colnames(wine[2:14])

# Visualize correlations between numeric variables and each Class factor
corrplot(cor(wine[wine$Class == 1, noClass]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         title = "Wine Class 1 Correlations",
         mar=c(1,3,1,3))

corrplot(cor(wine[wine$Class == 2, noClass]), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         title = "Wine Class 2 Correlations",
         mar=c(1,3,1,3))

corrplot(cor(wine[wine$Class == 3, noClass]), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         title = "Wine Class 3 Correlations",
         mar=c(1,3,1,3))

# Create plots of just a few significant variables in relation to Class
Flavanoids_Class_hist <- histogram(~ Flavanoids | Class, data = wine, 
                                   layout = c(3, 1), col = "steelblue", strip = strip.custom(bg="lightgrey"))

Flavanoids_Class_box <- bwplot(~ Flavanoids | Class, data = wine,
                               layout = c(3, 1),
                               par.settings = list(
                                 box.umbrella=list(col= "black"), 
                                 box.dot=list(col= "black"), 
                                 box.rectangle = list(col= "black", fill = "steelblue")),
                               strip = strip.custom(bg="lightgrey"))

grid.arrange(Flavanoids_Class_hist, Flavanoids_Class_box, ncol=2)

OD280_OD315_Class_hist <- histogram(~ OD280_OD315 | Class, data = wine, 
                                    layout = c(3, 1), col = "steelblue", strip = strip.custom(bg="lightgrey"))

OD280_OD315_Class_box <- bwplot(~ OD280_OD315 | Class, data = wine,
                                layout = c(3, 1),
                                par.settings = list(
                                  box.umbrella=list(col= "black"), 
                                  box.dot=list(col= "black"), 
                                  box.rectangle = list(col= "black", fill = "steelblue")),
                                strip = strip.custom(bg="lightgrey"))

grid.arrange(OD280_OD315_Class_hist, OD280_OD315_Class_box, ncol=2)

# Create tree model
fancyRpartPlot(rpart(Class ~ ., data = wine), sub = "")

#######################################################
# Model-Based EDA
#######################################################

# Create tree model (same as above)
fancyRpartPlot(rpart(Class ~ ., data = wine), sub = "")

# Create PCA model
wine$Class <- as.numeric(wine$Class) # must change Class to numeric to model
model.pca <- prcomp(wine, scale = T) # prcomp is preferred to princomp for accuracy
summary(model.pca)
par(mfrow=c(1,2))
screeplot(model.pca, type = c("lines"), main = "PCA Model", sub = "Number of Components") # 4 components explain most of variability in the data
biplot(model.pca, xlabs = wine[, "Class"], xlim=c(-0.20, 0.20))

# Reset plot display
par(mfrow=c(1,1))

# Change Class back to factor for LDA model
wine$Class = as.factor(wine$Class)

# Create LDA model
model.lda <- lda(Class ~ ., data = wine)
plot(model.lda, main = "LDA Model", cex = 0.90)

# Use backward subset selection on model.lda
model.lda.bwd<-regsubsets(Class~ .,data = wine, nvmax=13, method="backward")
summary(model.lda.bwd)

# Create second LDA model using top 3 selected variables
model.lda2 <- lda(Class ~ Flavanoids + Proline + Color_Intensity, data = wine)
plot(model.lda2)

# Create second LDA model using top 6 selected variables
model.lda3 <- lda(Class ~ Flavanoids + Proline + Color_Intensity + Ash_Alcalinity + OD280_OD315 + Alcohol, data = wine)
plot(model.lda3)

# Create second LDA model using top 10 selected variables
model.lda4 <- lda(Class ~ Flavanoids + Proline + Color_Intensity + Ash_Alcalinity + OD280_OD315 + Alcohol + Total_Phenols + Ash + Malic_Acid + Nonflavanoid_Phenols, data = wine)
plot(model.lda4)

# Create Random Forest model
set.seed(123)
model.RF <- randomForest(Class~., data = wine, mtry=13, ntree =25)
importance(model.RF)
varImpPlot(model.RF, main = "Random Forest Model: \n Variable Importance") # How to do in Lattice?

#######################################################
# Build Models
#######################################################

# (1) Random Forest

# Create Random Forest model
set.seed(123)
model.RF <- randomForest(Class~., data = wine, mtry=13, ntree =25)
importance(model.RF)
#MeanDecreaseGini
#Alcohol                 11.6206021409
#Malic_Acid               1.1164253670
#Ash                      0.5279954467
#Ash_Alcalinity           1.1887627480
#Magnesium                0.4291428571
#Total_Phenols            0.0000000000
#Flavanoids              25.1380480765
#Nonflavanoid_Phenols     0.0000000000
#Proanthocyanins          0.8726242957
#Color_Intensity         22.6599585177
#Hue                      1.6122292878
#OD280_OD315             19.3411866391
#Proline                 32.0224628257

varImpPlot(model.RF, main = "Random Forest Model: \n Variable Importance")

model.RF.pred <- predict(model.RF, newdata = wine[, -1])
model.RF.c.mat <- confusionMatrix(model.RF.pred, wine$Class)
names(model.RF.c.mat) # "positive" "table"    "overall"  "byClass"  "mode"     "dots"
model.RF.c.mat$table
#           Reference
#Prediction  1  2  3
#          1 59  0  0
#          2  0 71  0
#          3  0  0 48


levels(wine$Class) <- c("Class_1", "Class_2", "Class_3")
# Did the above code because I was getting the following error when I ran the model.rf:
# Error in train.default(x, y, weights = w, ...) : 
#At least one of the class levels is not a valid R variable name;
#This will cause errors when class probabilities are generated because
#the variables names will be converted to  X1, X2, X3 . Please use
#factor levels that can be used as valid R variable names  (see ?make.names for help).

ptm <- proc.time() # Start the clock!
control.rf <- trainControl(method = "repeatedcv",
                             number = 10, repeats = 3,
                             classProbs = T, savePred = T, verboseIter = T)

set.seed(123)
model.rf <- train(x = wine[, -1], y = wine[, 1], data = wine, method="rf", trControl = control.rf)
proc.time() - ptm # Stop the clock
#    user  system elapsed 
#8.638   0.464   9.169


#model.rf2 <- train(Class ~ ., data = wine, method="rf", trControl = control.rf)
#user  system elapsed 
#8.788   0.410   9.277

print(model.rf) # model.rf and model.rf2 have the same results--just checking
#Random Forest
#178 samples
#13 predictor
#3 classes: 'Class_1', 'Class_2', 'Class_3' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 160, 160, 161, 160, 162, 160, ... 
#Resampling results across tuning parameters:
  
#mtry  Accuracy      Kappa       
#2    0.9866851565  0.9798843968
#7    0.9775118966  0.9660151983
#13    0.9661693040  0.9486707565

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 2.

model.rf$finalModel
#randomForest(x = x, y = y, mtry = param$mtry, data = ..1) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 1.12%
#Confusion matrix:
#Class_1 Class_2 Class_3   class.error
#Class_1      59       0       0 0.00000000000
#Class_2       1      69       1 0.02816901408
#Class_3       0       0      48 0.00000000000

# why the confusion matrices don't match: http://stats.stackexchange.com/questions/175236/why-does-randomforest-confusion-matrix-not-match-the-one-i-calculate-using-predi

model.rf.pred <- predict(model.rf, newdata = wine[, -1])
model.rf.c.mat <- confusionMatrix(model.rf.pred, wine[, 1])
names(model.rf.c.mat) # "positive" "table"    "overall"  "byClass"  "mode"     "dots"
model.rf.c.mat$table
#Prediction Class_1 Class_2 Class_3
#Class_1      59       0       0
#Class_2       0      71       0
#Class_3       0       0      48

model.rf.c.mat$overall
#Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#1.000000e+00   1.000000e+00   9.794892e-01   1.000000e+00   3.988764e-01   8.896633e-72            NaN 

varImpPlot(model.rf$finalModel, main = "Random Forest Model: \n Variable Importance") # How to do in Lattice?
plot(varImp(model.rf))

ptm <- proc.time() # Start the clock!
control.rf.oob <- trainControl(method = "oob",
                           classProbs = T, savePred = T, verboseIter = T)

set.seed(123)
model.rf.oob <- train(x = wine[, -1], y = wine[, 1], data = wine, method="rf", trControl = control.rf.oob)
proc.time() - ptm # Stop the clock
#   user  system elapsed 
#0.848   0.085   0.942

print(model.rf.oob)
#Random Forest 

#178 samples
#13 predictor
#3 classes: 'Class_1', 'Class_2', 'Class_3' 

#No pre-processing
#Resampling results across tuning parameters:
  
#  mtry  Accuracy      Kappa       
#2    0.9887640449  0.9829608003
#7    0.9775280899  0.9659395331
#13    0.9719101124  0.9573775202

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 2.

model.rf.oob$finalModel
#Call:
#  randomForest(x = x, y = y, mtry = param$mtry, data = ..1) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 1.69%
#Confusion matrix:
#  Class_1 Class_2 Class_3   class.error
#Class_1      59       0       0 0.00000000000
#Class_2       1      68       2 0.04225352113
#Class_3       0       0      48 0.00000000000

model.rf.oob.pred <- predict(model.rf.oob, newdata = wine[, -1])
model.rf.oob.c.mat <- confusionMatrix(model.rf.oob.pred, wine[, 1])
names(model.rf.oob.c.mat) # "positive" "table"    "overall"  "byClass"  "mode"     "dots"
model.rf.oob.c.mat$table
#Prediction Class_1 Class_2 Class_3
#Class_1      59       0       0
#Class_2       0      71       0
#Class_3       0       0      48

model.rf.oob.c.mat$overall
#Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#1.000000e+00   1.000000e+00   9.794892e-01   1.000000e+00   3.988764e-01   8.896633e-72            NaN 



# -------------------------------------------------------------------------#
# (2) a Support Vector Machine
ptm <- proc.time() # Start the clock!
svm.control <- trainControl(method = "repeatedcv",
                            number = 10, repeats = 3,
                            classProbs = T, savePred = T, verboseIter = T)
#names(getModelInfo())
set.seed(123)
model.svm <- train(Class ~ ., data = wine, method = "svmRadial", # or svmRadialWeights?
                      trControl = svm.control)
proc.time() - ptm # Stop the clock
#user  system elapsed 
#7.545   0.298   7.901 

print(model.svm)
#Support Vector Machines with Radial Basis Function Kernel 

#178 samples
#13 predictor
#3 classes: 'Class_1', 'Class_2', 'Class_3' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 160, 160, 161, 160, 162, 160, ... 
#Resampling results across tuning parameters:
  
#  C     Accuracy   Kappa    
#0.25  0.9755740454  0.9627100236
#0.50  0.9774258973  0.9655402123
#1.00  0.9792777491  0.9683704010

#Tuning parameter 'sigma' was held constant at a value of 0.07665756
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were sigma = 0.07665756 and C = 1.

model.svm$finalModel
#Support Vector Machine object of class "ksvm" 

#SV type: C-svc  (classification) 
#parameter : cost C = 1 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.0766575562502905 

#Number of Support Vectors : 69 

#Objective Function Value : -12.1162 -4.5981 -12.5011 
#Training error : 0 
#Probability model included.

model.svm.pred <- predict(model.svm, newdata = wine[, -1])
model.svm.c.mat <- confusionMatrix(model.svm.pred, wine[, 1])
model.svm.c.mat$table
#Prediction Class_1 Class_2 Class_3
#Class_1      59       0       0
#Class_2       0      71       0
#Class_3       0       0      48

model.svm.c.mat$overall
#Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#1.000000e+00   1.000000e+00   9.794892e-01   1.000000e+00   3.988764e-01   8.896633e-72            NaN 

plot(varImp(model.svm),layout = c(1, 3), strip = strip.custom(bg="lightgrey"))

#x = wine[, -1], y = wine[, 1]
ptm <- proc.time() # Start the clock!
svm.control <- trainControl(method = "repeatedcv",
                            number = 10, repeats = 3,
                            classProbs = T, savePred = T, verboseIter = T)
#names(getModelInfo())
set.seed(123)
model.svm2 <- train(x = wine[, -1], y = wine[, 1], data = wine, method = "svmRadial", # or svmRadialWeights?
                   trControl = svm.control)
proc.time() - ptm # Stop the clock
#user  system elapsed 
#7.545   0.298   7.901 

print(model.svm2)
#Support Vector Machines with Radial Basis Function Kernel 

#178 samples
#13 predictor
#3 classes: 'Class_1', 'Class_2', 'Class_3' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 160, 160, 161, 160, 162, 160, ... 
#Resampling results across tuning parameters:

#  C     Accuracy   Kappa    
#0.25  0.9755740454  0.9627100236
#0.50  0.9774258973  0.9655402123
#1.00  0.9792777491  0.9683704010

#Tuning parameter 'sigma' was held constant at a value of 0.07665756
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were sigma = 0.07665756 and C = 1.

model.svm2$finalModel
#Support Vector Machine object of class "ksvm" 

#SV type: C-svc  (classification) 
#parameter : cost C = 1 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.0766575562502905 

#Number of Support Vectors : 69 

#Objective Function Value : -12.1162 -4.5981 -12.5011 
#Training error : 0 
#Probability model included.

model.svm2.pred <- predict(model.svm2, newdata = wine[, -1])
model.svm2.c.mat <- confusionMatrix(model.svm2.pred, wine[, 1])
model.svm2.c.mat$table
#Prediction Class_1 Class_2 Class_3
#Class_1      59       0       0
#Class_2       0      71       0
#Class_3       0       0      48

model.svm2.c.mat$overall
#Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#1.000000e+00   1.000000e+00   9.794892e-01   1.000000e+00   3.988764e-01   8.896633e-72            NaN 

# -------------------------------------------------------------------------#
# (3) a neural network model
ptm <- proc.time() # Start the clock!
nnet.control <- trainControl(method = "repeatedcv",
                            number = 10, repeats = 3,
                            classProbs = T, savePred = T, verboseIter = T)
#names(getModelInfo())
set.seed(123)
model.nnet <- train(Class ~ ., data = wine, method = "nnet",
                   trControl = nnet.control)
proc.time() - ptm # Stop the clock
#user  system elapsed 
#6.788   0.549   7.416  

print(model.nnet)
#Neural Network 

#178 samples
#13 predictor
#3 classes: 'Class_1', 'Class_2', 'Class_3' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 160, 160, 161, 160, 162, 160, ... 
#Resampling results across tuning parameters:
  
#  size  decay  Accuracy   Kappa     
#1     0.0000  0.3992539560  0.00000000000
#1     0.0001  0.3992539560  0.00000000000
#1     0.1000  0.6439900814  0.42087424613
#3     0.0000  0.4085132152  0.01559934319
#3     0.0001  0.4177724745  0.03147297944
#3     0.1000  0.9068598785  0.85408918106
#5     0.0000  0.4237637599  0.04210671530
#5     0.0001  0.4270317337  0.04635330768
#5     0.1000  0.9242224229  0.88112962105

#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were size = 5 and decay = 0.1. 

model.nnet$finalModel
#a 13-5-3 network with 88 weights
#inputs: Alcohol Malic_Acid Ash Ash_Alcalinity Magnesium Total_Phenols Flavanoids Nonflavanoid_Phenols Proanthocyanins Color_Intensity Hue OD280_OD315 Proline 
#output(s): .outcome 
#options were - softmax modelling  decay=0.1

model.nnet.pred <- predict(model.nnet, newdata = wine[, -1])
model.nnet.c.mat <- confusionMatrix(model.nnet.pred, wine[,1])
model.nnet.c.mat$table
#Prediction Class_1 Class_2 Class_3
#Class_1      59       0       0
#Class_2       0      71       0
#Class_3       0       0      48

model.nnet.c.mat$overall
#Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
#9.943820e-01   9.914612e-01   9.690977e-01   9.998578e-01   3.988764e-01   2.395450e-69            NaN

plot(model.nnet)

#######################################################
# END
#######################################################

# How to get neuralnet to work with factors: https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
# Encode as a one hot vector multilabel data
wine <- cbind(wine[, 2:14], class.ind(as.factor(wine$Class)))

# Set up formula
n <- names(wine)
f <- as.formula(paste("Class_1 + Class_2 + Class_3 ~", paste(n[!n %in% c("Class_1","Class_2","Class_3")], collapse = " + ")))
f

set.seed(123)
model.nnet2 <- neuralnet(f, data = wine, hidden=2, err.fct="sse", linear.output=FALSE)
model.nnet2$result.matrix
#                                   1
#error                              29.691533166661
#reached.threshold                   0.007680079688
#steps                            1873.000000000000
#Intercept.to.1layhid1              -0.560475646552
#Alcohol.to.1layhid1                -0.230177489483
#Malic_Acid.to.1layhid1              1.558708314149
#Ash.to.1layhid1                     0.070508391425
#Ash_Alcalinity.to.1layhid1          0.129287735161
#Magnesium.to.1layhid1               1.715064986883
#Total_Phenols.to.1layhid1           0.460916205989
#Flavanoids.to.1layhid1             -1.265061234607
#Nonflavanoid_Phenols.to.1layhid1   -0.686852851894
#Proanthocyanins.to.1layhid1        -0.445661970100
#Color_Intensity.to.1layhid1         1.224081797439
#Hue.to.1layhid1                     0.359813827057
#OD280_OD315.to.1layhid1             0.400771450594
#Proline.to.1layhid1                 0.110682715945
#Intercept.to.1layhid2              -1.471207499528
#Alcohol.to.1layhid2                 1.676009100557
#Malic_Acid.to.1layhid2              4.062152525901
#Ash.to.1layhid2                    -2.169840310304
#Ash_Alcalinity.to.1layhid2         -3.018885193119
#Magnesium.to.1layhid2              -0.313648150140
#Total_Phenols.to.1layhid2           1.829345601937
#Flavanoids.to.1layhid2              4.549266173528
#Nonflavanoid_Phenols.to.1layhid2  -14.161074197383
#Proanthocyanins.to.1layhid2        -0.658618882272
#Color_Intensity.to.1layhid2        -1.477108545396
#Hue.to.1layhid2                    -3.637280246460
#OD280_OD315.to.1layhid2             2.238205793393
#Proline.to.1layhid2                 0.069026263826
#Intercept.to.Class_1               -3.248905434374
#1layhid.1.to.Class_1               -0.856953576292
#1layhid.2.to.Class_1               12.912692632755
#Intercept.to.Class_2               -0.418262885912
#1layhid.1.to.Class_2                0.771934258125
#1layhid.2.to.Class_2               -4.857663244921
#Intercept.to.Class_3               -0.101402634479
#1layhid.1.to.Class_3               -0.234343462017
#1layhid.2.to.Class_3             -181.327636586456

plot(model.nnet2)

# Compute predictions
#model.nnet2.pred <- compute(model.nnet2, wine[, 1:13])

# Extract results
#pr.nn_ <- pr.nn$net.result
#head(pr.nn_)

#model.nnet2.pred <- predict(model.nnet2, newdata = wine[, -1])
#model.nnet2.c.mat <- confusionMatrix(model.nnet2.pred, wine$Class)
#model.nnet2.c.mat$table


#m <- model.matrix(~ Class, data = wine)

#model.nnet3 <- neuralnet(Class_1 + Class_2 + Class_3 ~ Alcohol+Malic_Acid+Ash+Ash_Alcalinity+Magnesium+Total_Phenols+Flavanoids
#                         +Nonflavanoid_Phenols+Proanthocyanins+Color_Intensity+Hue+OD280_OD315+Proline,
#                         data = m, hidden=2, err.fct="sse", linear.output=FALSE)

# Andrea Bruckner
# Predict 454
# Assignment 4

# All data and attribute names for the wine data are located here:
# http://archive.ics.uci.edu/ml/machine-learning-databases/wine/

#######################################################
# Workspace Setup
#######################################################

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
                      ,"caret")

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
model.RF <- randomForest(Class~., data = wine, mtry=13, ntree =25)
importance(model.RF)
varImpPlot(model.RF, main = "Random Forest Model: \n Variable Importance") # How to do in Lattice?

#######################################################
# Build Models
#######################################################

# (1) Random Forest

# Create Random Forest model
model.RF <- randomForest(Class~., data = wine, mtry=13, ntree =25)
importance(model.RF)
varImpPlot(model.RF, main = "Random Forest Model: \n Variable Importance") # How to do in Lattice?

levels(wine$Class) <- c("class_1", "class_2", "class_3")
# Did the above code because I was getting the following error when I ran the model:
# Error in train.default(x, y, weights = w, ...) : 
#At least one of the class levels is not a valid R variable name;
#This will cause errors when class probabilities are generated because
#the variables names will be converted to  X1, X2, X3 . Please use
#factor levels that can be used as valid R variable names  (see ?make.names for help).

control.rf <- trainControl(method = "repeatedcv",
                             number = 10, repeats = 3,
                             classProbs = T, savePred = T, verboseIter = T)

ptm <- proc.time() # Start the clock!
set.seed(123)
model.rf <- train(x = wine[, -1], y = wine[, 1], data = wine, method="rf", trControl = control.rf)
#    user  system elapsed 
#9.330   0.469   9.872

#model.rf2 <- train(Class ~ ., data = wine, method="rf", trControl = control.rf)
proc.time() - ptm # Stop the clock
#user  system elapsed 
#8.788   0.410   9.277

print(model.rf) # model.rf and model.rf2 have the same results--just checking
#178 samples
#13 predictor
#3 classes: 'class_1', 'class_2', 'class_3' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 160, 160, 161, 160, 162, 160, ... 
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa    
#2    0.9866852  0.9798844
#7    0.9775119  0.9660152
#13    0.9661693  0.9486708

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 2.

model.rf$finalModel
#randomForest(x = x, y = y, mtry = param$mtry, data = ..1) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 1.12%
#Confusion matrix:
#  class_1 class_2 class_3 class.error
#class_1      59       0       0  0.00000000
#class_2       1      69       1  0.02816901
#class_3       0       0      48  0.00000000

model.rf.pred <- predict(model.rf, newdata = wine[, -1])
model.rf.c.mat <- confusionMatrix(model.rf.pred, wine$Class)
names(model.rf.c.mat) # "positive" "table"    "overall"  "byClass"  "mode"     "dots"
model.rf.c.mat$table
#Prediction class_1 class_2 class_3
#class_1      59       0       0
#class_2       0      71       0
#class_3       0       0      48

model.rf.c.mat$overall


# (2) a Support Vector Machine
ptm <- proc.time() # Start the clock!
svm.control <- trainControl(method = "repeatedcv",
                            number = 10, repeats = 3,
                            classProbs = T, savePred = T, verboseIter = T)
#names(getModelInfo())
set.seed(123)
model.svm.CV <- train(Class ~ ., data = wine, method = "svmRadial", # or svmRadialWeights? -- same results
                      trControl = svm.control) # metric="ROC" doesn't do anything for this model
proc.time() - ptm # Stop the clock
#user  system elapsed 
#56.205   3.504  59.877


# (3) a neural network model

#######################################################
# END
#######################################################
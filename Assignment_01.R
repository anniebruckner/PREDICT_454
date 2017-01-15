# Andrea Bruckner
# Predict 454
# Assignment 1

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
                      ,"lattice")

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

# Make Class a factor since it takes only 3 values (maybe representing low, medium, and high wine class/quality)
## Must run above plot code before changing Class to a factor since geom_histogram can't plot factors--geom_bar can but looks worse.
wine$Class <- as.factor(wine$Class)

# Ensure changes to Class worked
str(wine)

# Examine quantiles of wine variables
#lapply(wine[2:14], quantile, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
sapply(wine[2:14], quantile, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)) # more readable

# Create plots of Proline to examine potential outliers
histogram(~ Proline, data = wine, col = "steelblue")

bwplot(~ Proline, data = wine, par.settings = list(
                     box.umbrella=list(col= "black"), 
                     box.dot=list(col= "black"), 
                     box.rectangle = list(col= "black", fill = "steelblue")))

# Create boxplots for all variables--need to figure out how to print variables on xlab               
for (i in wine[2:14]){
  p <- bwplot(~ i, data = wine, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")),
    xlab = paste(names(i)))
  print(p)
}

#######################################################
# EDA
#######################################################

histogram(~ OD280_OD315 | Class, data = wine, 
          layout = c(3, 1), col = "steelblue")

bwplot(~ Nonflavanoid_Phenols | Class, data = wine, # uses lattice to create trellis boxplots
       layout = c(3, 1))

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

# Visualize correlations between numberic variables and each Class factor
corrplot(cor(wine[wine$Class == 1, noClass]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         title = "Wine Class 1 Correlations",
         mar=c(3,5,3,3))

corrplot(cor(wine[wine$Class == 2, noClass]), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         title = "Wine Class 2 Correlations",
         mar=c(3,5,3,3))

corrplot(cor(wine[wine$Class == 3, noClass]), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         title = "Wine Class 3 Correlations",
         mar=c(3,5,3,3))

# Create tree model
fancyRpartPlot(rpart(Class ~ ., data = wine), sub = "")

#######################################################
# Model-Based EDA
#######################################################

# Create PCA model
wine$Class <- as.numeric(wine$Class) # must change Class to numeric to model
model.pca <- prcomp(wine, scale = T) # prcomp is preferred to princomp for accuracy
summary(model.pca)
screeplot(model.pca, type = c("lines"), main = "PCA", sub = "Number of Components")
biplot(model.pca, xlabs = wine[, "Class"])

# Change Class back to factor for LDA model
wine$Class = as.factor(wine$Class)

# Create LDA model
model.lda <- lda(Class ~ ., data = wine)
plot(model.lda)

# Use backward subset selection on model.log1b
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
varImpPlot(model.RF, main = "Random Forest Model: \n Variable Importance")

#######################################################
# END
#######################################################
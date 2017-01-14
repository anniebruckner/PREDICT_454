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

# Make Class a factor since it takes only 3 values (maybe representing low, medium, and high wine class/quality)
wine$Class <- as.factor(wine$Class)

# Make Magnesium and Proline numeric instead of integers (since it will be easier to work with all the same data type)
wine$Magnesium <- as.numeric(wine$Magnesium)
wine$Proline <-as.numeric(wine$Proline)

# Ensure changes to data types worked
str(wine)

# Examine quantiles of wine variables
lapply(wine[2:14], quantile, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
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




for (i in wine[2:14]){
  p <- bwplot(~ i, data = wine, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")),
    xlab = for (j in wine$[i]){
      paste(j)
    }
    )
  print(p)
}


lapply(wine[2:14], bwplot, data = wine)

# Plot the variables
wineBoxplots <- function (data, column){
  ggplot(data = wine, aes_string(x = column, y = wine$Class)) +
    geom_boxplot(color =I("black"), fill = I("steelblue"))+
    xlab(column) + theme_bw() + theme(axis.title=element_text(size=8, face="bold"))
}

boxplots <- lapply(colnames(wine), wineBoxplots, data = wine)
length(plots)
do.call("grid.arrange", c(boxplots, nrow=7))

#######################################################
# EDA
#######################################################

# Plot the variables
plot_vars <- function (data, column){
  ggplot(data = wine, aes_string(x = column)) +
    geom_histogram(color =I("black"), fill = I("steelblue"))+
    xlab(column) + theme_bw() + theme(axis.title=element_text(size=8, face="bold"))
}

plots <- lapply(colnames(wine), plot_vars, data = wine)
length(plots)
do.call("grid.arrange", c(plots, nrow=2))

histogram(~ OD280_OD315 | Class, data = wine, 
          layout = c(3, 1), col = "steelblue")

bwplot(~ Nonflavanoid_Phenols | Class, data = wine, # uses lattice to create trellis boxplots
       layout = c(3, 1))

# Create object of wine excluding Class
noClass <- colnames(wine[sapply(wine, is.numeric)])

## Visualize correlations
corrplot(cor(wine[wine$Class == 1, noClass]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45)

corrplot(cor(wine[wine$Class == 2, noClass]), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 45)

corrplot(cor(wine[wine$Class == 3, noClass]), 
         tl.col = "black", tl.cex = 0.7, tl.srt = 45)

# Examine correlations among just numeric variables
C <- cor(wine[sapply(wine, is.numeric)], use="complete.obs")

correlations <- data.frame(cor(wine[sapply(wine, is.numeric)],use="complete.obs"))

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

fancyRpartPlot(rpart(wine$Class ~ ., data = wine), sub = "")

#######################################################
# Model-Based EDA
#######################################################

model.lda <- lda(Class ~ ., data = wine)
summary(model.lda)
plot(model.lda)

wine$Class = as.numeric(wine$Class)
model.pca = prcomp(wine, scale = T)
biplot(model.pca, xlabs = wine[, "Class"])
wine$Class = as.factor(wine$Class)

#Use backward subset selection on model.log1b
model.regfit.bwd<-regsubsets(Class~ .,data = wine, nvmax=13, method="backward")
summary(model.regfit.bwd)

model.lda2 <- lda(Class ~ Flavanoids + Proline + Color_Intensity + Ash_Alcalinity + OD280_OD315 + Alcohol, data = wine)
summary(model.lda2)
plot(model.lda2)

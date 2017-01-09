# Andrea Bruckner
# Predict 454
# Assignment 1

# All data and attribute names for the wine data are located here:
# http://archive.ics.uci.edu/ml/machine-learning-databases/wine/

# Install Packages if they don't current exist on this machine
list.of.packages <- c("doBy"
                      ,"lazyeval"
                      ,"psych"
                      ,"lars"
                      ,"GGally"
                      ,"ggplot2"
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

# Explore the data -- how big is it, what types of variables included, distributions and missing values.
dim(wine) # 178  14
summary(wine) # no NA/missing values
str(wine) # all num except Class, Magnesium, and Proline are int
class(wine) # data.frame
nrow(wine) # 178 rows
ncol(wine) # 14 columns/variables
names(wine)

plots <- vector("list", 14)
names <- colnames(wine)

# Visualize the variables
plot_vars <- function (data, column)
  ggplot(data = wine, aes_string(x = column)) +
  geom_histogram(color =I('black'), fill = I('steelblue'))+
  xlab(column)

plots <- lapply(colnames(wine), plot_vars, data = wine)

n <- length(plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plots))

# This shows the plots as a 2x7 display
ml <- marrangeGrob(grobs = plots, nrow = 2, ncol = 7)
ml




# Andrea Bruckner
# Predict 454
# Assignment 2

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

#######################################################
# EDA
#######################################################

# Creat tree plot
fancyRpartPlot(rpart(data$price ~ ., data = data), sub = "")

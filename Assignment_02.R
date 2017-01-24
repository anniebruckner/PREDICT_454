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
                      ,"caret"
                      ,"plyr"
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

# Create scatter plot for price~carat
xyplot(price~carat, data = data, col = "steelblue")

#######################################################
# EDA
#######################################################

# Calculate correlation between price and carat
cor(data$price, data$carat) # 0.8796315

# Examine quantiles of carat
quantile((data$carat), probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))

# Create plots of price by factor predictor variables
## Do LAPPLY, SAPPLY, or FOR LOOP for these plots ##

# color
histogram(~price | color, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"))
bwplot(~price | color, data = data,
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

# clarity
histogram(~price | clarity, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"))
bwplot(~price | clarity, data = data,
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

# cut
histogram(~price | cut, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"))
bwplot(~price | cut, data = data,
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

# channel
histogram(~price | channel, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"),
          layout = c(3, 1))
bwplot(~price | channel, data = data,
       layout = c(3, 1),
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

# store
histogram(~price | store, data = data,
          col = "steelblue", strip = strip.custom(bg="lightgrey"))
bwplot(~price | store, data = data,
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

# Create tree plot
fancyRpartPlot(rpart(price ~ ., data = data), sub = "")

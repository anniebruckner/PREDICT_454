# Andrea Bruckner
# Predict 454
# Assignment 2

#######################################################
# Workspace Setup
#######################################################

# Reset plot display
par(mfrow=c(1,1))

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
summary(data) # no missing data

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
str(data)

# Create log transform of predictors just in case
predictors.log < - log(data[1:57]) ### DOESN"T WORK
head(data[1:57])

# Create scatterplot matrix 
splom(data[1:57], main="Spam Data")

# Create histograms for all predictor variables
for (i in 1:57){
  toPlot = paste0("y ~ ", names(data)[i])
  p <- histogram(as.formula(toPlot), data = data, col = "steelblue",  xlab = names(data)[i])
  print(p)
}

# Create barcharts for all predictor variables
for (i in 1:57){
  toPlot = paste0("y ~ ", names(data)[i])
  p <- barchart(as.formula(toPlot), data = data, col = "steelblue",  xlab = names(data)[i])
  print(p)
}

# Create boxplots for all predictor variables except carat
for (i in 1:57){
  toPlot = paste0("y ~ ", names(data)[i])
  p <- bwplot(as.formula(toPlot), data = data, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")),
    xlab = names(data)[i])
  print(p)
}

###### END

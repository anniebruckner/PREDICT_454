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
barchart(data$y, col = "steelblue")

# Create log transformations of all predictors
pred.log <- lapply(data[1:57], log)
pred.log <- data.frame(pred.log, y = data$y)
head(pred.log)

summary(data)

# Create naive tree models
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
significant.correlations

# Create object containing names of only predictor variables
pred <- colnames(data[1:57])

# Visualize correlations between predictors and response
corrplot(cor(data[data$y == "Not_Spam", pred]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         mar=c(1,1,2,2), type = "lower", main = "Correlations for Not_Spam")

corrplot(cor(data[data$y == "Spam", pred]),
         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
         mar=c(1,1,2,2), type = "lower", main = "Correlations for Spam")

# Plot histograms of the variables--How can I do this using lattice?
plot_vars <- function (data, column){
  ggplot(data = data, aes_string(x = column)) +
    geom_histogram(color =I("black"), fill = I("steelblue"))+
    xlab(column) + theme_bw() + theme(axis.title=element_text(size=8, face="bold"))
}

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

# Create box plots of each predictor according to response -- How do I automatically arrange the plots into a grid?
plot_box <- function (df){
for (i in 1:57){
  toPlot = paste0("~ ", names(data)[i], " | y")
  p <- bwplot(as.formula(toPlot), data = data, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")),
    strip = strip.custom(bg="lightgrey"),
    xlab = names(data)[i])
  print(p)
}
}

plotsE <- lapply(data[55:57], FUN=plot_box)
length(plotsE)
do.call("grid.arrange", c(plotsE, ncol=3)) # Error: only 'grobs' allowed in "gList" == how to fix?

#--------#
# Code for one lattice boxplot
bwplot(~ capital_run_length_average | y, data = data,
       layout = c(2, 1),
       par.settings = list(
         box.umbrella=list(col= "black"), 
         box.dot=list(col= "black"), 
         box.rectangle = list(col= "black", fill = "steelblue")),
       strip = strip.custom(bg="lightgrey"))

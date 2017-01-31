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

######

fac.freq = function(df.fac, df.fac.cn, cat = T){
  table.results = data.frame()
  # Check df.fac is factor
  if (!class(df.fac) %in% c("factor")){
    stop("Please supply a factor variable to df.fac")
  }
  # Assign data.frame and name
  temp = unlist(strsplit(deparse(substitute(df.fac)),
                         split = "$", fixed = T))
  df = eval(as.name(paste(temp[1])))
  fac = temp[2]
  # Check if df.fac.cn is missing or named (and class if named)
  if (missing(df.fac.cn)){
    cols = colnames(df[, sapply(df, is.factor)])
  } else if (!class(df.fac.cn) %in% c("factor")){
    stop("Please supply a factor variable to df.fac.cn")
  } else {
    cols = unlist(strsplit(deparse(substitute(df.fac.cn)),
                           split = "$", fixed = T))[2]
  }
  # Factor splits
  if (cat){
    for (i in cols){
      name.var = rep(paste(i), each = nlevels(df[, fac]))
      name.split = rep(paste(fac), each = nlevels(df[, fac]))
      table.level = levels(df[, fac])
      table.agg = aggregate(df[, i], by = list(Var = df[, fac]),
                            summary)$x
      table.prop = format(round(prop.table(table.agg, 1) * 100,
                                digits = 2), nsmall = 2)
      table.results = as.data.frame(cbind(name.var, name.split,
                                          table.level, table.prop))
      colnames(table.results)[1] = "Variable"
      colnames(table.results)[2] = "Split On"
      colnames(table.results)[3] = "Levels"
      if (missing(df.fac.cn)){
        print(table.results)
      } else {
        return(table.results)
      }
    }
  }
  # Factor counts and frequencies
  if (!cat){
    name.var = rep(paste(fac), each = 2)
    name.type = c("Count", "Percent")
    table.agg = t(summary(df[, fac]))
    table.prop = format(round(prop.table(table.agg) * 100,
                              digits = 2), nsmall = 2)
    table.row = rbind(table.agg, table.prop)
    table.col = cbind(name.var, name.type, table.row)
    table.results = as.data.frame(table.col)
    colnames(table.results)[1] = "Variable"
    colnames(table.results)[2] = "Type"
    return(table.results)
  }
}


spam.dist = fac.freq(data$y, cat = F)


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

# Create scatterplot matrix -- takes way too long -- did significant correlations instead
# splom(data[1:57], main="Spam Data")

# Create histograms for all predictor variables
for (i in 1:57){
  toPlot = paste0("y ~ ", names(data)[i])
  p <- histogram(as.formula(toPlot), data = data, col = "steelblue",  xlab = names(data)[i])
  print(p)
}

# Create object containing names of only predictor variables
pred <- colnames(data[1:57])

# Visualize correlations between numeric variables and responses variable -- NAs + too many variables cause issues
#corrplot(cor(data[data$y == 0, pred]),
#         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
#         title = "Not Spam Correlations",
#         mar=c(1,3,1,3))

#corrplot(cor(data[data$y == 1, pred]),
#         tl.col = "black", tl.cex = 0.7, tl.srt = 45,
#         title = "Spam Correlations",
#         mar=c(1,3,1,3))



# Create barcharts for all predictor variables
for (i in 1:57){
  toPlot = paste0("y ~ ", names(data)[i])
  p <- barchart(as.formula(toPlot), data = data, col = "steelblue",  xlab = names(data)[i])
  print(p)
}

# Create boxplots for all predictors
for (i in 1:57){
  toPlot = paste0("y ~ ", names(data)[i])
  p <- bwplot(as.formula(toPlot), data = data, par.settings = list(
    box.umbrella=list(col= "black"), 
    box.dot=list(col= "black"), 
    box.rectangle = list(col= "black", fill = "steelblue")),
    xlab = names(data)[i])
  print(p)
}

# Create histograms for all predictors
for (i in data[1:57]){
  toPlot = paste0(names(data)[i])
p <- histogram(as.formula(toPlot), data = data, col = "steelblue",
    xlab = names(data)[i])
  print(p)
}
names(data[1])


# Plot the variables--How can I do this using lattice?
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

plot_vars2 <- function (data, column){
  ggplot(data = data, aes_string(x = column, y = data$y)) +
    geom_boxplot(color =I("black"), fill = I("steelblue"))+
    xlab(column) + theme_bw() + theme(axis.title=element_text(size=8, face="bold"))
}

plotsD2 <- lapply(colnames(data[55:57]), plot_vars2, data = data)
length(plotsD2)
do.call("grid.arrange", c(plotsD2, ncol=3))

#cb <- bwplot(y~capital_run_length_total, data = data,
#             par.settings = list(
#               box.umbrella=list(col= "black"), 
#               box.dot=list(col= "black"), 
#               box.rectangle = list(col= "black", fill = "steelblue")),
#             strip = strip.custom(bg="lightgrey"))
#cb

#myPlots <- function(variable){
#  histogram(~y | variable, data = data,
#                  col = "steelblue", strip = strip.custom(bg="lightgrey"),
#                  main = variable)
#}

#lapply(data[c(1:57)],FUN=myPlots)

h1<-histogram(~char_freq_pound, data = pred.log, col = "steelblue")
class(h1)

cb <- bwplot(y~capital_run_length_total, data = data,
par.settings = list(
box.umbrella=list(col= "black"), 
box.dot=list(col= "black"), 
box.rectangle = list(col= "black", fill = "steelblue")),
strip = strip.custom(bg="lightgrey"))
cb


show.settings()

tp <- trellis.par.get()

unusual <- c("grid.pars", "fontsize", "clip", "axis.components",
             "layout.heights", "layout.widths")

for (u in unusual) tp[[u]] <- NULL
names.tp <- lapply(tp, names)
unames <- sort(unique(unlist(names.tp)))
ans <- matrix(0, nrow = length(names.tp), ncol = length(unames))
rownames(ans) <- names(names.tp)
colnames(ans) <- unames
for (i in seq(along = names.tp))
  ans[i, ] <- as.numeric(unames %in% names.tp[[i]])
ans <- ans[, order(-colSums(ans))]
ans <- ans[order(rowSums(ans)), ]
ans[ans == 0] <- NA

levelplot(t(ans), colorkey = FALSE, 
          scales = list(x = list(rot = 90)),
          panel = function(x, y, z, ...) {
            panel.abline(v = unique(as.numeric(x)), 
                         h = unique(as.numeric(y)), 
                         col = "darkgrey")
            panel.xyplot(x, y, pch = 16 * z, ...)
          },
          xlab = "Graphical parameters", 
          ylab = "Setting names")


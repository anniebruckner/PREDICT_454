# Andrea Bruckner
# Predict 454
# Assignment 1

# All data and attribute names for the wine data are located here:
# http://archive.ics.uci.edu/ml/machine-learning-databases/wine/

# Read data
wine <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header=F)

head(wine)
# Create column names
colnames(wine) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Ash_Alcalinity", 
                    "Magnesium", "Total_Phenols", "Flavanoids", 
                    "Nonflavanoid_Phenols", "Proanthocyanins", 
                    "Color_Intensity", "Hue", "OD280_OD315", "Proline")

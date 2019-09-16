#Breast Cancer Data Transformation and Analysis
#Mansi Gupta mgupta313@gatech.edu
#Created 09.15.2019
#Dataset:  Breast Cancer Wisconsin (Diagnostic) Data Set - (https://www.kaggle.com/uciml/breast-cancer-wisconsin-data)

#Contains the following checkpoints:
#Data transformations
#Explatory data analysis
#Data models
#Data visualization

#set your working directory where the data is stored:
setwd(choose.dir())

#install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
#install.packages("skimr") # a package to facilitate data summaries
#install.packages("Hmisc") # a package for data analysis

library(tidyverse)
library(skimr)
library(Hmisc)
library(ggplot2)

#Loading data into the console:
cancerData <- read.csv(file.choose(), header=TRUE)

#Cleaning data by removing missing values:
cleanCancerData <- cancerData[complete.cases(cancerData),]

#Selecting data by removing columns containing “worst” values for each variable: 
selectedCancerData <- select(cancerData, id, diagnosis, radius_mean,texture_mean, perimeter_mean, area_mean, smoothness_mean, compactness_mean, concave.points_mean, symmetry_mean, fractal_dimension_mean)
view(selectedCancerData)

#Making histograms to compare important parameters for M and B:
ggplot(data=cancerData, aes(x=radius_mean, fill=diagnosis)) + geom_histogram()
ggplot(data=cancerData, aes(x=concavity_mean, fill=diagnosis)) + geom_histogram()
ggplot(data=cancerData, aes(x=fractal_dimension_mean, fill=diagnosis)) + geom_histogram()

#Analysing the observed values for each parameter using box plots:
cancerData %>% ggplot(aes(diagnosis, y = radius_mean)) +geom_boxplot()
cancerData %>% ggplot(aes(diagnosis, y = concavity_mean)) +geom_boxplot()
cancerData %>% ggplot(aes(diagnosis, y = fractal_dimension_mean)) +geom_boxplot()

#Creating a data model to understand the effects of the different variables on the diagnosis of the tumorous cell mass
#Questions to ask:
#How much does the radius of the cell nuclei affect the cell mass to become malignant?
#Are the concavity of the cell and malignancy of the cell mass directly proportional?

#Variables:
#Independent variables - radius_mean, concavity_mean, fractal_dimension_mean
#Dichotmous dependent variable - diagnosis (M=malign, B=benign)

# A frequency distribution of malignant and beningn tumors
table(cancerData$diagnosis)

#This analysis uses logistic (aka logit) regression to determine the effect of one or more independent variables on a dichotomous dependent variable (with values like yes or no; true or false; 1 or 0). 
cancerModelTest <- glm(diagnosis ~ radius_mean + concavity_mean + fractal_dimension_mean, family=binomial(link="logit"), data = cancerData)
summary(cancerModelTest)

#Exponential function computes factor changes in the odds for every independent variable in the model
round(exp(coef(cancerModelTest)),digits=2)

#Visulaization of the model using scatter plot
modelVis <- ggplot(cancerModelTest, aes(radius_mean, concavity_mean, shape=factor(diagnosis)))
modelVis + geom_point(aes(colour  =  factor(diagnosis)), size  =  3) + geom_point(colour  =  "grey90", size  =  1)



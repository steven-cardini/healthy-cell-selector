##############################################
# Script to train decision tree classifiers and evaluate it on other datasets
# Author: Steven Cardini
# Spring 2017
##############################################

library(dplyr)
library(rpart)
library(rpart.plot)

setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")


###########################################################
# Global variables

experiment.id <- createExperimentId()

input.training.file.name <- '20170117_test_multipulses_100_50_10_2percent_100ms_interval_5_ver2_manual.csv'
input.training.class.arg <- 'mid.in.man'
input.testing.files.name <- c('20170228_test_multipulses_30min_break.csv', '20170228_test_multipulses_45min_break.csv', '20170228_test_multipulses_60min_break.csv')
input.testing.class.arg <- 'mid.in'

input.file.alias <- c('dataset.training', 'dataset.test.30min', 'dataset.test.45min', 'dataset.test.60min')
names(input.file.alias) <- c(input.training.file.name, input.testing.files.name)
#file.output.path <- 'dt.output.csv'


###########################################################
# Prepare training dataset for decision tree

dat.training.raw <- importRawDataset (input.training.file.name, input.training.class.arg)

dat.training.dt.in <- importTrainingDataset (input.training.file.name, input.training.class.arg)
dat.training.dt.in <- dat.training.dt.in %>% 
  mutate_at(funs(replace(., . == TRUE, 'healthy')), .cols = input.training.class.arg) %>% 
  mutate_at(funs(replace(., . == FALSE, 'unhealthy')), .cols = input.training.class.arg)


###########################################################
# Decision Tree (package rpart) - Gini split method
# http://www.statmethods.net/advstats/cart.html
# https://www.r-bloggers.com/classification-trees-using-the-rpart-function/

# Create formula from class variable for the decision tree
fRpart <- as.formula(paste(input.training.class.arg, '.', sep=" ~ "))

# Construct decision tree
dt.gini <- rpart(formula = fRpart, data=dat.training.dt.in, parms=list(split="gini"))
summary(dt.gini)

# Evaluate decision tree with training dataset
data.test.0 <- importTestDataset(input.training.file.name, input.training.class.arg)
classes.actual.0 <- importActualClassLabels(input.training.file.name, input.training.class.arg, relabel = FALSE)
classes.eval.0 <- evaluateDecisionTree (dt.gini, data.test.0, classes.actual.0, input.file.alias[input.training.file.name])


# TODO

############################################################################



# Evaluate decision tree with training dataset
pred.gini <- predict(dt.gini, type="class")
sprintf("Confusion Matrix for dt.gini")
table(pred.gini, dat.dt.input[,input.training.class.arg])

predicted <- predict(dt.gini, dat.dt.input %>% dplyr::select(-get(input.training.class.arg)))
predicted <- (predicted[,1] > 0.5)

# Write output data to CSV

dat.predicted.gini <- dat.training.raw %>% dplyr::select(-get(input.training.class.arg))
dat.temp <- data.frame(c1 = as.integer(names(predicted)), c2 = predicted)
colnames(dat.temp) <- c(dat.cellid.arg, input.training.class.arg)
dat.predicted.gini <- full_join(dat.predicted.gini, dat.temp, by = dat.cellid.arg)
write.csv(dat.predicted.gini, file.output.path)

# Evaluate decision tree with test dataset(s)






# Build the decision tree according to Information split method

#dt.info <- rpart(mid.in~., data=dat.dt.input, parms=list(split="information"))
dt.info <- rpart(mid.in~., data=dat.dt.input, parms=list(split="information"), control=rpart.control(cp = 0.01, minsplit = 8))
prp(dt.info, extra = 2, under = TRUE, varlen = 0)
summary(dt.info)

pred.info <- predict(dt.info, type="class")
sprintf("Confusion Matrix for dt.info")
table(pred.info, dat.dt.input$mid.in)

predicted <- predict(dt.info, dat.dt.predict)
predicted <- (predicted[,1] > 0.5)

dat.raw.predicted.info <- data.table(dat.raw)
dat.raw.predicted.info[, (cell.class.arg) := predicted[get(cell.id.arg)]]


# Build decision tree (RWeka)
# http://data-mining.business-intelligence.uoc.edu/home/j48-decision-tree
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Packages/RWeka/Weka_classifier_trees
# dt2 <- J48(mid.in~., data=dt.avgData.scaled)
# summary(dt2)


# Plots
require(ggplot2)
require(plotly)
plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg <- 'objCell_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg <- dat.cellid.arg
plot.color.arg <- dat.class.arg

# plot original data set
doScatterPlot (dat, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)
# plot prediction according to decision tree (Gini)
doScatterPlot (dat.predicted.gini, plot.x.arg, plot.y.arg, dat.cellid.arg, plot.color.arg)
# plot prediction according to decision tree (information)
doScatterPlot (dat.raw.predicted.info, plot.x.arg, plot.y.arg, dat.cellid.arg, plot.color.arg)


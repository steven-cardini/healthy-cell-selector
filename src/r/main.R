##############################################################################
# Script to train decision tree classifiers and evaluate it on testing datasets
# Author: Steven Cardini
# Spring 2017
##############################################################################

setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/func.general.R")
source("src/r/func.dataimport.R")
source("src/r/func.evaluation.R")
# general R libraries
library(dplyr)
library(data.table) 
library(ggplot2)
library(plotly)
# libraries for Decision Tree
library(rpart)
library(rpart.plot)
# library for Naive Bayes
require(klaR)


#################################################
# EXPERIMENT PARAMETERS
#################################################

G.feat.timediffs <- TRUE
G.feat.scale <- TRUE
G.label.outliers.mode <- 'global'
G.feat.aggr.fun <- c('var', 'mean', 'min', 'max')

train.data.info <- list(
  file.name = '20170117_test_multipulses_100_50_10_2percent_100ms_interval_5_ver2_manual.csv',
  file.alias = 'dataset.training',
  class.attr = 'mid.in.man',
  label.outliers = 'global', # global: look at all features together, individual: look at features individually, none: no relabeling
  upper.bound = 0.9992,
  lower.bound = 0.0008 # kicks out 21 cells, if label.outliers = global
)

test.data.1.info <- list(
  file.name = '20170228_test_multipulses_30min_break.csv',
  file.alias = 'dataset.test.30min',
  class.attr = 'mid.in',
  label.outliers = 'none',
  upper.bound = NA,
  lower.bound = NA
)

test.data.2.info <- list(
  file.name = '20170228_test_multipulses_45min_break.csv',
  file.alias = 'dataset.test.45min',
  class.attr = 'mid.in',
  label.outliers = 'none',
  upper.bound = NA,
  lower.bound = NA
)

test.data.3.info <- list(
  file.name = '20170228_test_multipulses_60min_break.csv',
  file.alias = 'dataset.test.60min',
  class.attr = 'mid.in',
  label.outliers = 'none',
  upper.bound = NA,
  lower.bound = NA
)


#################################################
# CONSTANTS
#################################################

G.cellid.arg <- 'cell_Id' # custom name for the new cell id attribute
G.id.args.vec <- c('Image_Metadata_Site', 'objNuc_TrackObjects_Label')
G.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
G.timepoint.arg <- 'RealTime'

G.feature.include.regex <- '^objCell_Intensity_MeanIntensity_imErkCorrOrig$|^objNuc_Intensity_MeanIntensity_imNucCorrBg$|^objCell_AreaShape*'
G.feature.exclude.regex <- '.*(EulerNumber)+.*'

G.data.files.path <- 'data/'
G.result.files.path <- 'results/'

G.plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
G.plot.y.arg <- 'objCell_Intensity_MeanIntensity_imErkCorrOrig'


#################################################
# EVALUATION ROUTINE
#################################################

# Import training dataset for decision tree and change class labels
data.training <- getFeatureDataset(train.data.info)


######################
# Decision Tree
######################

initializeExperiment('DecisionTree')

# Create formula from class variable for the decision tree
dt.formula <- as.formula(paste(train.data.info$class.attr, '.', sep=" ~ "))
dt.params <- list(
  split="gini"
)


# To weigh the classes see: https://stackoverflow.com/questions/8717493/using-minsplit-and-unequal-weights-in-rpart
# Construct decision tree model
dt.model <- rpart(formula = dt.formula, data = data.training, parms = dt.params)

# Save experiment information
dt.params.string <- paste0(paste(names(dt.params), dt.params, sep = ' = '), collapse = ' / ')
saveExperimentInfo(dt.params.string)
saveDecisionTree(dt.model)

# Evaluate decision tree with training dataset
succ.rate.0 <- evaluateModel(dt.model, train.data.info)

# Evaluate decision tree with test datasets
succ.rate.1 <- evaluateModel(dt.model, test.data.1.info)
succ.rate.2 <- evaluateModel(dt.model, test.data.2.info)
succ.rate.3 <- evaluateModel(dt.model, test.data.3.info)

# Print the mean success rate as info
succ.rate.mean <- mean(succ.rate.1, succ.rate.2, succ.rate.3)
sprintf('Mean Success Rate: %f', succ.rate.mean)


######################
# Naive Bayes
######################

initializeExperiment('NaiveBayes')

# Create formula from class variable for the Naive Bayes
nb.formula <- as.formula(paste0('as.factor(', train.data.info$class.attr, ') ~ .'))
  
nb.model <- NaiveBayes(formula = nb.formula, data = data.training)

# Save experiment information
saveExperimentInfo()

# Evaluate decision tree with training dataset
succ.rate.0 <- evaluateModel(nb.model, train.data.info)

# Evaluate decision tree with test datasets
succ.rate.1 <- evaluateModel(nb.model, test.data.1.info)
succ.rate.2 <- evaluateModel(nb.model, test.data.2.info)
succ.rate.3 <- evaluateModel(nb.model, test.data.3.info)

# Print the mean success rate as info
succ.rate.mean <- mean(succ.rate.1, succ.rate.2, succ.rate.3)
sprintf('Mean Success Rate: %f', succ.rate.mean)

##############################################################################
# Script to train decision tree classifiers and evaluate it on testing datasets
# Author: Steven Cardini
# Spring 2017
##############################################################################

setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/dt.functions.R")


#################################################
# EXPERIMENT PARAMETERS
#################################################

G.experiment.id <- createExperimentId()

G.dt.params <- list(
  split="gini"
)

G.feat.timediffs <- TRUE
G.feat.scale <- TRUE
G.feat.aggr.fun <- c('mean', 'var', 'min', 'max')

train.data.info <- list(
  file.name = '20170117_test_multipulses_100_50_10_2percent_100ms_interval_5_ver2_manual.csv',
  file.alias = 'dataset.training',
  class.attr = 'mid.in.man',
  label.outliers = TRUE,
  upper.bound = 0.999,
  lower.bound = 0.001
)

test.data.1.info <- list(
  file.name = '20170228_test_multipulses_30min_break.csv',
  file.alias = 'dataset.test.30min',
  class.attr = 'mid.in',
  label.outliers = TRUE,
  upper.bound = 0.999,
  lower.bound = 0.001
)

test.data.2.info <- list(
  file.name = '20170228_test_multipulses_45min_break.csv',
  file.alias = 'dataset.test.45min',
  class.attr = 'mid.in',
  label.outliers = TRUE,
  upper.bound = 0.999,
  lower.bound = 0.001
)

test.data.3.info <- list(
  file.name = '20170228_test_multipulses_60min_break.csv',
  file.alias = 'dataset.test.60min',
  class.attr = 'mid.in',
  label.outliers = TRUE,
  upper.bound = 0.999,
  lower.bound = 0.001
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
data.training <- data.training %>% 
  dplyr::mutate_at(funs(replace(., . == TRUE, 'healthy')), .cols = train.data.info$class.attr) %>% 
  dplyr::mutate_at(funs(replace(., . == FALSE, 'unhealthy')), .cols = train.data.info$class.attr)

# Create formula from class variable for the decision tree
fRpart <- as.formula(paste(train.data.info$class.attr, '.', sep=" ~ "))

# To weigh the classes see: https://stackoverflow.com/questions/8717493/using-minsplit-and-unequal-weights-in-rpart
# Construct decision tree
dec.tree <- rpart(formula = fRpart, data = data.training, parms = G.dt.params)
#summary(dec.tree)

# Evaluate decision tree with training dataset
evaluateDecisionTree(dec.tree, train.data.info)

# Evaluate decision tree with test datasets
evaluateDecisionTree(dec.tree, test.data.1.info)
evaluateDecisionTree(dec.tree, test.data.2.info)
evaluateDecisionTree(dec.tree, test.data.3.info)
  

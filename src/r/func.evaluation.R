#########################################################################################################################
# Script containing functions to evaluate a classifier / model
# Author: Steven Cardini
# Spring 2017
#########################################################################################################################


#########################################################################################################################
#########################################################################################################################
### PUBLIC FUNCTIONS
#########################################################################################################################
#########################################################################################################################


###### evaluateDataset #######################################################
# returns 
##############################################################################
evaluateDataset = function (data.params) {
  paths <- getDatasetFilePaths(data.params)
  data.eval <- getFeatureDataset(data.params, paths)
}


###### evaluateModel #########################################################
# returns 
##############################################################################
evaluateModel = function (class.model, data.params) {
  
  # Import raw dataset and remove class attribute to output it with new classification labels further down
  data.out <- importDataFromFile(data.params$file.name, data.params$class.attr)
  data.out <- data.out %>% dplyr::select(-get(data.params$class.attr))
  
  # Import test dataset
  data.test <- getFeatureDataset(data.params)
  
  # Save actual (correct) classes for later evaluation
  data.temp <- data.test %>% 
    dplyr::select_(G.cellid.arg, data.params$class.attr)
  classes.actual <- as.vector(data.temp[,data.params$class.attr])
  names(classes.actual) <- data.temp[,G.cellid.arg]
  rm(data.temp)
  
  # Remove class attribute from test dataset
  data.test <- data.test %>%
    dplyr::select(-get(data.params$class.attr))
  
  # Initialize paths for output files
  filepaths <- getExperimentFilePaths(data.params)
  
  # Predict classes of training dataset according to the model and create a confusion matrix / table
  classes.predicted <- getPredictedClasses(class.model, data.test)
  conf.matrix <- getConfusionMatrix(classes.predicted, classes.actual)
  
  # Get success rate and save statistics to txt file
  succ.rate <- getStatistics(data.params, conf.matrix, filepaths['stats'])
  
  # Add predicted class labels to output dataset and save it to CSV file
  data.temp <- data.frame(c1 = 1:length(classes.predicted), c2 = classes.predicted)
  colnames(data.temp) <- c(G.cellid.arg, data.params$class.attr)
  data.out <- data.out %>%
    dplyr::full_join(data.temp, by = G.cellid.arg)
  rm(data.temp)
  write.csv(data.out, filepaths['data'])
  
  # Save plot to PNG file
  saveScatterPlot (data.out, G.plot.x.arg, G.plot.y.arg, G.cellid.arg, data.params$class.attr, file.name = filepaths['plot'])
  
  # Return the success rate
  return(succ.rate)
  
}



#########################################################################################################################
#########################################################################################################################
### PRIVATE FUNCTIONS
#########################################################################################################################
#########################################################################################################################


###### getDatasetFilePaths ##########################################################
# IN: 
# OUT: 
##############################################################################
getDatasetFilePaths = function (data.params) {
  id <- hexId(4)
  path.prefix <- paste0(G.result.files.path, 'dataset-statistics', '/', id, '_', data.params$file.alias, '_')
  # initialize file paths
  paths <- list(
    params.info = paste0(path.prefix, 'info.txt'),
    wilcox.tests = paste0(path.prefix, 'wilcoxon_', data.params$lower.bound, '-', data.params$upper.bound, '.csv'),
    feats.boxplots.1 = paste0(path.prefix, 'boxplots_feats.pdf'),
    diffs.boxplots.1 = paste0(path.prefix, 'boxplots_diffs.pdf'),
    feats.boxplots.2 = paste0(path.prefix, 'boxplots_feats_outlying-values_', data.params$lower.bound, '-', data.params$upper.bound, '.pdf'),
    diffs.boxplots.2 = paste0(path.prefix, 'boxplots_diffs_outlying-values_', data.params$lower.bound, '-', data.params$upper.bound, '.pdf'),
    feats.boxplots.3 = paste0(path.prefix, 'boxplots_feats_outlier-cells_', data.params$lower.bound, '-', data.params$upper.bound, '.pdf'),
    diffs.boxplots.3 = paste0(path.prefix, 'boxplots_diffs_outlier-cells_', data.params$lower.bound, '-', data.params$upper.bound, '.pdf'),
    discarded.stepplot.raw = paste0(path.prefix, 'stepplot_discarded.png'),
    discarded.stepplot.marked = paste0(path.prefix, 'stepplot_discarded_marked.png')
  )
  return(paths)
}


###### getExperimentFilePaths ##########################################################
# IN: 
# OUT: 
##############################################################################
getExperimentFilePaths = function (data.params) {
  results.file <- paste0(G.experiment.id, '_', data.params$file.alias)
  # initialize file paths
  paths <- c(stats = paste0(G.experiment.files.path, results.file, '.txt'),
             plot = paste0(G.experiment.files.path, results.file, '.png'),
             data = paste0(G.experiment.files.path, results.file, '.data.csv')
            )
  return(paths)
}


###### calculateAndSaveDatasetStatistics #####################################
# IN: 
# OUT: 
# 
##############################################################################
calculateAndSaveDatasetStatistics = function (data.input, data.input.melted, data.params, bound.val.lower, bound.val.upper, ids.outliers, cell.numbers, stats.save.paths) {
  
  # save basic information about the dataset
  saveDatasetInfo(data.params, cell.numbers, stats.save.paths$params.info)
  
  # subdivide melted data into outlier values
  data.melted.outlyingVals <- data.input.melted[value<bound.val.lower | value>bound.val.upper,]
  
  # prepare plot data for plain features
  ## outlierCells: contains all features of cells that have at least one outlying value
  ## outlyingVals: contains only the outlying values
  data.feats.all <- data.input.melted[!(feature %like% "diffs")]
  data.feats.outlierCells <- data.feats.all[get(G.cellid.arg) %in% ids.outliers]
  data.feats.outlyingVals <- data.melted.outlyingVals[!(feature %like% "diffs")]
  
  # generate and save boxplots of plain features
  boxplotFeatures(data.feats.all, stats.save.paths$feats.boxplots.1)
  boxplotFeaturesMarkOutlyingValues(data.feats.all, data.feats.outlyingVals, data.params, bound.val.lower, bound.val.upper, stats.save.paths$feats.boxplots.2)
  boxplotFeaturesConnectOutlierCells(data.feats.all, data.feats.outlyingVals, data.feats.outlierCells, data.params, bound.val.lower, bound.val.upper, stats.save.paths$feats.boxplots.3)
  
  # if time differences were added, do the same for those
  if (G.feat.timediffs) {
    data.diffs.all <- data.input.melted[feature %like% "diffs"]
    data.diffs.outlierCells <- data.diffs.all[get(G.cellid.arg) %in% ids.outliers]
    data.diffs.outlyingVals <- data.melted.outlyingVals[feature %like% "diffs"]
    
    boxplotFeatures(data.diffs.all, stats.save.paths$diffs.boxplots.1)
    boxplotFeaturesMarkOutlyingValues(data.diffs.all, data.diffs.outlyingVals, data.params, bound.val.lower, bound.val.upper, stats.save.paths$diffs.boxplots.2)
    boxplotFeaturesConnectOutlierCells(data.diffs.all, data.diffs.outlyingVals, data.diffs.outlierCells, data.params, bound.val.lower, bound.val.upper, stats.save.paths$diffs.boxplots.3)
  }
  
  
  stepplotQuantileDiscards(data.input, data.input.melted, data.params, bound.val.lower, bound.val.upper, cell.numbers$outliers, stats.save.paths$discarded.stepplot.raw, stats.save.paths$discarded.stepplot.marked)
  
}


###### boxplotFeatures #####################################
# IN: 
# OUT: 
# boxplot of all features
##############################################################################
boxplotFeatures = function (data.melted.all, save.path) {
  
  ggplot() + 
    ggtitle("Boxplot of features over all cells") +
    theme_bw() +
    geom_boxplot(data = data.melted.all, aes(feature, value)) + 
    coord_flip()
  ggsave(save.path, width = 16, height = 40)
  
}

###### boxplotFeaturesMarkOutlyingValues #####################################
# IN: 
# OUT: 
# boxplot of all features
##############################################################################
boxplotFeaturesMarkOutlyingValues = function (data.melted.all, data.melted.outlyingVals, data.params, bound.val.lower, bound.val.upper, save.path) {
  
  ggplot() + 
    ggtitle(paste0("Outlying values among features for quantile ", data.params$lower.bound, " - ", data.params$upper.bound)) +
    theme_bw() +
    geom_boxplot(data = data.melted.all, aes(feature, value)) + 
    geom_point(data = data.melted.outlyingVals, aes(feature, value, size = 2), color = "red", shape = 3) +
    geom_hline(yintercept = bound.val.lower, color = 'red', linetype = 'dotted') +
    geom_hline(yintercept = bound.val.upper, color = 'red', linetype = 'dotted') +
    coord_flip()
  ggsave(save.path, width = 16, height = 40)
  
}

###### boxplotFeaturesConnectOutlierCells #####################################
# IN: 
# OUT: 
# boxplot of all features
##############################################################################
boxplotFeaturesConnectOutlierCells = function (data.melted.all, data.melted.outlyingVals, data.melted.outliers, data.params, bound.val.lower, bound.val.upper, save.path) {
  
  ggplot() + 
    ggtitle(paste0("Outlier cells for quantile ", data.params$lower.bound, " - ", data.params$upper.bound)) +
    theme_bw() +
    geom_boxplot(data = data.melted.all, aes(feature, value)) + 
    geom_path(data = data.melted.outliers, aes(feature, value, group = get(G.cellid.arg), alpha = 0.3)) +
    geom_point(data = data.melted.outlyingVals, aes(feature, value, size = 2), color = "red", shape = 3) +
    geom_hline(yintercept = bound.val.lower, color = 'red', linetype = 'dotted') +
    geom_hline(yintercept = bound.val.upper, color = 'red', linetype = 'dotted') +
    coord_flip()  
  ggsave(save.path, width = 16, height = 40)
  
}

###### stepplotQuantileDiscards #####################################
# IN: 
# OUT: 
# STEP-PLOT DISCARDED CELLS IN FUNCTION OF QUANTILE THRESHOLD
##############################################################################
stepplotQuantileDiscards = function (data, data.melted, data.params, bound.val.lower, bound.val.upper, cell.number.outliers, save.path.unmarked, save.path.marked) {
  
  # Calculate number of discarded cells for different quantile thresholds
  quantile.data <- data.frame(x = numeric(), y = numeric())
  x <- 0.0001
  y <- 0
  i <- 1
  while(y < 0.2*nrow(data)) {
    bound.lower <- quantile(data.melted[,value], x)
    bound.upper <- quantile(data.melted[,value], 1-x)
    y <- data.melted[value<bound.val.lower | value>bound.val.upper, get(G.cellid.arg)] %>%
      unique() %>%
      length()
    quantile.data[i,] <- c(x,y)
    x <- x + 0.0001
    i <- i+1
  }
  # Save plot
  ggplot(quantile.data, aes(x,y)) + 
    theme_bw() + 
    labs(title = "Number of cells discarded in function of quantile threshold", x = "lower quantile threshold", y = "Number of cells discarded") +
    labs(subtitle = data.params$file.alias) +
    geom_step() +
    ggsave(save.path.unmarked, width = 6, height = 4) +
    labs(subtitle = paste0(data.params$file.alias, ' / chosen threshold = (', data.params$lower.bound, ', ', cell.number.outliers ,')')) +
    geom_hline(yintercept = cell.number.outliers, color = 'red', linetype = 'dashed') + 
    geom_vline(xintercept = data.params$lower.bound, color = 'red', linetype = 'dashed') +
    ggsave(save.path.marked, width = 6, height = 4)
  
}


###### getPredictedClasses ###################################################
# IN: 
# OUT: 
##############################################################################
getPredictedClasses = function(class.model, data.test) {
  # for each instance in test dataset calculate probability of class assignments
  class.prob <- predict(class.model, data.test)
  # determine classes predicted by the model, depending on the model type
  if(G.experiment.model == "DecisionTree") {
    class.pred <- class.prob > 0.5 
  } else if (G.experiment.model == "NaiveBayes") {
    class.pred <- class.prob$class
  } else if (G.experiment.model == "SVM") {
    return (class.prob)
  } else {
    # Model not known, stop code execution
    stop()
  }
  return (class.pred)
}


###### getConfusionMatrix ####################################################
# Creates confusion matrix and ensures its dimension is 4x4
# IN: 
# OUT: 
##############################################################################
getConfusionMatrix = function(class.pred, class.act) {
  conf.matrix <- table(class.pred, class.act)
  stopifnot(dim(conf.matrix)[1] == 2)
  stopifnot(dim(conf.matrix)[2] == 2)
  return(conf.matrix)
}


###### getStatistics #########################################################
# IN: 
# OUT: 
##############################################################################
getStatistics = function (in.params, in.conf.matrix, in.file.path) {
  # Calculate statistics
  tp <- in.conf.matrix['TRUE', 'TRUE'] # true positives
  fp <- in.conf.matrix['TRUE', 'FALSE'] # false positives
  tn <- in.conf.matrix['FALSE', 'FALSE'] # true negatives
  fn <- in.conf.matrix['FALSE', 'TRUE'] # false negatives
  tpr <- tp / (tp + fn) # true positive rate = TP / number of positives
  fpr <- fp / (fp + tn) # false positive rate = FP / number of negatives
  succ <- (tp + tn) / (tp + tn + fp + fn) # success rate, number of correct classiﬁcations divided by the total number of classiﬁcations
  
  # Save information and error statistics of classification to text file
  fileconn <- file(in.file.path)
  writeLines(c(paste0('Timestamp: ', Sys.time()),
               '--------------------------------',
               paste0('Test Dataset: ', in.params$file.name),
               paste0('Class Attribute: ', in.params$class.attr),
               paste0('Label Outliers: ', in.params$label.outliers.method),
               paste0('Upper quantile bound: ', in.params$upper.bound),
               paste0('Lower quantile bound: ', in.params$lower.bound),
               '--------------------------------',
               paste0("TP: ", tp), 
               paste0("TN: ", tn), 
               paste0("FP: ", fp), 
               paste0("FN: ", fn), 
               paste0("TPR: ", tpr), 
               paste0("FPR: ", fpr), 
               paste0("Success Rate: ", succ)), 
             fileconn)
  close(fileconn)
  
  # Return success rate 
  return(succ)
}


###### saveScatterPlot #######################################################
# IN: 
# OUT: 
##############################################################################
saveScatterPlot = function (in.data, in.plot.x.arg, in.plot.y.arg, in.plot.group, in.plot.color = 1, file.name) {
  ggplot(in.data, aes_string(x = in.plot.x.arg, y = in.plot.y.arg, group = in.plot.group)) + 
    geom_point(alpha = 0.02) + 
    geom_path(aes_string(colour = in.plot.color), alpha = 0.5) + 
    theme_bw()
  ggsave(file.name)
}
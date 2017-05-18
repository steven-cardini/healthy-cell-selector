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
    feats.boxplots = paste0(path.prefix, 'boxplots_feats_', data.params$lower.bound, '-', data.params$upper.bound, '.pdf'),
    diffs.boxplots = paste0(path.prefix, 'boxplots_diffs_', data.params$lower.bound, '-', data.params$upper.bound, '.pdf')
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
             data = paste0(G.experiment.files.path, results.file, '.csv')
            )
  return(paths)
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
               paste0('Label Outliers: ', in.params$label.outliers),
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
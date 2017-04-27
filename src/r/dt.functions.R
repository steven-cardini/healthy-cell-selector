##############################################################################
# Script containing functions for decision tree evaluation
# Author: Steven Cardini
# Spring 2017
##############################################################################

library(dplyr)
library(data.table) 
library(ggplot2)
library(plotly)

library(rpart)
library(rpart.plot)


#################################################
# PUBLIC FUNCTIONS
#################################################

# function to generate a random experiment ID
createExperimentId = function () {
  exp.id <- paste0(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = '')
  return(exp.id)
}


# function to import cell_Id, class and features from a CSV, as specified by data.params
# returns a data.frame
getFeatureDataset = function (data.params) {
  
  # Import raw features from file and add cell_Id
  data <- importDataFromFile(data.params$file.name, data.params$class.attr)
  
  # Discard unrelevant attributes
  data <- data %>% 
    dplyr::select(one_of(G.cellid.arg, G.timepoint.arg, data.params$class.attr), matches(G.feature.include.regex), -matches(G.feature.exclude.regex))
  
  # Order dataset by (cell_Id, timepoints) and discard timepoints attribute
  data <- data %>%
    dplyr::arrange_(.dots = c(G.cellid.arg, G.timepoint.arg)) %>%
    dplyr::select(-dplyr::one_of(G.timepoint.arg))
  
  # If requested, add differences between time points as new features
  if (G.feat.timediffs)
    data <- addTimeValueDifferences(data, data.params$class.attr)
  
  # Aggregate features for each cell
  data <- aggregateTimeCourses(data, data.params$class.attr)
  
  # If requested, label cell class FALSE if any of its aggregated features is an outlier (individually per feature)
  if (data.params$label.outliers == 'individual')
    data <- labelOutliersPerFeature(data, data.params$class.attr, data.params$lower.bound, data.params$upper.bound)
  
  # If requested, scale the features
  if (G.feat.scale)
    data <- scaleFeatures(data, data.params$class.attr)
  
  # If requested, label cell class FALSE if any of its aggregated features is an outlier (globally over all features)
  if (data.params$label.outliers == 'global')
    data <- labelOutliersOverAllFeatures(data, data.params$class.attr, data.params$lower.bound, data.params$upper.bound)
  
  data <- as.data.frame(data)
  
  return(data)
  
}


evaluateDecisionTree = function (dec.tree, data.params) {
  
  # Import raw dataset and remove class attribute to output it with new classification labels further down
  data.out <- importDataFromFile(data.params$file.name, data.params$class.attr)
  data.out <- data.out %>% dplyr::select(-get(data.params$class.attr))
  
  # Import test dataset
  data.test <- getFeatureDataset(data.params)
  
  # Save actual (correct) classes for later evaluation
  data.temp <- data.test %>% 
    dplyr::select_(G.cellid.arg, data.params$class.attr)
  class.act <- as.vector(data.temp[,data.params$class.attr])
  names(class.act) <- data.temp[,G.cellid.arg]
  rm(data.temp)
  
  # Remove class attribute from test dataset
  data.test <- data.test %>%
    dplyr::select(-get(data.params$class.attr))
  
  # Initialize paths for output files
  res.file.path <- paste0(G.result.files.path, G.experiment.id, '/')
  res.file.name <- paste0(G.experiment.id, '_', data.params$file.alias)
  pdf.file.path <- paste0(res.file.path, G.experiment.id, '_dt.pdf')
  txt.info.file.path <- paste0(res.file.path, G.experiment.id, '_info.txt')
  txt.stats.file.path <- paste0(res.file.path, res.file.name, '.txt')
  png.file.path <- paste0(res.file.path, res.file.name, '.png')
  csv.file.path <- paste0(res.file.path, res.file.name, '.csv')
  
  # Save decision tree, if not present yet
  if(!dir.exists(res.file.path)) {
    # create the experiment folder in results/
    dir.create(res.file.path)
    # save experiment information
    saveExperimentInfo(txt.info.file.path)
    # save the decision tree
    pdf(file = pdf.file.path)
    prp(dec.tree, under = TRUE, varlen = 0)
    dev.off()
  }

  # Predict classes of training dataset with decision tree and create a confusion matrix / table
  class.prob <- predict(dec.tree, data.test) # probabilities of class assignment
  class.pred <- class.prob[,'healthy']>0.5 # classes predicted by decision tree
  conf.matrix <- table(class.pred, class.act)
  
  # Test that confusion matrix is 4x4
  stopifnot(dim(conf.matrix)[1] == 2)
  stopifnot(dim(conf.matrix)[2] == 2)
  
  # Get success rate and save statistics to txt file
  succ.rate <- getStatistics(data.params, conf.matrix, txt.stats.file.path)
  
  # Add predicted class labels to output dataset and save it to CSV file
  data.temp <- data.frame(c1 = as.integer(names(class.pred)), c2 = class.pred)
  colnames(data.temp) <- c(G.cellid.arg, data.params$class.attr)
  data.out <- data.out %>%
    dplyr::full_join(data.temp, by = G.cellid.arg)
  rm(data.temp)
  write.csv(data.out, csv.file.path)
  
  # Save plot to PNG file
  saveScatterPlot (data.out, G.plot.x.arg, G.plot.y.arg, G.cellid.arg, data.params$class.attr, file.name = png.file.path)
  
  # Return the success rate
  return(succ.rate)
  
}



#################################################
# PRIVATE FUNCTIONS
#################################################


addCellIds = function (in.data) {
  
  # add cell IDs to data set
  data <- in.data %>%
    dplyr::select(dplyr::one_of(G.id.args.vec)) %>%
    dplyr::distinct()
  data[,G.cellid.arg] <- 1:nrow(data)
  
  out.data <- data %>%
    dplyr::inner_join(in.data, by = G.id.args.vec) # now equals input data set plus new row in.cell.id.arg
  
  return(out.data)
}


# calculate the coefficient of variation (CV) for a statistic, ignoring NA values
cv = function (x) {
  x <- x[!is.na(x)] # remove NA values
  
  if (mean(x)==0 | sd(x)/mean(x) > .Machine$integer.max)
    return (.Machine$integer.max)
  
  return(sd(x) / mean(x))
}


myScaleFunction = function (in.vector) {
  loc.vector <- in.vector
  loc.vector <- loc.vector - mean(loc.vector, na.rm = TRUE) # center the values at 0
  loc.vector <- loc.vector / sd(loc.vector, na.rm = TRUE) # scale the values by their standard deviation
  
  return (loc.vector)
}


importDataFromFile = function (in.file.name, in.class.arg) {
  
  loc.file.path <- paste0(G.data.files.path, in.file.name)
  
  # Import data from file and add cell IDs
  loc.data <- read.csv(loc.file.path)
  loc.data <- addCellIds(loc.data)
  
  # Test that all cells contain the same number of time points and save the number of timepoints
  test <- table(loc.data[,G.cellid.arg])
  #stopifnot(var(test) == 0)
  #stopifnot(mean(test) %% 1 == 0)
  rm(test)
  
  # Test that there is the identical classification (TRUE | FALSE) for all time points of a cell
  test <- loc.data %>% 
    dplyr::select(one_of(c(G.cellid.arg, in.class.arg))) %>% 
    unique()
  stopifnot(nrow(test) == max(loc.data[,G.cellid.arg]))
  rm(test)

  return(loc.data)
  
}


# INPUT: data set consisting of grouping attributes (ID, class) + time points + several features across the columns
addTimeValueDifferences = function (in.data, in.class.arg) {
  data <- in.data %>%
    dplyr::group_by_(G.cellid.arg, in.class.arg) %>%
    dplyr::mutate_each(funs(diffs = c(NA, diff(.)))) %>%
    na.omit() %>%
    dplyr::ungroup()
  
  return(data.frame(data))
}


aggregateTimeCourses = function (in.data, in.class.arg) {
  data <- in.data %>%
    dplyr::group_by_(G.cellid.arg, in.class.arg) %>%
    dplyr::summarise_all(.funs = G.feat.aggr.fun) %>%
    dplyr::ungroup()
  
  return (data)
}


# function: for each feature, label outliers (above in.quantile.upper or below in.quantile.lower) as FALSE
labelOutliersPerFeature = function (in.data, in.class.arg, in.quantile.lower, in.quantile.upper) {
  data <- in.data
  features <- setdiff(names(data), c(G.cellid.arg, in.class.arg))
  setDT(data)[,  (in.class.arg) := Reduce(`&`, lapply(.SD, function(x) x < quantile(x, in.quantile.upper) & 
                                                            x > quantile(x, in.quantile.lower))), .SDcols = features]
  
  return(data)
}


# function: label outliers globally across all features (above in.quantile.upper or below in.quantile.lower) as FALSE
# input data should be scaled!
labelOutliersOverAllFeatures = function (data, class.arg, quantile.lower, quantile.upper) {
  features <- setdiff(names(data), c(G.cellid.arg, class.arg))
  
  
  # create a melted data.table
  data.dt <- data %>% 
    select(-get(class.arg)) %>%
    as.data.table() %>%
    melt(id.vars = G.cellid.arg, variable.name = 'feat', value.name = 'val')
  
  # determine cut boundaries
  bound.lower <- quantile(data.dt[,val], quantile.lower)
  bound.upper <- quantile(data.dt[,val], quantile.upper)
  
  assign("bound.lower", bound.lower, envir = .GlobalEnv)
  assign("bound.upper", bound.upper, envir = .GlobalEnv)
  
 
  # get cell_Ids where any feature is below bound.lower or bound.upper
  ids.relabel <- data.dt[val<bound.lower | val>bound.upper, get(G.cellid.arg)]
  
  assign("ids.relabel", ids.relabel, envir = .GlobalEnv)
  
  # relabel class of those ids
  data <- as.data.table(data)
  data[get(G.cellid.arg) %in% ids.relabel, (class.arg) := FALSE]
  
  return(as.data.frame(data))
}


scaleFeatures = function (in.data, in.class.arg) {
  data <- in.data
  scale.cols <- setdiff(names(data), c(G.cellid.arg, in.class.arg))
  data <- data %>% 
    dplyr::mutate_at(scale.cols, myScaleFunction)
  
  return(data)
}


saveExperimentInfo = function (in.file.path) {
  fileconn <- file(in.file.path)
  writeLines(c(paste0('Timestamp: ', Sys.time()),
               '--------------------------------',
               paste0('Decision tree params: ', paste0(paste(names(G.dt.params), G.dt.params, sep = ' = '), collapse = ' / ')), 
               paste0('Features included: ', G.feature.include.regex),
               paste0('Features excluded: ', G.feature.exclude.regex),
               paste0('Time differences as features: ', G.feat.timediffs),
               paste0('Aggregate functions: ', paste0(G.feat.aggr.fun, collapse = ' / ')),
               paste0('Features were scaled: ', G.feat.scale)),
             fileconn)
  close(fileconn)
}


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


saveScatterPlot = function (in.data, in.plot.x.arg, in.plot.y.arg, in.plot.group, in.plot.color = 1, file.name) {
  ggplot(in.data, aes_string(x = in.plot.x.arg, y = in.plot.y.arg, group = in.plot.group)) + 
    geom_point(alpha = 0.02) + 
    geom_path(aes_string(colour = in.plot.color), alpha = 0.5) + 
    theme_bw()
  
  if(is.character(file.name))
    ggsave(file.name)
}


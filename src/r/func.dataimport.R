#########################################################################################################################
# Script containing functions to import feature datasets
# Author: Steven Cardini
# Spring 2017
#########################################################################################################################


#########################################################################################################################
#########################################################################################################################
### PUBLIC FUNCTIONS
#########################################################################################################################
#########################################################################################################################


###### getFeatureDataset #####################################################
# returns a data.frame containing cell_Id, class and aggregated features 
# per cell from a CSV, as specified by data.params
##############################################################################
getFeatureDataset = function (data.params, stats.save.paths = NA) {
  
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
  if (data.params$label.outliers.method == 'individual')
    data <- labelOutliersPerFeature(data, data.params$class.attr, data.params$lower.bound, data.params$upper.bound)
  
  # If requested, scale the features
  if (G.feat.scale)
    data <- scaleFeatures(data, data.params$class.attr)
  
  # If requested, label cell class FALSE if any of its aggregated features is an outlier (globally over all features)
  if (data.params$label.outliers.method == 'global')
    data <- labelOutliersOverAllFeatures(data, data.params, stats.save.paths)
  
  data <- as.data.frame(data)
  
  return(data)
  
}


###### importDataFromFile ####################################################
# Returns a data.frame containing all data from a CSV and an additional
#   attribute cell_Id, which uniquely identifies separate cells
# This function also performs some tests on the integrity of the CSV data
##############################################################################
importDataFromFile = function (in.file.name, in.class.arg) {
  
  loc.file.path <- paste0(G.data.files.path, in.file.name)
  
  # Import data from file and add cell IDs
  loc.data <- read.csv(loc.file.path)
  loc.data <- addCellIds(loc.data)
  
  # Test that all cells contain the same number of time points and save the number of timepoints
  #test <- table(loc.data[,G.cellid.arg])
  #stopifnot(var(test) == 0)
  #stopifnot(mean(test) %% 1 == 0)
  #rm(test)
  
  # Test that there is the identical classification (TRUE | FALSE) for all time points of a cell
  test1 <- loc.data %>% 
    dplyr::select(one_of(c(G.cellid.arg, in.class.arg))) %>% 
    unique()
  test2 <- loc.data %>%
    dplyr::select_(G.cellid.arg) %>%
    unique()
  stopifnot(nrow(test1) == nrow(test2))
  rm(test1, test2)
  
  return(loc.data)
  
}



#########################################################################################################################
#########################################################################################################################
### PRIVATE FUNCTIONS
#########################################################################################################################
#########################################################################################################################


###### addCellIds ############################################################
# IN: data.frame containing all features from a CSV
# OUT: data.frame from input, with additional attribute cell_Id, without site and obj nr attributes
##############################################################################
addCellIds = function (in.data) {
  # add new attribute cell_Id composed of site nr. and object nr.
  data <- as.data.table(in.data)
  data[, (G.cellid.arg) := sprintf("%03d_%03d", get(G.site.arg), get(G.objnr.arg))]
  # remove site nr. and object nr. from dataset
  data <- data %>%
    as.data.frame() %>%
    dplyr::select(-one_of(G.site.arg, G.objnr.arg))
  
  return(data)
}


###### addTimeValueDifferences ###############################################
# IN: data.frame consisting of grouping attributes (ID, class) + time points 
#     + several features across the columns
# OUT: 
# Returns 
##############################################################################
# INPUT: 
addTimeValueDifferences = function (in.data, in.class.arg) {
  data <- in.data %>%
    dplyr::group_by_(G.cellid.arg, in.class.arg) %>%
    dplyr::mutate_each(funs(diffs = c(NA, diff(.)))) %>%
    na.omit() %>%
    dplyr::ungroup()
  return(data.frame(data))
}


###### aggregateTimeCourses ##################################################
# IN: 
# OUT: 
# Returns 
##############################################################################
aggregateTimeCourses = function (in.data, in.class.arg) {
  data <- in.data %>%
    dplyr::group_by_(G.cellid.arg, in.class.arg) %>%
    dplyr::summarise_all(.funs = G.feat.aggr.fun) %>%
    dplyr::ungroup()
  return (data)
}


###### labelOutliersPerFeature ###############################################
# IN: 
# OUT: 
# for each feature, label outliers (above in.quantile.upper or below in.quantile.lower) as FALSE 
##############################################################################
labelOutliersPerFeature = function (in.data, in.class.arg, in.quantile.lower, in.quantile.upper) {
  data <- in.data
  features <- setdiff(names(data), c(G.cellid.arg, in.class.arg))
  setDT(data)[,  (in.class.arg) := Reduce(`&`, lapply(.SD, function(x) x < quantile(x, in.quantile.upper) & 
                                                        x > quantile(x, in.quantile.lower))), .SDcols = features]
  return(data)
}


###### labelOutliersOverAllFeatures ##########################################
# IN: should be scaled!
# OUT: 
# label outliers globally across all features (above in.quantile.upper or below in.quantile.lower) as FALSE
##############################################################################
labelOutliersOverAllFeatures = function (data, data.params, stats.save.paths = NA) {
  
  features <- setdiff(names(data), c(G.cellid.arg, data.params$class.attr))
  data.all <- as.data.table(data)
  
  # get cell_Ids of the cells that were manually kicked out due to bad segmentation
  ids.false <- data.all[get(data.params$class.attr) == FALSE, get(G.cellid.arg)]

  # if required, remove manually kicked out cells from dataset, such that outliers are determined from TRUE cells only
  if (data.params$label.outliers.onlyTrueCells) {
    data.input <- data.all[get(data.params$class.attr) == TRUE,]
  } else {
    data.input <- data.all
  }
  
  # set melted dataset that is the base for determining outliers
  data.input.melted <- data.input %>%
    dplyr::select(-get(data.params$class.attr)) %>%
    as.data.table() %>%
    melt(id.vars = G.cellid.arg, variable.name = 'feature', value.name = 'value')
  
  # determine cut boundary values
  bound.val.lower <- quantile(data.input.melted[,value], data.params$lower.bound)
  bound.val.upper <- quantile(data.input.melted[,value], data.params$upper.bound)
  
  # get cell_Ids where any feature is below bound.lower or bound.upper
  ids.outliers <- data.input.melted[value<bound.val.lower | value>bound.val.upper, get(G.cellid.arg)] %>%
    unique() %>%
    sort()
  
  # relabel class of those ids and store it in output data.table
  data.output <- data.all
  data.output[get(G.cellid.arg) %in% ids.outliers, (data.params$class.attr) := FALSE]
  
  
  # if required, calculate and save dataset statistics
  if (is.list(stats.save.paths)) {
    cell.numbers <- list(
      total = nrow(data.all),
      manual = length(ids.false),
      outliers = length(ids.outliers)
    )
    calculateAndSaveDatasetStatistics(data.input.melted, data.params, bound.val.lower, bound.val.upper, ids.outliers, cell.numbers, stats.save.paths)
  }
    
  # return the dataset where outliers are labelled FALSE
  return(as.data.frame(data.output))
  
}


###### scaleFeatures ########################################################
# IN: 
# OUT: 
# Returns
##############################################################################
scaleFeatures = function (in.data, in.class.arg) {
  data <- in.data
  scale.cols <- setdiff(names(data), c(G.cellid.arg, in.class.arg))
  data <- data %>% 
    dplyr::mutate_at(scale.cols, scaleVector)
  
  return(data)
}


###### scaleVector ###########################################################
# IN: vector of numeric data
# OUT: vector of numeric data
# Returns a new vector containing scaled data from an input vector, such
# that the mean of the date equals 0 and the standard deviation equals 1
##############################################################################
scaleVector = function (in.vector) {
  loc.vector <- in.vector - mean(in.vector, na.rm = TRUE) # center the values at 0
  loc.vector <- loc.vector / sd(loc.vector, na.rm = TRUE) # scale the values by their standard deviation
  return (loc.vector)
}

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
  if (data.params$label.outliers == 'individual')
    data <- labelOutliersPerFeature(data, data.params$class.attr, data.params$lower.bound, data.params$upper.bound)
  
  # If requested, scale the features
  if (G.feat.scale)
    data <- scaleFeatures(data, data.params$class.attr)
  
  # If requested, label cell class FALSE if any of its aggregated features is an outlier (globally over all features)
  if (data.params$label.outliers == 'global')
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



#########################################################################################################################
#########################################################################################################################
### PRIVATE FUNCTIONS
#########################################################################################################################
#########################################################################################################################


###### addCellIds ############################################################
# IN: data.frame containing features from a CSV
# OUT: data.frame identical to input, with additional attribute cell_Id
##############################################################################
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


###### labelOutliersPerFeature ###############################################
# IN: should be scaled!
# OUT: 
# label outliers globally across all features (above in.quantile.upper or below in.quantile.lower) as FALSE
##############################################################################
labelOutliersOverAllFeatures = function (data, data.params, stats.save.paths = NA) {
  features <- setdiff(names(data), c(G.cellid.arg, data.params$class.attr))
  
  # create a melted data.table
  data.melted <- data %>% 
    dplyr::select(-get(data.params$class.attr)) %>%
    as.data.table() %>%
    melt(id.vars = G.cellid.arg, variable.name = 'feature', value.name = 'value')
  
  # determine cut boundaries
  bound.lower <- quantile(data.melted[,value], data.params$lower.bound)
  bound.upper <- quantile(data.melted[,value], data.params$upper.bound)
  
  # get cell_Ids where any feature is below bound.lower or bound.upper
  ids.outliers <- data.melted[value<bound.lower | value>bound.upper, get(G.cellid.arg)] %>%
    unique() %>%
    sort()
  
  # relabel class of those ids
  data <- as.data.table(data)
  data[get(G.cellid.arg) %in% ids.outliers, (data.params$class.attr) := FALSE]

  # IF dataset information should not be saved, return the dataset
  if (!is.list(stats.save.paths))
    return(as.data.frame(data))
  
  # ELSE get dataset statistics and save them to files
  saveDatasetInfo(data.params, stats.save.paths$params.info)
  
  # subdivide melted data into outlier values and non-outlier values
  outliers.values <- data.melted[value<bound.lower | value>bound.upper,]
  nonoutliers.values <- data.melted[value>=bound.lower & value<=bound.upper,]
  
  #########################
  # WILCOXON TESTS OF OUTLIERS VS. NON-OUTLIERS
  #########################
  # calculate Wilcoxon tests
  n <- length(features)
  wilcox.values <- data.frame(statistic = numeric(length = n), pvalue = numeric(length = n), 
                              outliers = numeric(length = n), nonoutliers = numeric(length = n))
  row.names(wilcox.values) <- features
  for(colname in features) {
    x <- outliers.values[feature == colname]$value
    y <- nonoutliers.values[feature == colname]$value
    wilcox.values[colname, "outliers"] <- length(x)
    wilcox.values[colname, "nonoutliers"] <- length(y)
    
    if (length(x)==0 | length(y)==0) { # not enough (or only) outliers
      wilcox.values[colname, "statistic"] <- NA
      wilcox.values[colname, "pvalue"] <- NA
    } else {
      w <- wilcox.test(x, y, paired = FALSE)
      wilcox.values[colname, "statistic"] <- w$statistic
      wilcox.values[colname, "pvalue"] <- w$p.value
    }
  }
  
  # sort by p-value and save results of Wilcoxon tests
  wilcox.values <- cbind(feature=row.names(wilcox.values), wilcox.values) # preserve features from row names
  wilcox.values <- wilcox.values %>%
    arrange(pvalue)
  write.csv(wilcox.values, stats.save.paths$wilcox.tests)
  
  #########################
  # BOXPLOTS OF THE FEATURES
  #########################
  # make box plots of the features, add paths of the outliers and save it
  data.feats <- data.melted[ !(feature %like% "diffs") ]
  data.feats.outliers <- data.feats[get(G.cellid.arg) %in% ids.outliers]
  ggplot() + 
    ggtitle(paste0("Outliers of quantile ", data.params$lower.bound, " - ", data.params$upper.bound)) +
    geom_boxplot(data = data.feats, aes(feature, value)) + 
    geom_path(data = data.feats.outliers, aes(feature, value, group = get(G.cellid.arg), alpha = 0.3)) + 
    coord_flip()  
  ggsave(stats.save.paths$feats.boxplots, width = 16, height = 40)

  # save also the diff features, if enabled
  if (G.feat.timediffs) {
    data.diffs <- data.melted[ feature %like% "diffs" ]
    data.diffs.outliers <- data.diffs[get(G.cellid.arg) %in% ids.outliers]
    ggplot() +
      ggtitle(paste0("Outliers of quantile ", data.params$lower.bound, " - ", data.params$upper.bound)) +
      geom_boxplot(data = data.diffs, aes(feature, value)) + 
      geom_path(data = data.diffs.outliers, aes(feature, value, group = get(G.cellid.arg), alpha = 0.3)) + 
      coord_flip()  
    ggsave(stats.save.paths$diffs.boxplots, width = 16, height = 40)
  }
    
  return(as.data.frame(data))
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

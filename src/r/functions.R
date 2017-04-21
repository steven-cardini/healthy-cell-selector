library(dplyr)
library(data.table) 


#################################################
# CONSTANTS
#################################################

dat.cellid.arg <- 'cell_Id' # custom name for the new cell id attribute
dat.id.args.vec <- c('Image_Metadata_Site', 'objNuc_TrackObjects_Label')
dat.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
dat.timepoint.arg <- 'RealTime'

cell.feature.include.regex <- '^objCell_Intensity_MeanIntensity_imErkCorrOrig$|^objNuc_Intensity_MeanIntensity_imNucCorrBg$|^objCell_AreaShape*'
cell.feature.exclude.regex <- '.*(EulerNumber)+.*'



#################################################
# PUBLIC FUNCTIONS
#################################################

importRawDataset = function (in.file.path, in.class.arg) {
  
  loc.data <- importDataFromFile(in.file.path, in.class.arg, features.only = FALSE)
  
  return(loc.data)
  
}


importTrainingDataset = function (in.file.path, in.class.arg) {
  
  # Import test dataset from file
  loc.data <- importDataFromFile(in.file.path, in.class.arg, features.only = TRUE)
  
  # Order rows by cell_Id, timepoint and remove timepoints from dataset
  loc.data <- loc.data %>% 
    dplyr::arrange_(.dots = c(dat.cellid.arg, dat.timepoint.arg)) %>% 
    dplyr::select(-dplyr::one_of(dat.timepoint.arg))
  
  # Add differences between time points as new features and aggregate features for each cell
  loc.data <- addDifferencesAndAggregate(loc.data, in.class.arg)
  
  # Label cells FALSE if any of the features is an outlier (< 0.001 quantile OR > 0.999 quantile)
  loc.data <- labelOutliersAsFalse(loc.data, in.class.arg, 0.001, 0.999)
  
  # Scale the features
  loc.data <- scaleFeatures(loc.data, in.class.arg)

  return(loc.data)
  
}


importTestDataset = function (in.file.path, in.class.arg) {
  
  # Import test dataset from file
  loc.data <- importDataFromFile(in.file.path, in.class.arg, features.only = TRUE)
  
  # Order rows by cell_Id, timepoint and remove timepoints from dataset
  loc.data <- loc.data %>% 
    dplyr::arrange_(.dots = c(dat.cellid.arg, dat.timepoint.arg)) %>% 
    dplyr::select(-dplyr::one_of(dat.timepoint.arg))
  
  # Add differences between time points as new features and aggregate features for each cell
  loc.data <- addDifferencesAndAggregate(loc.data, in.class.arg)
  
  # Scale the features
  loc.data <- scaleFeatures(loc.data, in.class.arg)
  
  # Remove the class attribute
  loc.data <- loc.data %>% dplyr::select(-get(in.class.arg))
  
  return(loc.data)
  
}



#################################################
# PRIVATE FUNCTIONS
#################################################

importDataFromFile = function (in.file.path, in.class.arg, features.only) {
  
  # Import data from file and add cell IDs
  loc.data <- read.csv(in.file.path)
  loc.data <- addCellIds(loc.data)
  
  # Test that all cells contain the same number of time points and save the number of timepoints
  test <- table(loc.data[,dat.cellid.arg])
  stopifnot(var(test) == 0)
  stopifnot(mean(test) %% 1 == 0)
  rm(test)
  
  # Test that there is the identical classification (TRUE | FALSE) for all time points of a cell
  test <- loc.data %>% 
    dplyr::select(one_of(c(dat.cellid.arg, in.class.arg))) %>% 
    unique()
  stopifnot(nrow(test) == max(loc.data[,dat.cellid.arg]))
  rm(test)
  
  # Return the raw data, if features.only = false
  if (features.only == FALSE)
    return(loc.data)
  
  # Else, return specified features only
  loc.data <- loc.data %>% 
    dplyr::select(one_of(dat.cellid.arg, dat.timepoint.arg, in.class.arg), matches(cell.feature.include.regex), -matches(cell.feature.exclude.regex))
  
  return(loc.data)
  
}

addCellIds = function (in.data) {
  
  # add cell IDs to data set
  loc.data <- in.data %>%
    dplyr::select(dplyr::one_of(dat.id.args.vec)) %>%
    dplyr::distinct()
  loc.data[,dat.cellid.arg] <- 1:nrow(loc.data)
  
  out.data <- dplyr::inner_join(loc.data, in.data, by = dat.id.args.vec) # now equals input data set plus new row in.cell.id.arg
  
  return(out.data)
}

# calculate the coefficient of variation (CV) for a statistic, ignoring NA values
cv = function (x) {
  x <- x[!is.na(x)] # remove NA values
  
  if (mean(x)==0 | sd(x)/mean(x) > .Machine$integer.max)
    return (.Machine$integer.max)

  return(sd(x) / mean(x))
}

# INPUT: data set consisting of grouping attributes (ID, class) + several features across the columns
addDifferencesAndAggregate = function (in.data, in.class.arg) {
  loc.data <- in.data %>%
    group_by_(dat.cellid.arg, in.class.arg) %>%
    mutate_each(funs(diffs = c(NA, diff(.)))) %>%
    na.omit() %>%
    summarise_each(funs(col.cv = cv(.), col.mn = mean(.))) %>%
    ungroup()
  
  loc.data <- data.frame(loc.data)
  
  return(loc.data)
}

labelOutliersAsFalse = function (in.data, in.class.arg, in.quantile.lower, in.quantile.upper) {
  
  loc.data <- in.data
  features <- setdiff(names(loc.data), c(dat.cellid.arg, in.class.arg))
  setDT(loc.data)[,  (in.class.arg) := Reduce(`&`, lapply(.SD, function(x) x < quantile(x, in.quantile.upper) & 
                                             x > quantile(x, in.quantile.lower))), .SDcols = features]
  
  return(loc.data)
  
}

myScaleFunction = function (in.vector) {
  
  loc.vector <- in.vector
  loc.vector <- loc.vector - mean(loc.vector, na.rm = TRUE) # center the values at 0
  loc.vector <- loc.vector / sd(loc.vector, na.rm = TRUE) # scale the values by their standard deviation
  
  return (loc.vector)
  
}

scaleFeatures = function (in.data, in.class.arg) {
  
  loc.data <- in.data
  loc.scale.cols <- setdiff(names(loc.data), c(dat.cellid.arg, in.class.arg))
  loc.data <- loc.data %>% 
    mutate_at(loc.scale.cols, myScaleFunction)
  
  return(loc.data)
  
}








doScatterPlot = function (in.data, in.plot.x.arg, in.plot.y.arg, in.plot.group, in.plot.color = 1) {
  
  ggplot(in.data, aes_string(x = in.plot.x.arg, y = in.plot.y.arg, group = in.plot.group)) + 
    geom_point(alpha = 0.02) + 
    geom_path(aes_string(colour = in.plot.color), alpha = 0.5) + 
    theme_bw()
  
}


makeScatterPlot = function (in.data, in.plot.x.arg, in.plot.y.arg, in.plot.group, in.plot.color = NULL) {
  
  loc.plot = ggplot(in.data, aes_string(x = in.plot.x.arg, 
                                        y = in.plot.y.arg,
                                        group = in.plot.group)) + geom_point(alpha = 0.02)
  
  if (is.null(in.plot.color)) {
    loc.plot = loc.plot + geom_path(alpha = 0.5)
  } else {
    loc.plot = loc.plot + geom_path(aes_string(colour = in.plot.color), alpha = 0.5)
  }
  
  loc.plot = loc.plot + theme_bw()
  
  return(loc.plot)
}
library(dplyr)
library(data.table) 
require(ggplot2)
require(plotly)


#################################################
# CONSTANTS
#################################################

dat.cellid.arg <- 'cell_Id' # custom name for the new cell id attribute
dat.id.args.vec <- c('Image_Metadata_Site', 'objNuc_TrackObjects_Label')
dat.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
dat.timepoint.arg <- 'RealTime'

cell.feature.include.regex <- '^objCell_Intensity_MeanIntensity_imErkCorrOrig$|^objNuc_Intensity_MeanIntensity_imNucCorrBg$|^objCell_AreaShape*'
cell.feature.exclude.regex <- '.*(EulerNumber)+.*'

data.files.path <- 'data/'
result.files.path <- 'results/'

plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg <- 'objCell_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg <- dat.cellid.arg


#################################################
# PUBLIC FUNCTIONS
#################################################

createExperimentId = function () {
  
  loc.id <- paste(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = '')
  
  return(loc.id)
  
}

if (!exists('experiment.id')) experiment.id <- createExperimentId()

importRawDataset = function (in.file.name, in.class.arg) {
  
  loc.data <- importDataFromFile(in.file.name, in.class.arg, features.only = FALSE)
  
  return(loc.data)
  
}


importTrainingDataset = function (in.file.name, in.class.arg) {
  
  # Import test dataset from file
  loc.data <- importDataFromFile(in.file.name, in.class.arg, features.only = TRUE)
  
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


importTestDataset = function (in.file.name, in.class.arg) {
  
  # Import test dataset from file
  loc.data <- importDataFromFile(in.file.name, in.class.arg, features.only = TRUE)
  
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


importActualClassLabels = function (in.file.name, in.class.arg, relabel) {
  
  # Import whole dataset from file
  loc.data <- importTrainingDataset(in.file.name, in.class.arg)
  
  # Select cell_Id and class attributes only
  loc.data <- loc.data %>% 
    select_(dat.cellid.arg, in.class.arg)
  
  # Change class labels if desired
  if (relabel) {
    loc.data <- loc.data %>% 
      mutate_at(funs(replace(., . == TRUE, 'healthy')), .cols = in.class.arg) %>% 
      mutate_at(funs(replace(., . == FALSE, 'unhealthy')), .cols = in.class.arg)
  }
    
  loc.vec <- as.vector(loc.data[,in.class.arg])
  names(loc.vec) <- loc.data[,dat.cellid.arg]

  return(loc.vec)
  
}

# in.test.data -> data.frame loaded with function importTestDataset()
# in.true.data -> vector with actual classification for performance assessment 
evaluateDecisionTree = function (in.dt, in.file.name, in.class.arg, in.file.alias) {
  
  # Import raw dataset and remove class attribute to output it with new classification labels further down
  loc.data.out <- importRawDataset(in.file.name, in.class.arg)
  loc.data.out <- loc.data.out %>% dplyr::select(-get(in.class.arg))
  
  # Import test dataset
  loc.data.test <- importTestDataset(in.file.name, in.class.arg)
  
  # Prepare vector with true class labels for evaluation
  loc.class.act <- importActualClassLabels(in.file.name, in.class.arg, relabel = FALSE)
  
  # Initialize paths for output files
  loc.file.path <- paste0(result.files.path, experiment.id, '/')
  if(!dir.exists(loc.file.path))
    dir.create(loc.file.path)
  loc.file.name <- paste0(experiment.id, '_', in.file.alias)
  loc.pdf.file.path <- paste0(loc.file.path, experiment.id, '_dt.pdf')
  loc.txt.file.path <- paste0(loc.file.path, loc.file.name, '.txt')
  loc.png.file.path <- paste0(loc.file.path, loc.file.name, '.png')
  loc.csv.file.path <- paste0(loc.file.path, loc.file.name, '.csv')
  
  # Save PDF file of decision tree if not present yet
  if (!file.exists(loc.pdf.file.path)) {
    pdf(file = loc.pdf.file.path)
    prp(in.dt, under = TRUE, varlen = 0)
    dev.off()
  }
  
  # Predict classes of training dataset with decision tree
  loc.class.prob <- predict(in.dt, loc.data.test) # probabilities of class assignment
  
  assign("data.test", loc.data.test, envir = .GlobalEnv)
  
  assign("class.prob", loc.class.prob, envir = .GlobalEnv)
  
  
  loc.class.pred <- loc.class.prob[,'healthy']>0.5 # classes predicted by decision tree
  loc.conf.matrix <- table(loc.class.pred, loc.class.act)
  # TODO: Check if confusion matrix is 4x4
  
  
  # Add predicted class labels to output dataset
  loc.data.temp <- data.frame(c1 = as.integer(names(loc.class.pred)), c2 = loc.class.pred)
  colnames(loc.data.temp) <- c(dat.cellid.arg, in.class.arg)
  loc.data.out <- full_join(loc.data.out, loc.data.temp, by = dat.cellid.arg)
  rm(loc.data.temp)
  
  assign("conf.mat", loc.conf.matrix, envir = .GlobalEnv)
  
  # Save information and error statistics of classification to text file
  loc.tp <- loc.conf.matrix['TRUE', 'TRUE'] # true positives
  loc.fp <- loc.conf.matrix['TRUE', 'FALSE'] # false positives
  loc.tn <- loc.conf.matrix['FALSE', 'FALSE'] # true negatives
  loc.fn <- loc.conf.matrix['FALSE', 'TRUE'] # false negatives
  loc.tpr <- loc.tp / (loc.tp + loc.fn) # true positive rate = TP / number of positives
  loc.fpr <- loc.fp / (loc.fp + loc.tn) # false positive rate = FP / number of negatives
  loc.succ <- (loc.tp + loc.tn) / (loc.tp + loc.tn + loc.fp + loc.fn) # success rate, number of correct classi???cations divided by the total number of classi???cations
  loc.fileconn <- file(loc.txt.file.path)
  writeLines(c(paste0('Test dataset: ', names(in.file.alias)),
    paste0("TP: ", loc.tp), paste0("TN: ", loc.tn), paste0("FP: ", loc.fp), paste0("FN: ", loc.fn), paste0("TPR: ", loc.tpr), paste0("FPR: ", loc.fpr), paste0("Success Rate: ", loc.succ)), loc.fileconn)
  close(loc.fileconn)
  
  
  # Save plot to PNG file
  #png(filename = loc.png.file.path, width = 700, height = 500, units = 'px')
  doScatterPlot (loc.data.out, plot.x.arg, plot.y.arg, dat.cellid.arg, in.class.arg, loc.png.file.path)
  #dev.off()
  
  
  # Save dataset with classes assigned by decision tree to CSV file
  write.csv(loc.data.out, loc.csv.file.path)
  
}



#################################################
# PRIVATE FUNCTIONS
#################################################

importDataFromFile = function (in.file.name, in.class.arg, features.only) {
  
  loc.file.path <- paste0(data.files.path, in.file.name)
  
  # Import data from file and add cell IDs
  loc.data <- read.csv(loc.file.path)
  loc.data <- addCellIds(loc.data)
  
  # Test that all cells contain the same number of time points and save the number of timepoints
  test <- table(loc.data[,dat.cellid.arg])
  #stopifnot(var(test) == 0)
  #stopifnot(mean(test) %% 1 == 0)
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


doScatterPlot = function (in.data, in.plot.x.arg, in.plot.y.arg, in.plot.group, in.plot.color = 1, save.file = FALSE) {
  
  ggplot(in.data, aes_string(x = in.plot.x.arg, y = in.plot.y.arg, group = in.plot.group)) + 
    geom_point(alpha = 0.02) + 
    geom_path(aes_string(colour = in.plot.color), alpha = 0.5) + 
    theme_bw()
  
  if(is.character(save.file))
    ggsave(save.file)
  
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
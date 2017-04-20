##############################################
# Script to import data from time courses and prepare it for further analysis
# Author: Steven Cardini
# Spring 2017
##############################################

library(dplyr)
library(data.table)
setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")


##############################################
# Global variables

file.input.path <- 'data/20170117_test_multipulses_100_50_10_2percent_100ms_interval_5_ver2_manual.csv'

dat.cellid.arg <- 'cell_Id' # custom name for the new cell id attribute
dat.id.args.vec <- c('Image_Metadata_Site', 'objNuc_TrackObjects_Label')
dat.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
dat.timepoint.arg <- 'RealTime'
dat.class.arg <- 'mid.in.man'

dat.feature.grouping.args <- c(dat.cellid.arg, dat.class.arg)

cell.feature.include.regex <- '^objCell_Intensity_MeanIntensity_imErkCorrOrig$|^objNuc_Intensity_MeanIntensity_imNucCorrBg$|^objCell_AreaShape*'
cell.feature.exclude.regex <- '.*(EulerNumber)+.*'


###############################################
# Import data from file and add cell IDs

dat <- read.csv(file.input.path, header = TRUE)
dat <- addCellIds(dat)


###############################################
# Perform some tests on imported data

## test that all cells contain the same number of time points and save the number of timepoints
test <- table(dat[,dat.cellid.arg])
stopifnot(var(test) == 0)
stopifnot(mean(test) %% 1 == 0)
timepointsPerCell <- as.integer(mean(test))
rm(test)

## test that there is the identical classification (TRUE | FALSE) for all time points of a cell
numberOfCells <- max(dat[,dat.cellid.arg])
test <- dat %>% dplyr::select(one_of(c(dat.cellid.arg, dat.class.arg))) %>% group_by(.) %>% unique()
stopifnot(nrow(test) == numberOfCells)
rm(test)


##############################################
# Create data and metadata subsets for further analysis

dat.features <- dat %>% 
  dplyr::select(one_of(dat.cellid.arg, dat.timepoint.arg, dat.class.arg), matches(cell.feature.include.regex), -matches(cell.feature.exclude.regex)) %>% 
  dplyr::arrange_(.dots = c(dat.cellid.arg, dat.timepoint.arg))

dat.meta <- dat %>%
  dplyr::select(dplyr::one_of(dat.cellid.arg, dat.class.arg, dat.id.args.vec, dat.metadata.args.vec)) %>%
  dplyr::distinct() %>%
  dplyr::arrange_(.dots = dat.cellid.arg)


##############################################
# Add differences between time points as new features and aggregate features for each cell (coefficient of variation, mean)
dat.features.aggr.1 <- addDifferencesAndAggregate(dat.features %>% dplyr::select(-dplyr::one_of(dat.timepoint.arg)))

# Label cells FALSE if any of the features is an outlier (< 0.001 quantile OR > 0.999 quantile)
dat.features.aggr.2 <- labelOutliersAsFalse(dat.features.aggr.1, 0.001, 0.999)

# TODO: scale the features
dat.features.aggr.3 <- scaleFeatures(dat.features.aggr.2)

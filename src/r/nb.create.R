#require(e1071)
require(klaR)

require(data.table)
require(dplyr)
require(ggplot2)
require(plotly)



setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")

## Global variables

cell.id.arg <- 'cell_Id'
cell.id.args.vec <- c('Image_Metadata_Site', 'objNuc_TrackObjects_Label')
cell.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
cell.class.arg <- 'mid.in'
cell.feature.prefix.regex <- 'objCell_*'
cell.feature.exclude.regex <- '.*(Intensity|Euler)+.*'

plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg <- 'objCell_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg <- cell.id.arg
plot.color.arg <- cell.class.arg


attrib.scale.exclude <- c('objCell_Neighbors_FirstClosestObjectNumber_25',
                          'objCell_Neighbors_NumberOfNeighbors_25', 'objCell_Neighbors_PercentTouching_25',
                          'objCell_Neighbors_SecondClosestObjectNumber_25')



# import data from file and add cell IDs

dat <- fread("data/tCoursesSelected_midin_v2.csv")
dat <- addCellIds(dat, cell.id.arg, cell.id.args.vec)

# create data and metadata subsets for further analysis

dat.raw <- dplyr::select(dat, -dplyr::one_of(cell.id.args.vec, cell.metadata.args.vec))
dat.features <- dplyr::select(dat.raw, one_of(cell.id.arg, cell.class.arg), matches(cell.feature.prefix.regex), -matches(cell.feature.exclude.regex))
dat.aggr.meta <- dplyr::select(dat, dplyr::one_of(cell.id.arg, cell.class.arg, cell.id.args.vec, cell.metadata.args.vec)) %>%
  dplyr::distinct()
rm(dat) # delete raw data


# aggregate data (average) for each cell and add variance, min, max and median for some attributes

attrib.aggr.group <- c(cell.id.arg, cell.class.arg)
attrib.aggr.meanplus <- setdiff(names(dat.features), c(attrib.scale.exclude, cell.id.arg, cell.class.arg))
attrib.aggr.meanonly <- setdiff(attrib.scale.exclude, "objCell_AreaShape_EulerNumber_Mean")

dat.features.aggr <- aggregateCellData(dat.features, attrib.aggr.group, attrib.aggr.meanplus, attrib.aggr.meanonly)
dat.features.aggr.scaled <- scaleCellData(dat.features.aggr, c(attrib.scale.exclude, cell.id.arg, cell.class.arg))

# prepare training set data for Naive Bayesian classifier

dat.nb.training <- dat.features.aggr.scaled
dat.nb.training$mid.in[dat.nb.training$mid.in==TRUE] <- "healthy"
dat.nb.training$mid.in[dat.nb.training$mid.in==FALSE] <- "not healthy"

dat.nb.prediction <- dat.nb.training %>%
  ungroup() %>%
  dplyr::select(-get(cell.class.arg))


# use Naive Bayesian classifier
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Na%C3%AFve_Bayes

nb.classifier <- NaiveBayes(as.factor(mid.in) ~ ., data = dat.nb.training)
nb.predicted <- predict(nb.classifier, dat.nb.training[,-2])
nb.predicted <- (nb.predicted$posterior[,1] > 0.5)

dat.raw.predicted.nb <- data.table(dat.raw)
dat.raw.predicted.nb[, (cell.class.arg) := nb.predicted[get(cell.id.arg)]]


# Plots

# plot original data set
doScatterPlot (dat.raw, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)
# plot prediction accorind to Naive Bayesian approach (nb)
doScatterPlot (dat.raw.predicted.nb, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)



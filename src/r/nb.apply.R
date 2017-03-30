require(data.table)
require(dplyr)
require(ggplot2)
require(plotly)


setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")

## Global variables

file.path <- 'data/20170228_test_multipulses_60min_break.csv'

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

attrib.scale.exclude <- c()


# import data from file and add cell IDs

dat <- fread(file.path)
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

dat.features.aggr <- aggregateCellData(dat.features, attrib.aggr.group, attrib.aggr.meanplus)
dat.features.aggr.scaled <- scaleCellData(dat.features.aggr, c(attrib.scale.exclude, cell.id.arg, cell.class.arg))

# remove class from data
dat.nb.input <- dat.features.aggr.scaled %>%
  dplyr::ungroup() %>%
  dplyr::select(-get(cell.class.arg))


# apply Naive Bayesian classifier

dat.nb.result <- predict(nb.classifier, dat.nb.input)
dat.nb.result <- (dat.nb.result$posterior[,1] > 0.5)

dat.raw.predicted.nb <- data.table(dat.raw)
dat.raw.predicted.nb[, (cell.class.arg) := dat.nb.result[get(cell.id.arg)]]

# build confusion matrix
table(dat.nb.result, dat.features.aggr.scaled$mid.in)

# plot original data set
doScatterPlot (dat.raw, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)

# plot result according to Naive Bayesian classifier
doScatterPlot (dat.raw.predicted.nb, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)

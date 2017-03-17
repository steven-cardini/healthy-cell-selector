require(data.table)
require(dplyr)

require(rpart)
require(rpart.plot)
require(RWeka)

require(ggplot2)
require(plotly)

setwd("C:/Users/cst/Documents/Programmierung/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")

## Global variables

cell.id.arg <- 'cell_Id'
cell.id.args.vec <- c('Image_Metadata_Site', 'Well', 'objNuc_TrackObjects_Label')
cell.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
cell.class.arg <- 'mid.in'
cell.feature.prefix <- 'objCell_'

plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg <- 'objNuc_Intensity_MeanIntensity_imErkCorrOrig + objCyto_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg <- cell.id.arg
plot.color.arg <- cell.class.arg

scale.exlude.args.vec <- c('objCell_AreaShape_EulerNumber', 'objCell_Neighbors_FirstClosestObjectNumber_25',
                           'objCell_Neighbors_NumberOfNeighbors_25', 'objCell_Neighbors_PercentTouching_25',
                           'objCell_Neighbors_PercentTouching_25', 'objCell_Neighbors_SecondClosestObjectNumber_25')



# import data from file and add cell IDs

dat <- fread("data/tCoursesSelected_midInCol.csv")
dat <- addCellIds(dat, cell.id.arg, cell.id.args.vec)

# create data and metadata subsets for further analysis

#dat.raw <- dplyr::select(dat, -dplyr::one_of(cell.id.args.vec, cell.metadata.args.vec))
dat.features <- dplyr::select(dat, one_of(cell.id.arg, cell.class.arg), starts_with(cell.feature.prefix))
dat.aggr.meta <- dplyr::select(dat, dplyr::one_of(cell.id.arg, cell.class.arg, cell.id.args.vec, cell.metadata.args.vec)) %>%
  dplyr::distinct()
rm(dat) # delete raw data


# aggregate data (average) for each cell and add variance, min, max and median for some attributes

aggr.attribs.group <- c(cell.id.arg, cell.class.arg)
aggr.attribs.meanplus <- setdiff(names(dat.features), c(scale.exlude.args.vec, cell.id.arg, cell.class.arg))
aggr.attribs.meanonly <- scale.exlude.args.vec

dat.features.aggr <- aggregateCellData(dat.features, aggr.attribs.group, aggr.attribs.meanplus, aggr.attribs.meanonly)

# TODO: fix scaling
dat.features.aggr.scaled <- scaleCellData(dat.features.aggr, c(scale.exlude.args.vec, cell.id.arg, cell.class.arg))  



# Build decision tree
# http://data-mining.business-intelligence.uoc.edu/home/j48-decision-tree
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Packages/RWeka/Weka_classifier_trees

dt <- rpart(mid.in~., data=dt.avgData.scaled)
prp(dt)
summary(dt)

# dt2 <- J48(mid.in~., data=dt.avgData.scaled)
# summary(dt2)


# plots

# plot cells and add path tracks
doScatterPlot (cells.data, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)

# interactive plot
p1 = makeScatterPlot (cells.data, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)
ggplotly(p1)

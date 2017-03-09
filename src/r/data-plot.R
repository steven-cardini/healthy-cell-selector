require(data.table)
require(dplyr)

require(rpart)
require(rpart.plot)

require(ggplot2)
require(plotly)

setwd("C:/Users/cst/Documents/Programmierung/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")

## Global variables

cell.id.arg <- 'cell_Id'
cell.id.args.vec <- c('Image_Metadata_Site', 'Well', 'objNuc_TrackObjects_Label')
cell.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
cell.class.arg <- 'mid.in'

plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg <- 'objNuc_Intensity_MeanIntensity_imErkCorrOrig + objCyto_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg <- cell.id.arg
plot.color.arg <- cell.class.arg

decisiontree.args.vec <- c('objCell_AreaShape_Area', 'objCell_AreaShape_Compactness', 'objCell_AreaShape_Eccentricity',
                           'objCell_AreaShape_EulerNumber', 'objCell_AreaShape_Extent', 'objCell_AreaShape_FormFactor',
                           'objCell_AreaShape_MajorAxisLength', 'objCell_AreaShape_MaxFeretDiameter', 'objCell_AreaShape_MaximumRadius',
                           'objCell_AreaShape_MeanRadius', 'objCell_AreaShape_MedianRadius', 'objCell_AreaShape_MinFeretDiameter',
                           'objCell_AreaShape_MinorAxisLength', 'objCell_AreaShape_Orientation', 'objCell_AreaShape_Perimeter',
                           'objCell_AreaShape_Solidity', 'objCell_Neighbors_AngleBetweenNeighbors_25', 
                           'objCell_Neighbors_FirstClosestDistance_25', 'objCell_Neighbors_FirstClosestObjectNumber_25',
                           'objCell_Neighbors_NumberOfNeighbors_25', 'objCell_Neighbors_PercentTouching_25',
                           'objCell_Neighbors_SecondClosestDistance_25', 'objCell_Neighbors_SecondClosestObjectNumber_25')



# setdiff(names(cells.data)[names(cells.data) %like% 'objCell'], c('objCell_AreaShape_Compactness'))

scale.exlude.args.vec <- c('objCell_AreaShape_EulerNumber', 'objCell_Neighbors_FirstClosestObjectNumber_25',
                           'objCell_Neighbors_NumberOfNeighbors_25', 'objCell_Neighbors_PercentTouching_25',
                           'objCell_Neighbors_PercentTouching_25', 'objCell_Neighbors_SecondClosestObjectNumber_25', 'mid.in')


# import data from file and add cell IDs

dat <- fread("data/tCoursesSelected_midInCol.csv")
dat <- addCellIds(dat, cell.id.arg, cell.id.args.vec)

# create data and metadata subsets for further analysis

cells.data <- getCellData(dat, cell.id.args.vec, cell.metadata.args.vec)
cells.metadata <- getCellMetadata(dat, cell.id.arg, cell.id.args.vec, cell.metadata.args.vec)
rm(dat) # delete raw data

cells.averageData <- getAverageCellData(cells.data, cell.id.arg, cell.class.arg, decisiontree.args.vec)
cells.averageData.scaled <- getScaledData(cells.averageData, scale.exlude.args.vec)

# Decision tree
# http://data-mining.business-intelligence.uoc.edu/home/j48-decision-tree
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Packages/RWeka/Weka_classifier_trees

dt <- rpart(mid.in~., data=cells.averageData.scaled)
prp(dt)
summary(dt)

# require(RWeka)
# dt2 <- J48(mid.in~., data=cells.averageData)
# summary(dt2)


# plots

# plot cells and add path tracks
doScatterPlot (cells.data, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)

# interactive plot
p1 = makeScatterPlot (cells.data, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)
ggplotly(p1)

require(data.table)
require(dplyr)
require(ggplot2)
require(plotly)

require(rpart)
require(rpart.plot)
require(RWeka)

setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")

## Global variables

file.path <- 'data/20170117_test_multipulses_100_50_10_2percent_100ms_interval_5_ver2_manual.csv'

cell.id.arg <- 'cell_Id'
cell.id.args.vec <- c('Image_Metadata_Site', 'objNuc_TrackObjects_Label')
cell.metadata.args.vec <- c('Stimulation_duration', 'Stimulation_intensity', 'Stimulation_treatment')
cell.class.arg <- 'mid.in'
cell.feature.prefix.regex <- 'objCell_*'
cell.feature.exclude.regex <- '.*(Intensity|Neighbors)+.*'

plot.x.arg <- 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg <- 'objCell_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg <- cell.id.arg
plot.color.arg <- cell.class.arg

attrib.scale.exclude <- c('objCell_AreaShape_EulerNumber')



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
attrib.aggr.meanonly <- attrib.scale.exclude

dat.features.aggr <- aggregateCellData(dat.features, attrib.aggr.group, attrib.aggr.meanplus, attrib.aggr.meanonly)
dat.features.aggr.scaled <- scaleCellData(dat.features.aggr, c(attrib.scale.exclude, cell.id.arg, cell.class.arg))  


# Build decision tree (rpart)# Build decision tree (rpart)
# http://www.statmethods.net/advstats/cart.html
# https://www.r-bloggers.com/classification-trees-using-the-rpart-function/


dat.dt.input <- dat.features.aggr.scaled
dat.dt.input$mid.in[dat.dt.input$mid.in==TRUE] <- "healthy"
dat.dt.input$mid.in[dat.dt.input$mid.in==FALSE] <- "not healthy"

dat.dt.predict <- dat.dt.input %>%
  dplyr::ungroup() %>%
  dplyr::select(-get(cell.class.arg))


# Build the decision tree according to Gini split method

dt.gini <- rpart(mid.in~., data=dat.dt.input, parms=list(split="gini"), control=rpart.control(cp = 0.01, minsplit = 8))
prp(dt.gini, extra = 2, under = TRUE, varlen = 0)
summary(dt.gini)

pred.gini <- predict(dt.gini, type="class")
sprintf("Confusion Matrix for dt.gini")
table(pred.gini, dat.dt.input$mid.in)

predicted <- predict(dt.gini, dat.dt.predict)
predicted <- (predicted[,1] > 0.5)

dat.raw.predicted.gini <- data.table(dat.raw)
dat.raw.predicted.gini[, (cell.class.arg) := predicted[get(cell.id.arg)]]


# Build the decision tree according to Information split method

dt.info <- rpart(mid.in~., data=dat.dt.input, parms=list(split="information"), control=rpart.control(cp = 0.01, minsplit = 8))
prp(dt.info, extra = 2, under = TRUE, varlen = 0)
summary(dt.info)

pred.info <- predict(dt.info, type="class")
sprintf("Confusion Matrix for dt.info")
table(pred.info, dat.dt.input$mid.in)

predicted <- predict(dt.info, dat.dt.predict)
predicted <- (predicted[,1] > 0.5)

dat.raw.predicted.info <- data.table(dat.raw)
dat.raw.predicted.info[, (cell.class.arg) := predicted[get(cell.id.arg)]]


# Build decision tree (RWeka)
# http://data-mining.business-intelligence.uoc.edu/home/j48-decision-tree
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Packages/RWeka/Weka_classifier_trees
# dt2 <- J48(mid.in~., data=dt.avgData.scaled)
# summary(dt2)


# Plots

# plot original data set
doScatterPlot (dat.raw, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)
# plot prediction according to decision tree (Gini)
doScatterPlot (dat.raw.predicted.gini, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)
# plot prediction according to decision tree (information)
doScatterPlot (dat.raw.predicted.info, plot.x.arg, plot.y.arg, cell.id.arg, plot.color.arg)


library(dplyr)
library(rpart)
library(rpart.plot)

setwd("C:/Code/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")


# Build decision tree (rpart)# Build decision tree (rpart)
# http://www.statmethods.net/advstats/cart.html
# https://www.r-bloggers.com/classification-trees-using-the-rpart-function/


dat.dt.input <- dat.features.aggr.3 %>% 
  mutate_at(funs(replace(., . == TRUE, 'healthy')), .cols = dat.class.arg) %>% 
  mutate_at(funs(replace(., . == FALSE, 'unhealthy')), .cols = dat.class.arg)


dat.dt.predicted <- dat.dt.input %>%
  dplyr::select(-get(dat.class.arg))


# Build the decision tree according to Gini split method

#dt.gini <- rpart(mid.in~., data=dat.dt.input, parms=list(split="gini"))
dt.gini <- rpart(mid.in.man~., data=dat.dt.input, parms=list(split="gini"))
prp(dt.gini, under = TRUE, varlen = 0)
summary(dt.gini)

pred.gini <- predict(dt.gini, type="class")
sprintf("Confusion Matrix for dt.gini")
table(pred.gini, dat.dt.input$mid.in)

predicted <- predict(dt.gini, dat.dt.predicted)
predicted <- (predicted[,1] > 0.5)

# TODO: write output to CSV
dat.raw.predicted.gini <- data.table(dat.raw)
dat.raw.predicted.gini[, (cell.class.arg) := predicted[get(cell.id.arg)]]
write.csv(dat.raw.predicted.gini, 'data/data.predicted.gini.csv')


# Build the decision tree according to Information split method

#dt.info <- rpart(mid.in~., data=dat.dt.input, parms=list(split="information"))
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


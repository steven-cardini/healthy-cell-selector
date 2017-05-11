#########################################################################################################################
# Script containing general functions
# Author: Steven Cardini
# Spring 2017
#########################################################################################################################


#########################################################################################################################
#########################################################################################################################
### PUBLIC FUNCTIONS
#########################################################################################################################
#########################################################################################################################


###### cv ###################################################################
# IN: vector of numeric data
# OUT: vector of numeric data
# Calculates the coefficient of variation (CV) from values in a vector,
# ignoring NA values
##############################################################################
cv = function (in.vector) {
  loc.vector <- in.vector[!is.na(x)] # remove NA values
  if (mean(loc.vector)==0 | sd(loc.vector)/mean(loc.vector) > .Machine$integer.max)
    return (.Machine$integer.max)
  return(sd(loc.vector) / mean(loc.vector))
}


###### createExperimentId ####################################################
# generates a random experiment ID
##############################################################################
initializeExperiment = function (model.type) {
  exp.id <- paste0(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = '')
  # assign global variables for experiment ID and experiment results file path
  assign("G.experiment.id", exp.id, envir = .GlobalEnv)
  assign("G.experiment.model", model.type, envir = .GlobalEnv)
  assign("G.experiment.files.path", paste0(G.result.files.path, G.experiment.id, '/'), envir = .GlobalEnv)
  # create the experiment's results folder
  if(!dir.exists(G.experiment.files.path))
    dir.create(G.experiment.files.path)
  return(exp.id)
}


###### saveExperimentInfo ####################################################
# IN: 
# OUT: 
##############################################################################
saveExperimentInfo = function (model.params = NA) {
  output.file <- paste0(G.experiment.files.path, G.experiment.id, '_info.txt')
  if (is.na(model.params))
    model.params = "none"
  fileconn <- file(output.file)
  writeLines(c(paste0('Timestamp: ', Sys.time()),
               '--------------------------------',
               paste0('Model: ', G.experiment.model),
               paste0('Model params: ', model.params), 
               paste0('Features included: ', G.feature.include.regex),
               paste0('Features excluded: ', G.feature.exclude.regex),
               paste0('Time differences as features: ', G.feat.timediffs),
               paste0('Aggregate functions: ', paste0(G.feat.aggr.fun, collapse = ' / ')),
               paste0('Features were scaled: ', G.feat.scale)),
             fileconn)
  close(fileconn)
}


###### saveDecisionTree ######################################################
# Saves a pretty print of the decision tree to a PDF file
# IN: 
# OUT: 
##############################################################################
saveDecisionTree = function (dt.model) {
  output.file <- paste0(G.experiment.files.path, G.experiment.id, '_dt.pdf')
  pdf(file = output.file)
  prp(dt.model, under = TRUE, varlen = 0)
  dev.off()
}



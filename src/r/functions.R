library(dplyr)
library(data.table) 

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
addDifferencesAndAggregate = function (in.data) {
  loc.data <- in.data %>%
    group_by_(.dots = dat.feature.grouping.args) %>%
    mutate_each(funs(diffs = c(NA, diff(.)))) %>%
    na.omit() %>%
    summarise_each(funs(col.cv = cv(.), col.mn = mean(.)))
  
  return(loc.data)
}

labelOutliersAsFalse = function (in.data, in.quantile.lower, in.quantile.upper) {
  
  loc.data <- in.data
  features <- setdiff(names(loc.data), dat.feature.grouping.args)
  setDT(loc.data)[,  (dat.class.arg) := Reduce(`&`, lapply(.SD, function(x) x < quantile(x, in.quantile.upper) & 
                                             x > quantile(x, in.quantile.lower))), .SDcols = features]
  
  return(loc.data)
  
}

myScaleFunction = function (in.vector) {
  
  loc.vector <- in.vector
  loc.vector <- loc.vector - mean(loc.vector, na.rm = TRUE) # center the values at 0
  loc.vector <- loc.vector / sd(loc.vector, na.rm = TRUE) # scale the values by their standard deviation
  
  return (loc.vector)
  
}

scaleFeatures = function (in.data) {
  
  loc.data <- in.data
  loc.scale.cols <- setdiff(names(loc.data), dat.feature.grouping.args)
  loc.data <- loc.data %>% mutate_at(loc.scale.cols, myScaleFunction)
  
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
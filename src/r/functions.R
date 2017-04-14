library(dplyr)
library(data.table) 

addCellIds = function (in.data, in.cellid.arg, in.id.args.vec) {
  
  # add cell IDs to data set
  loc.data <- in.data %>%
    dplyr::select(dplyr::one_of(in.id.args.vec)) %>%
    dplyr::distinct()
  loc.data[,in.cellid.arg] <- 1:nrow(loc.data)
  
  out.data <- dplyr::inner_join(loc.data, in.data, by = in.id.args.vec) # now equals input data set plus new row in.cell.id.arg
  
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
addDifferencesAndAggregate = function (in.data, in.grouping.args) {
  loc.data <- in.data %>%
    group_by_(.dots = in.grouping.args) %>%
    mutate_each(funs(diffs = c(NA, diff(.)))) %>%
    na.omit() %>%
    summarise_each(funs(col.cv = cv(.), col.mn = mean(.)))
  
  return(loc.data)
}


labelOutliers = function (in.data, in.class.arg, in.grouping.args) {
  
  loc.data <- in.data
  features <- setdiff(names(loc.data), in.grouping.args)
  setDT(loc.data)[,  (in.class.arg) := Reduce(`&`, lapply(.SD, function(x) x < quantile(x, 0.99) & 
                                             x > quantile(x, .01))), .SDcols = features]
  
  return(loc.data)
  
}


scaleCellData = function (in.data, in.attrib.scale.exclude) {
  
  loc.attrib.scale.exclude.ind <- grep(paste(in.attrib.scale.exclude, collapse="|"), names(in.data))
  out.data <- in.data
  out.data[,-c(loc.attrib.scale.exclude.ind)] <- scale(out.data[,-c(loc.attrib.scale.exclude.ind)])
  
  return(out.data)
  
}



# TODO: probably obsolete function
aggregateCellData = function (in.data, in.attribs.group, in.attribs.meanplus, in.attribs.meanonly = NULL) {
  
  loc.dat.1 <- in.data %>%
    dplyr::group_by_(.dots = in.attribs.group) %>%
    #dplyr::summarise_at(.cols = in.attribs.meanplus, .funs = c(median="median", var="var", IQR="IQR"))
    dplyr::summarise_at(.cols = in.attribs.meanplus, .funs = c(Mean="mean", Var="var", Min="min", Max="max"))
  
  if(is.null(in.attribs.meanonly)) {
    out.data <- loc.dat.1
  } else {
    loc.dat.2 <- in.data %>%
      dplyr::group_by_(.dots = in.attribs.group) %>%
      dplyr::summarise_at(.cols = in.attribs.meanonly, .funs = c(Mean="mean"))
    
    out.data <- dplyr::full_join(loc.dat.1, loc.dat.2, by=c("cell_Id", "mid.in"))
  }
  
  return(out.data)
  
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
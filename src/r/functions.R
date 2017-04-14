require(data.table)
require(dplyr)

addCellIds = function (in.data, in.cell.id.arg, in.cell.id.args.vec) {
  
  # add cell IDs to data set
  loc.dat.temp <- dplyr::select(in.data, dplyr::one_of(in.cell.id.args.vec)) %>%
    dplyr::distinct()
  loc.dat.temp[,in.cell.id.arg] <- 1:nrow(loc.dat.temp)
  
  out.data <- dplyr::inner_join(loc.dat.temp, in.data, by = in.cell.id.args.vec) # now equals input data set plus new row in.cell.id.arg
  return(out.data)
}

# calculate the coefficient of variation (CV) for a statistic, ignoring NA values
cv = function (x) {
  x <- x[!is.na(x)] # remove NA values
  
  res <- sd(x) / mean(x)
  return(res)
}

# TODO: add time points and sort by those
# INPUT: data set consisting of grouping attributes (ID, class) + several features across the columns
addDifferencesAndAggregate = function (in.data, in.attribs.group) {
  loc.data <- in.data %>%
    group_by_(.dots = in.attribs.group) %>%
    mutate_each(funs(diffs = c(NA, diff(.)))) %>%
    na.omit() %>%
    summarise_each(funs(col.cv = cv(.), col.mn = mean(.)))
}


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

scaleCellData = function (in.data, in.attrib.scale.exclude) {
  
  loc.attrib.scale.exclude.ind <- grep(paste(in.attrib.scale.exclude, collapse="|"), names(in.data))
  out.data <- in.data
  out.data[,-c(loc.attrib.scale.exclude.ind)] <- scale(out.data[,-c(loc.attrib.scale.exclude.ind)])
  
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
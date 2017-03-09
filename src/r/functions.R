require(data.table)
require(dplyr)

addCellIds = function (in.data, in.cell.id.arg, in.cell.id.args.vec) {
  
  # add cell IDs to data set
  loc.dat.temp <- dplyr::select(in.data, dplyr::one_of(in.cell.id.args.vec))
  loc.dat.temp <- dplyr::distinct(loc.dat.temp)
  loc.dat.temp[,in.cell.id.arg] <- 1:nrow(loc.dat.temp)
  
  out.data <- dplyr::inner_join(loc.dat.temp, in.data, by = in.cell.id.args.vec) # now equals input data set plus new row in.cell.id.arg
  return(out.data)
}


getCellData = function (in.data, in.cell.id.args.vec, in.cell.metadata.args.vec) {
  
  # remove cell id group columns as well as metadata columns
  out.data <- dplyr::select(in.data, -dplyr::one_of(in.cell.id.args.vec, in.cell.metadata.args.vec)) 
  
  return(out.data)
}


getCellMetadata = function (in.data, in.cell.id.arg, in.cell.id.args.vec, in.cell.metadata.args.vec) {
  
  out.data <- dplyr::select(in.data, dplyr::one_of(in.cell.id.arg, in.cell.id.args.vec, in.cell.metadata.args.vec))
  out.data <- dplyr::distinct(out.data)
  
  return(out.data)
}


getAverageCellData = function (in.data, in.cell.id.arg, in.cell.class.arg, in.select.args.vec) {
  
  # group data by cell id and average other attributes, return only relevant attributes
  out.data <- in.data %>%
                group_by_(in.cell.id.arg, in.cell.class.arg) %>%
                summarise_each(funs(mean)) %>%
                ungroup() %>%
                select(one_of(in.cell.class.arg, in.select.args.vec))
  
  return(out.data)
}

getScaledData = function (in.data, in.scale.exlude.args.vec) {
  
  temp.index = match(in.scale.exlude.args.vec, names(in.data))
  out.data <- in.data
  out.data[,-c(temp.index)] <- scale(out.data[,-c(temp.index)])
  
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
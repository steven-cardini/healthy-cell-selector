require(data.table)

addCellIds = function (in.data, in.cell.id.arg, in.cell.id.args.vec) {
  
  # add cell IDs to data set
  loc.dat.temp <- dplyr::select(in.data, dplyr::one_of(in.cell.id.args.vec))
  loc.dat.temp <- dplyr::distinct(loc.dat.temp)
  loc.dat.temp[,in.cell.id.arg] <- 1:nrow(loc.dat.temp)
  
  out.data <- dplyr::inner_join(loc.dat.temp, in.data, by = in.cell.id.args.vec) # now equals input data set plus new row in.cell.id.arg
  return (out.data)
}


getCellData = function (in.data, in.cell.id.args.vec, in.cell.metadata.args.vec) {
  
  # remove cell id group columns as well as metadata columns
  out.data <- dplyr::select(in.data, -dplyr::one_of(in.cell.id.args.vec, in.cell.metadata.args.vec)) 
  
  return (out.data)
}


getCellMetadata = function (in.data, in.cell.id.arg, in.cell.id.args.vec, in.cell.metadata.args.vec) {
  
  out.data <- dplyr::select(in.data, dplyr::one_of(in.cell.id.arg, in.cell.id.args.vec, in.cell.metadata.args.vec))
  out.data <- dplyr::distinct(out.data)
  
  return (out.data)
  
}


myGgplotScatter = function(in.dat, in.plot.x, in.plot.y, in.plot.group, in.plot.col = NULL) {
  loc.p = ggplot(in.dat, aes_string(x = in.plot.x, 
                                    y = in.plot.y,
                                    group = in.plot.group)) +
    geom_point(alpha = 0.02)
  
  if (is.null(in.plot.col)) {
    loc.p = loc.p + geom_path(alpha = 0.5)
  } else
    loc.p = loc.p + geom_path(aes_string(colour = in.plot.col), alpha = 0.5)
  
  loc.p = loc.p +
    theme_bw()
  
  return(loc.p)
}
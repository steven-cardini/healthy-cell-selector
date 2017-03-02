require(data.table)

addCellIds = function (in.dat, in.id.arg, in.id.group.args) {
  
  loc.id.group.vals = dplyr::select(in.dat, dplyr::one_of(in.id.group.args))
  
  for (i in 1:nrow(in.dat)) {
    in.dat[i, (in.id.arg) := paste(loc.id.group.vals[i], collapse = "_")]
  }
  
  return (in.dat)
  
}

# TODO: add padding
# dplyr::rename(loc.id.group.vals, )
# for (col.name in names(loc.id.group.vals)) {
#  loc.id.group.vals[, (col.name) := sprintf("%03s", loc.id.group.vals[,(col.name)])]
# }

# loc.ids = tidyr::unite(loc.id.group.vals, (in.id.group.args[1]), (in.id.group.args[2]), sep = "_")
# in.dat [, (in.id.arg) := loc.ids]

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
require(data.table)
require(ggplot2)
require(plotly)

setwd("C:/Users/cst/Documents/Programmierung/eclipse-workspaces/java/healthy-cell-selector")
source("src/r/functions.R")



## Global variables

data.id.arg = 'obj_Id'
data.id.group.args = c('Image_Metadata_Site', 'Well', 'objNuc_TrackObjects_Label')

plot.x.arg = 'objNuc_Intensity_MeanIntensity_imNucCorrBg'
plot.y.arg = 'objNuc_Intensity_MeanIntensity_imErkCorrOrig + objCyto_Intensity_MeanIntensity_imErkCorrOrig'
plot.group.arg = 'objNuc_TrackObjects_Label_uni'
plot.color.arg = 'mid.in'


dat = fread("data/tCoursesSelected_midInCol.csv")
dat = addCellIds (dat, data.id.arg, data.id.group.args)


#dat[, objNuc_TrackObjects_Label_uni := sprintf("%04d_%04d", Image_Metadata_Site, objNuc_TrackObjects_Label)]


p1 = myGgplotScatter(dat, plot.x.arg, plot.y.arg, plot.group.arg, in.plot.col = plot.col.arg)

ggplotly(p1)


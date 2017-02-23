# Healthy Cell Selector
One problem with automatic cell segmentation is that the output includes cells that are not suitable for further analysis for various reasons, e.g. because the cytosol had a too low signal-to-noise ratio for accurate segmentation of the cell, or because the cell was dying, or …

To get a training set for "healthy" cells out of the data, one can select cells based on fluorescence marker intensities. It is important that for all time points the fluorescence intensities are within the selected range.

The aim of this part is to assess other features (other than the fluorescence intensities) in predicting the cells to be "healthy" or not. Which features differ among "healthy" and other cells?
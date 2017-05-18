# Plan for upcoming Weeks

## Week 21
- Add support vector machine classifier
- Reduce amount of FALSE POSITIVES predicted by classifiers
- Evaluate classifiers based on manually annotated test datasets

## Week 22
- Maybe add feature: comparison after treatment to before treatment

# Project Journal

## 18.05.2017
14:15 - 18:15
- finished work on Wilcoxon tests for outliers' features vs. non-outliers' features
- implemented function to evaluate a dataset generically (make boxplots of features, calculate Wilcoxon tests of outliers vs. non-outliers for each feature)

## 11.05.2017
13:15 - 18:30
- made code more generic and integrated Naive Bayes evaluation
- started working on Wilcox Test for outliers' features vs. non-outliers' features

## 05.05.2017
15:45 - 16:15
- Project Meeting with Prof. B Anrig

## 04.05.2017
14:00 - 16:00
- Added code to save boxplots of the (normalized) features, and visualize outliers' paths across the boxplots

## 27.04.2017
13:15 - 18:45
- Added function to label outliers globally across all features (after scaling)
- Started to create box plot of scaled features

## 23.04.2017
16:00 - 20:00
- Rewrote scripts to enable generic evaluation of different decision tree input

## 22.04.2017
16:30 - 19:30
- Added generic function to evaluate a decision tree with a given test dataset
- Added experiment id

## 21.04.2017
19:30 - 21:00
- Added generic functions to import datasets

16:00 - 16:30
- Project Meeting with Prof. B. Anrig

## 20.04.2017
14:00 - 18:30
- Some refactoring to use dplyr and data.frames instead of data.table
- Refined the labelling of outliers (use 0.001 and 0.999 quantiles)
- Added a function to scale aggregated data

## 14.04.2017
10:00 - 14:00
- Some refactoring and simplification of the code (added new script for data import and preparation)
- Label outliers as false (if any feature is below or above a certain quantile)

## 13.04.2017
13:00 - 18:00
- Added function to calculate differences between time points and aggregate the data
- Started work to label outliers as FALSE (even if they were labeled TRUE before)

## 06.04.2017
13:00 - 18:15
- Used more refined criteria for decision trees and other aggregate functions (IQR) for attributes
- Added evaluation / diagnostics (confusion matrix overview)
- Discussion with M. Dobrzynski about next steps
- Started work to add a measure for difference between time points

## 31.03.2017
16:00 - 16:20
- Project Meeting with Prof. B. Anrig

## 30.03.2017
13:00 - 18:00
- Implemented a first Naive Bayesian classifier (package klaR)
- Applied Gini decision tree as well as Naive Bayesian classifier to 3 other data sets for evaluation 

## 23.03.2017
14:00 - 17:30
- Modified rpart decision trees to split based on Gini vs. Information
- Created datasets with predicted classes according to decision trees
- Added confusion matrices for decision trees and plots for data sets according to decision trees

## 17.03.2017
17:00 - 19:00
- Added new input data
- Work on decision trees

10:50 - 11:10
- Project Meeting with Prof. B. Anrig

08:30 - 10:30
- Simplified data handling
- Re-designed data aggregation so that the aggregate data contains more information (variance, min, max)

## 09.03.2017
13:00 - 17:30
- Implemented functions in R to aggregate cellular data (average) and normalize / scale attributes
- First decision trees using RWeka and rpart

## 04.03.2017
12:00 - 15:00
- Work in RStudio to add cell IDs and separate data from metadata

## 03.03.2017
20:00 - 22:00
- R Online Tutorial

15:30 - 16:00
- Project Meeting with Prof. B. Anrig

## 02.03.2017
13:15 - 18:30
- Installation of R, R Studio and R Packages
- Getting familiar with R as well as data table, ggplot2, plotty and dployr packages
- First attempts to reproduce the scatter plots of healthy vs. unhealthy cells

## 23.02.2017
13:00 - 18:00
- Introduction to data formats and microscopy images with M. Dobrzynski
- Work on CSV Condenser (averages all values for a given instance / cell)
- First J48 Decision Tree with Weka / Java

## 21.02.2017
17:15 - 18:00
- Git repository setup

## 17.02.2017
17:30 - 18:00
- Kickoff Meeting with Prof. B. Anrig

09:30 - 17:00
- Introduction with Prof. O. Pertz and M. Dobrzynski
- Defined two projects: 1) assessment of cell features for sorting out badly segmented cells and 2) experiment with deep learning approach for cell segmentation
- Started reading two journal articles
- Initialized project 1)
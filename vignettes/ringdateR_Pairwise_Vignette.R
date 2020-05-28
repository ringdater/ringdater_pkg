## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message = FALSE, warning=FALSE-------------------------------------------
library(ringdater)

## -----------------------------------------------------------------------------
exampleData <- load_undated("example_data/UndatedSeries.csv")


## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- exampleData[1:5,1:5]
knitr::kable(df, caption = "Preview", floating.environment="sidewaystable")

## -----------------------------------------------------------------------------
detrending.plot.fun(undet.data = exampleData, first_series = "ID_021a", detrending_select = 3, splinewindow = 21, 
font_size = 12, axis_line_width = 0.5, plot_line = 0.5)
detrendedData <- normalise(the.data = exampleData, detrending_select = 3, splinewindow = 7)

## -----------------------------------------------------------------------------
pairwiseData <- lead_lag_analysis(the_data = detrendedData, mode = 1, neg_lag = -20, pos_lag = 20, complete = TRUE, shiny = FALSE)

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- pairwiseData[[1]][1:10,]
knitr::kable(df, caption = "Pairwise Corelations", floating.environment="sidewaystable")

## -----------------------------------------------------------------------------
line_plot(the_data = detrendedData, series_1 = "ID_021a", "ID_032a", lag = 30)

## -----------------------------------------------------------------------------
lead_lag_bar(the_data = as.data.frame(pairwiseData[2]), sample_1 = "ID_021b", sample_2 = "ID_032a")

## -----------------------------------------------------------------------------
heatmap_analysis(the_data = detrendedData, s1 = "ID_032a", s2 = "ID_021b") 

## -----------------------------------------------------------------------------
filtered_data <- filter_crossdates(the_data = as.data.frame(pairwiseData[1]), r_val = 0.6, p_val = 0.01, overlap = 30, target = "ID_032a")

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- filtered_data[,1:9]
knitr::kable(df, caption = "Filtered Data Preview", floating.environment="sidewaystable")

## -----------------------------------------------------------------------------
aligned_data <- align_series(the_data = detrendedData, cross_dates = filtered_data, sel_target = "ID_032a")

plot_all_series(aligned_data)

## -----------------------------------------------------------------------------
alignmentStats <- correl_replace(aligned_data)

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- alignmentStats
knitr::kable(df, caption = "Aligned Data", floating.environment="sidewaystable")


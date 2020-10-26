## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message = FALSE, warning=FALSE-------------------------------------------
library(ringdater)

## -----------------------------------------------------------------------------
undated <- load_undated("example_data/UndatedSeries.csv")
undated <- normalise(the.data = undated, detrending_select = 3, splinewindow = 21)

## -----------------------------------------------------------------------------
chronoData <- load_chron("example_data/chronologies/ExampleChron.csv")
chronoData<-normalise(chronoData, detrending_select = 3, splinewindow = 21)
chronoData<-data.frame(chronoData[,1], rowMeans(chronoData[,-1], na.rm = TRUE))
colnames(chronoData)<-c("year","chronoData")
plot(chronoData, type ="l", xlab = "Year", ylab = "Standardised growth index")

## -----------------------------------------------------------------------------
chron_n_series <- comb.NA(chronoData, undated[,-1], fill = NA)

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- chron_n_series[1:6,1:6]
knitr::kable(df, caption = "Preview of Combined Data", floating.environment="sidewaystable")


## -----------------------------------------------------------------------------
chron_comp <- lead_lag_analysis(the_data = chron_n_series, mode = 2, neg_lag = -20, pos_lag = 20, complete = TRUE)

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- chron_comp[[1]][2:9,]
knitr::kable(df, caption = "Chronology Corelations", floating.environment="sidewaystable")


## -----------------------------------------------------------------------------
line_plot(the_data = chron_n_series, series_1 = "chronoData", series_2 = "ID_021b", lag = 58)

## -----------------------------------------------------------------------------
lead_lag_bar(the_data = as.data.frame(chron_comp[2]), sample_1 = "chronoData", sample_2 = "ID_021b")

## -----------------------------------------------------------------------------
heatmap_analysis(the_data = chron_n_series, s1 = "chronoData", s2 = "ID_021b", neg_lag = -10, pos_lag = 10, center = 58, complete = FALSE, leg_size = 1)

## -----------------------------------------------------------------------------
filtered_data <- filter_crossdates(the_data = as.data.frame(chron_comp[1]), r_val = 0.6, p_val = 0.05, overlap = 40, target = colnames(chron_n_series)[2])

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- filtered_data[1:6,1:9]
knitr::kable(df, caption = "Filtered Data", floating.environment="sidewaystable")

## -----------------------------------------------------------------------------
aligned_data <- align_series(the_data = chron_n_series, cross_dates = filtered_data, sel_target = colnames(chron_n_series)[2])
plot_all_series(aligned_data)

## -----------------------------------------------------------------------------
alignmentStats <- correl_replace(aligned_data)

## ----comment='', echo=FALSE, results='asis'-----------------------------------
df <- alignmentStats[2:9,]
knitr::kable(df, caption = "Check Alignment Stats", floating.environment="sidewaystable")


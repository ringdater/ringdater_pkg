# RingdateR Read Me
Authors: David Reynolds, David Edge, Bryan Black
Development build - Experimental

Current Version: 1.1

Last updated: 30-5-24

contact: d.reynolds2@exeter.ac.uk

  <!-- badges: start -->
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ringdater/ringdater_pkg?branch=master&svg=true)](https://ci.appveyor.com/project/ringdater/ringdater_pkg)
  
<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active – The project has reached a stable, usable state and is being actively developed." /></a>
<a href="https://github.com/ringdater/ringdater_pkg/issues"><img alt="GitHub issues" src="https://img.shields.io/github/issues/ringdater/ringdater_pkg"></a>
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->
## RingdateR web page
The RingdateR webpage can be accessed <a href="https://ringdater.github.io/ringdater/">here</a>.

## About
RingdateR is an R Package and interactive shiny application for statistical and visual crossdating annually resolved data (e.g. tree rings or mollusc and fish otolith growth increments). RingdateR utilizes lead-lag analyses to identify possible matches between samples and presents running-lead-lag correlation analyses, presented as heat maps, to evaluate for potential errors within measurement timeseries. If significant crossdates are identified, RingdateR can automatically align the data ready for further analyses. 

## Goal
The overarching aim of RingdateR is to facilitate the rapid development of crossdated absolutely dated chronologies built using annually resolved growth rings (e.g. tree rings or mollusc growth rings). Developing multi-centennial to millennial length chronologies is a painstaking and time-consuming task. This is especially the case if working with dead-collected samples that have an unknown antiquity. As such, to find samples that crossdate can take a significant amount of time as large numbers of samples need to be measured and compared with every other sample. Whilst existing software exists for performing such analyses, RingdateR has been specifically optimized for analysing large numbers of dead-collected samples. Ultimately it is hoped that RingdateR will provide a useful tool for both the dendrochronology and sclerochronology communities in their pursuit for developing ever longer chronologies.

## Running RingdateR online
<a href="https://ringdater.shinyapps.io/launcher/">Use this link to launch the online version of RingdateR</a>.
<b>Please note that we strongly recommend you download and run RingdateR in RStudio if you decide to use it regularly</b>. There is limited access to the online version and the online server will time out after 30 minutes of inactive use. Any analyses not downloaded will be lost if the server times out. There are no usage limits when using RingdateR in RStudio.

## Running RingdateR in Rstudio
RingdateR can be used as a package in Rstudio. The package can be downloaded and installed directly from Github using the following code:

install.packages("devtools") 

library(devtools)

devtools::install_github("ringdater/ringdater_pkg")

## Requirements

RingdateR requires R version 4.0. There are also a number of dependencies, listed in the help file, that will automatically install when the RingdateR package is first installed. If you are currently using an older version of R, you can automatically install the latest version using the following code:

install.packages("installr")

library(installr)

updateR()

## Launching the RingdateR app in Rstudio
Once the RingdateR package is installed, use the following code to launch the RingdateR app:

library(ringdater)

run_ringdater()

## Help Files and Vignettes
## Using the RingdateR App (online and in RStudio)
<a href="https://ringdater.shinyapps.io/ringdater_shiny_vig_v2/"> RingdateR shiny app interactive vignette. </a>

<a href="https://ringdater.github.io/ringdater/RingdaetR_help_file.pdf"> RingdateR shiny app PDF vignette. </a>

## Running RingdateR in the RStuidio Console
<a href="https://ringdater.github.io/ringdater/RingdateR%20Pairwise%20Vignette.html"> Pairwise analysis in R console vignette. </a>

<a href="https://ringdater.github.io/ringdater/RingdateR%20Chronology%20Vignette.html"> Chronology analysis in R console vignette. </a>

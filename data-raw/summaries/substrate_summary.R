## /* substrate_summary.R --- 
## Filename: substrate_summary.R
## Description: Examine the substrate data
## Author: Noah Peart
## Created: Thu Feb 18 09:42:12 2016 (-0500)
## Last-Updated: Thu Feb 18 10:56:41 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Substrate Data"
##' output_format: 
##'   html_document:
##'     theme: readable
##'     highlight: zenburn
##'     toc: true
##' ---
##' 
##+setup, echo=FALSE, include=FALSE
library(knitr)
opts_chunk$set(fig.path='figures/', echo=TRUE, message=FALSE, cache=FALSE)

library(data.table)
library(DT)
library(visNetwork)
library(stringi)

dtopts <- list(scrollX=TRUE)

## Load the master file
library(seedsub.mas)
segsub <- copy(submas99c)

## /* end setup */
##'
##' # Overview
##'
##' The goal of this script is to explore the substrate master file
##' named `submas99c`.  This file can be loaded from `library(seedsub.mas)`.  See
##' [substrate_clean.R](../clean/substrate_clean.R) for the actual data transformation.
##' 
##' ## Columns:
##' + `CENS`: The year of census
##' + `CONTNAM`: The contour name
##' + `STPACE`: The starting pace of the segment.
##' + `SGDSP`: Segment displacement from the contour ('U'=up, 'D'=down) in (meters or paces?)
##' + `QPOS`: Quadrat position ?
##' + Then there are a bunch of substrate types:
##'
##'     - `BLA5`/`BLD5`: Bole aerial/Bole dead
##'     - `BSOIL`: Bare soil
##'     - `RCK`: Rock
##'     - `WATER`: water
##'     - `WDG5`/`WDG1`: Wood on the ground (at least 5cm/1cm)
##'     - `MSSG`: Moss
##'     - `TIPA`: Tip-up
##'     - `STPA`: ?
##'     - `LITT`/`LITM`/`LITC`/`LITD`: General/Mixed/Coniferous/Deciduous litters
##'     - `WDA5`/`WDA1`: Wood aerial (at least 5cm/1cm)
##'     - `MSWDA5`/`MSWDA1`: ?
##' + `ASPCL`: Aspect class
##' + `ELEVCL`: Elevation class
##' + `SEASON`: All in the summer, this was only collected in 2003
##' + `DATE`: Date of collection
##' + Then some combinations of substrate w/ underlying substrate:
##'     - `LITCRCK`: Coniferous litter on rock
##'     - `LITMRCK`: Mixed litter on rock
##'     - `MSRCK`: Moss on rock
##'     - `MSBLA5`: Moss on aerial bole at least 5cm diameter.
##'     - `MSWDG5`: Moss on dead wood at least 5cm diameter.
##' + `SUMG`: Sum on the ground?
##' + `SUMA`: Sum of aerial?
##' + `CORRECT`: Corrected in some way
##'
##' # Raw data
##'
##+raw-data, echo=FALSE

datatable(segsub, options=dtopts, caption = "The raw data")

## /* end raw-data */
##'
##+year-info, echo=FALSE
yrsconts <- segsub[, .(Count=.N, Contours=list(unique(CONTNAM))), by=CENS]
stpaces <- segsub[, .(Pace=list(STPACE)), by=c("CENS", "CONTNAM")]
yrs <- unique(segsub$CENS)

## /* end year-info */
##' # Years
##'
##' Substrate was sampled for various plots in
{{prettify(yrs, type="all")}}
##' .  The following tables summarise the contours sampled in each census and 
##' the paces (plots) sampled on each contour/census.
##'
##+year-output, echo=FALSE

## Contours/census years
datatable(yrsconts, options=dtopts, caption="Contours sampled in each year.")

## Plots/census years
datatable(stpaces, options=dtopts, 
  caption="Plots (contour/stpace) sampled in census years.")

## /* end year-output */
##'
##' # Repeated Samplings
##'
##' So, the only plots with repeated samplings were as follows:
##+repeated-samplings, echo=FALSE
reptab <- segsub[, .(
  `Times sampled`=uniqueN(CENS),
  `Years sampled`=list(unique(CENS))
  ), by=c("CONTNAM", "STPACE")][`Times sampled` > 1L]
datatable(reptab, options=dtopts, caption="Repeated sampling years for plots.")

## /* end repeated-samplings */
##'
##' 
##' # Seedling substrates
##'
##' Types of substrates recorded in the seedling data.  These are the types
##' of substrate that seedlings were recorded to be growing on in the field (protocols
##' varied by year - see (segdata_summary.R)[segdata_summary.R] for details).
##' 
##' Here is shown only the substrates the seedlings were growing directly on (ie
##' not the underlying substrates (`SUBON`)).  These need to correspond to columns
##' in the substrate data in order to determine the percentage cover of each type 
##' for seedling preferences.
##'
##+seed-substrates, echo=FALSE
## Contour plant data
load("../temp/segplants.rda")

## Table
tab <- dcast(segplants[, SUB, by=YEAR], YEAR ~ SUB, value.var="SUB", fun=length)
datatable(tab, caption="Substrates seedlings were growing on.")

## gather types

## /* end seed-substrates */
##'

##'
##' # Cleaning
##'
##' Not much to do here, data already has column for each variable.  See 
##' [substrate_clean.R](../clean/substrate_clean.R).
##'
##' + Normalize variable names: `CENS` -> `YEAR`
##' + Convert to integer where appropriate
##' + Fix the `DATE` column to be date type not string
##'

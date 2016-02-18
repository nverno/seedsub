## /* substrate_cover.R --- 
##' Filename: substrate_cover.R
##' Description: Compute percent covers for substrate data
##' Author: Noah Peart
##' Created: Thu Feb 18 10:36:36 2016 (-0500)
##' Last-Updated: Thu Feb 18 12:56:17 2016 (-0500)
##'           By: Noah Peart
##' */

## /* yaml */
##' ---
##' title: "Substrate Percent Cover"
##' output_format: 
##'   html_document:
##'     theme: readable
##'     highlight: zenburn
##'     toc: true
##' ---
##' 
#+setup, echo=FALSE, include=FALSE
library(knitr)
opts_chunk$set(fig.path='Figures/', echo=TRUE, message=FALSE, cache=FALSE)

library(data.table)
library(stringi)
library(DT)
library(d3heatmap)
dtopts <- list(scrollX=TRUE)

## Load data
load("../temp/segsub.rda")
load("../temp/segplants.rda")
load("../temp/segplots.rda")

## /* end setup */
##'
##' # Difficulties
##'
##'   + Protocols differ by year - different substrates sampled
##'   + Don't have substrate data for all the segments
##'   + Don't have substrate data collected the same year as seedling data (except 1999).
##' 
##' # Segments missing substrate
##' 
##' Some segments have plant data but no substrate data.  All the plant data from 1999
##' has associated substrate data.  For the other years it is more complicated since many
##' segments are missing substrate data from those years, but may have substrate data that 
##' was collected in another year.
##'
##+missing-substrate, echo=FALSE

nosub <- segplots[!(PID %in% segsub[, PID]), PID]
nosub_plts <- segplots[list(PID = nosub), .(CONTNAM, STPACE), on="PID"]
tab <- dcast(nosub_plts, CONTNAM ~ ., fun=list, fill="STPACE", value.var = "STPACE")
names(tab)[[2]] <- "STPACE"

datatable(tab, options=dtopts, caption="Plots with plant data but no substrate data.")

## Years/plots with missing substrate data, counts of plants
plants <- segplants[list(PID=nosub), on="PID"][!is.na(HT)]
plants <- plants[segplots[list(PID=nosub), .(CONTNAM, STPACE, PID), on="PID"], on="PID"]
tab <- plants[, .(PID=unique(PID), Count=.N), by=c("CONTNAM", "STPACE", "YEAR")]

datatable(tab, options=dtopts, caption="Counts of plants in plots/years where missing substrate.")
## /* end missing-substrate */
##'
##'

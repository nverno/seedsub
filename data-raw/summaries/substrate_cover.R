## /* substrate_cover.R --- 
##' Filename: substrate_cover.R
##' Description: Compute percent covers for substrate data
##' Author: Noah Peart
##' Created: Thu Feb 18 10:36:36 2016 (-0500)
##' Last-Updated: Fri Feb 19 04:15:28 2016 (-0500)
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
opts_chunk$set(fig.path='Figures/', echo=FALSE, message=FALSE, cache=FALSE)

library(data.table)
library(stringi)
library(DT)
library(d3heatmap)
dtopts <- list(scrollX=TRUE)

## Load data
load("../temp/segsub.rda")
load("../temp/segplants.rda")
load("../temp/segplots.rda")
load("../temp/segdata.rda")

## /* end setup */
##'
##' # Data
##'
##+raw-data
datatable(segsub, options=dtopts, caption="Raw data")
## /* end raw-data */
##'
##' # Difficulties
##'
##'   + Protocols differ by year - different substrates sampled
##'   + Don't have substrate data for all the segments
##'   + Don't have substrate data collected the same year as seedling data (except 1999).
##' 
##' # Substrates
##' 
##' ## Collected for seedlings
##'
##' The substrate types collected for seedlings varied between census years.  An overview
##' of the substrates/year is as follows:
##+seed-subs, echo=FALSE

seedsubs <- segplants[!is.na(HT), 
  .(Substrates = list(na.omit(unique(SUB)))), by=YEAR][order(YEAR)]
datatable(seedsubs, caption="Substrates collected for seedlings by year")

## /* end seed-subs */
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
##' # Substrate groupings
##'
##+sub-groups
nonsubs <- c("PID", "CONTNAM", "STPACE", "YEAR", "SGDSP", "QPOS", "SUMA", "SUMG",
  "CORRECT", "ASPCL", "ELEVCL", "SEASON", "DATE")
ground <- c("BLA5", "BLD5", "BSOIL", "RCK", "WATER", "WDG1", "WDG5", "MSSG", "LITT", "LITC",
  "LITD", "LITM", "TIPA", "STPA", "LITCRCK", "LITMRCK", "MSRCK", "MSBLA5", "MSWDG5")
aerial <- c("WDA1", "WDA5", "MSWDA1", "MSWDA5")

## Check they all got listed
stopifnot(length(setdiff(names(segsub), c(nonsubs, ground, aerial))) == 0L)
stopifnot(length(setdiff(c(nonsubs, ground, aerial), names(segsub))) == 0L)

## Ground substrate by year
ground_yr <- segsub[, .(Substrates = list(ground[
  sapply(ground, function(x) if (length(na.omit(get(x)))) TRUE else FALSE)
])), by=YEAR]

## Remove `LITT` from years where other litter types were collected (1999 / 2003)
ground_yr[, Substrates := lapply(Substrates, function(x) {
  if ("LITC" %in% x) setdiff(x, "LITT") else x
})]
## /* end sub-groups */
##'
##' ## Ground substrate
##' 
##' Substrates collected on the ground by year:
##+ground-subs
datatable(ground_yr, options=dtopts, caption="Ground substrates/year.")
## /* end ground-subs */
##'
##' ## Aerial substrate
##'
##' Aerial substrates collected by year:
##+aerial-sub
aerial_yr <- segsub[, .(Substrates = list(aerial[
  sapply(aerial, function(x) if (length(na.omit(get(x)))) TRUE else FALSE)
])), by=YEAR]
datatable(aerial_yr, options=dtopts, caption="Aerial substrates/year.")
## /* end aerial-sub */
##'

##'
##' # Totals
##'
##' ## Corrections
##'
##' Some corrections were already made by Lixi.  In the docs, it says that `LITT` was 
##' adjusted so that the `SUMG` column would be 100 if the calculated `SUMG` was <95 or >105.
##' In these cases a value of "1" is indicated in the `CORRECT` column.  This checks out, 
##' but there is also a value of "2" in the `CORRECT` column for 6 rows and this is 
##' undocumented.
##'
##' It looks like it would be possible to retrieve the initial `LITT` values by using
##' the difference between the `SUMG` and `LITT` variables?
##' 
##' ## Total ground cover
##'
##' This should align with `SUMG` according to the docs.  It is simply the sum of all
##' the non-aerial substrates for each row in the dataset.  Except, since `LITT` is already
##' the sum of the `LITC`, `LITM`, and `LITD` columns, these should be ignored when 
##' calculating the sums.
##'
##+total-ground
## Total sum, combining all the litters into one
segsub[, GSUM := rowSums(.SD, na.rm=TRUE), 
  .SDcols = setdiff(ground, c("LITC", "LITM", "LITD"))]

## Separate the litters
for (yr in unique(ground_yr$YEAR)) {
  segsub[YEAR==yr, GSUM2 := rowSums(.SD, na.rm=TRUE),
    .SDcols = unlist(ground_yr[YEAR == yr, Substrates])]
}

## /* end total-ground */
##'

##'
##' # Calculate Percents
##'
##' Calculate percent cover for each substrate type by year.
##'
##+percent-cover

res <- copy(segsub)

for (yr in unique(ground_yr$YEAR)) {
  res[YEAR == yr, ]
}

## /* end percent-cover */

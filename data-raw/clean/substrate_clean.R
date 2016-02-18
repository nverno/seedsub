## /* substrate_clean.R --- 
## Filename: substrate_clean.R
## Description: Cleaning submas99c
## Author: Noah Peart
## Created: Sat Feb  6 21:53:42 2016 (-0500)
## Last-Updated: Thu Feb 18 11:31:44 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Substrate Cleaning"
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

## Load the master file
library(seedsub.mas)
segsub <- copy(submas99c)

## /* end setup */
##'
##' # Overview
##'
##' The goal of this script is to clean the substrate data summarised in 
##' [substrate_summary.R](../summaries/substrate_summary.R).  There isn't much to do,
##' basically just:
##'
##'   + change the `CENS` name to `YEAR`
##'   + rename the dataset to `segsub`
##'   + convert some of the variable types to integers and the `DATE` column to date type.
##'   + Add a `PID` variable to link to `segplots`
##'
##' # Clean
##+clean, echo=FALSE

## Change year name
setnames(segsub, "CENS", "YEAR")

## Convert some variables to integers
intCols <- c("YEAR", "STPACE", "QPOS", "CORRECT")
segsub[, intCols := lapply(.SD, as.integer), .SDcols=intCols, with=FALSE]

## Convert date column
## Form: "2003-07-10", no special conversion necessary
segsub[, DATE := as.Date(DATE)]

## Create PID
load("../temp/segplots.rda")
segsub <- segplots[, .(CONTNAM, STPACE, PID)][segsub, on=c("CONTNAM", "STPACE")]

## Order columns
ord <- c("PID", "CONTNAM", "STPACE", "YEAR")
rest <- setdiff(names(segsub), ord)
setcolorder(segsub, c(ord, rest))

## /* end clean */
##'
##' # Save
##' 
##' Saving as `segsub.rda`, with bzip2 compression.
##'
##+save, echo=FALSE

## If saving 
## save(segsub, file="../temp/segsub.rda", compress="bzip2")

## /* end save */
##'




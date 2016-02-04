## /* cseed_summary.R --- 
## Filename: cseed_summary.R
## Description: 
## Author: Noah Peart
## Created: Wed Feb  3 22:46:27 2016 (-0500)
## Last-Updated: Thu Feb  4 01:18:20 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Some Basic Summaries for the __cseed__ Dataset"
##' author: "Noah Peart"
##' date: "`r Sys.Date()`"
##' output_format: 
##'   html_document:
##'     toc: true
##' ---
##'
##+setup, include=FALSE, echo=FALSE
## /* knitr setup */
library(knitr)
opts_chunk$set(fig.path='figures/', cache=FALSE, echo=TRUE, message=FALSE)
## /* end setup */

##+load-stuff
library(data.table)
library(DT)
library(igraph)
source('utils.R')             # prettify, see
load('temp/cseed.rda')        # data
dat <- copy(cseed)
dtopts <- list(scrollX=TRUE)  # DT options
## /* end load-stuff */

##' This document contains some basic summaries of the dataset 'cseed' 
##' created from the seesapmas11 master file.  It will attempt to present some 
##' helpful summary figures/tables pertaining to seedlings/saplings and substrates
##' sampled around the contours, as well as experimenting with some tree-like
##' visualizations of the relationships between the two levels of substrate.
##' 
##' ----------------------
##'
##' ## Sample Counts
##' Here counts are determined by the presence of a variable (ie. it wasn't missing from
##' the dataset).
##'
##' Seedlings are designated as plants (< 100cm) while saplings are (>= 100cm).
##' Substrate is the substrate seedlings were sampled on, while the secondary level
##' of substrate is hereafter referred to as 'subon'.
##'
##' Table 1 shows counts of seedlings, saplings, substrate, and subon.  Table 2 further
##' breaks the counts of these variables by plot.  A plot along the contours is 
##' determined by the unique combinations of `CONTNAM` and `STPACE`.
##+year-count
## Summary statistics by year of some variables
yrstats <- dat[, .(
  `Seedling`=sum(HT < 100, na.rm=TRUE), 
  `Sapling`=sum(HT >= 100, na.rm=TRUE),
  `Substrate`=sum(!is.na(SUB)),
  `Sub_On`=sum(!is.na(SUBON))
), by=YEAR][order(YEAR, Seedling, Substrate, Sub_On, Sapling)]
datatable(yrstats, options=dtopts, caption='Table 1: Samples by YEAR.')

## Summary by YEAR/CONTNAM/STPACE
pltstats <- dat[, .(
  `Count(hts)`=sum(!is.na(HT)),
  `Seedling`=sum(HT < 100, na.rm=TRUE), 
  `Sapling`=sum(HT >= 100, na.rm=TRUE),
  `Substrate`=sum(!is.na(SUB)),
  `Sub_On`=sum(!is.na(SUBON))
), by=c('YEAR', 'CONTNAM', 'STPACE')][order(YEAR, CONTNAM, STPACE)]
datatable(pltstats, options=dtopts, caption='Table 2: Samples by YEAR/Plot.')

## Substrate values
subs <- sort(dat[!is.na(SUB), unique(SUB)])
subons <- sort(dat[!is.na(SUBON), unique(SUBON)])
## /* end year-count */
##'

##' ## Substrate Types
##'
##' Here, the various substrates and subons are tabulated further.  In the 'cseed'
##' dataset the unique values of substrate and subon are:
##' 
##' + __Substrate(`SUB`)__:
{{prettify(subs)}}
##' + __Substrate ON(`SUBON`)__:
{{prettify(subons)}}
##'
##' Some summary tables: Table 3 shows the total counts (ie. where there was a 
##' height measurement) for substrates and subons.  
##' Table 4 further breaks down the counts of substrate
##' types by years.
##+sub-tables
## Total substrate counts
substats <- dat[, .(Count=sum(!is.na(HT))), by=c('SUB','SUBON')][
  !(is.na(SUB) & is.na(SUBON))][order(-Count)]
datatable(substats, options=dtopts, caption='Table 3: Total substrate counts.')

## Counts by year
subyrs <- dat[, .(Count=sum(!is.na(HT))), by=c('YEAR','SUB','SUBON')][
  !(is.na(SUB) & is.na(SUBON)),][order(YEAR, -Count, SUB, SUBON, na.last=TRUE)]
datatable(subyrs, options=dtopts, 
  caption='Table 4: Substrate counts by year and type.')
## /* end sub-tables */

##'
##' -------------------------------------
##' 
##' ## Substrate and Substrate On
##'
##' Further breaking down the relationship between substrate and subon.
##+sub-subon
library(igraph)
library(DiagrammeR)
grViz('substrates.dot')
## sg1 <- graph_from_data_frame(substats)
## sg1 <- sg1 - V(sg1)[vertex_attr(sg1)$name=='NA']
## sg1 <- sg1 + vertices(c('Substrate', ''))
## /* end sub-subon */
##'

## /* csub_clean.R --- 
## Filename: csub_clean.R
## Description: Cleaning submas99c
## Author: Noah Peart
## Created: Sat Feb  6 21:53:42 2016 (-0500)
## Last-Updated: Mon Feb  8 17:36:48 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Substrate Cleaning"
##' output_format: 
##'   html_document:
##'     toc: true
##' ---
##' 
#+setup, echo=FALSE, include=FALSE
library(knitr)
opts_chunk$set(fig.path='Figures/', echo=TRUE, message=FALSE, cache=FALSE)

library(data.table)
library(DT)
library(visNetwork)
library(stringi)
source('utils.R')

## Load the master file
library(seedsub.mas)
csub <- copy(submas99c)

## /* end setup */
##'
##' The goal of this script is to clean and explore (minorly) the substrate master
##' named `submas99c`.  This file can be loaded from `library(seedsub.mas)`.
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



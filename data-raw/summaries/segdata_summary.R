## /* segdata_summary.R --- 
## Filename: segdata_summary.R
## Description: Summarise the segdata output from segments_clean.R
## Author: Noah Peart
## Created: Wed Feb  3 22:46:27 2016 (-0500)
## Last-Updated: Fri Feb 19 21:23:33 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Some Basic Summaries for the __segdata__ Dataset"
##' ---
##'
##+setup, include=FALSE
## /* knitr setup */
library(knitr)
opts_chunk$set(fig.path='figures/', cache=FALSE, echo=TRUE, message=FALSE)

library(data.table)
library(DT)
library(igraph)
library(networkD3)
library(treemap)

source('../clean/utils.R')                   # prettify, see
load('../temp/segdata.rda')                  # data
dat <- copy(segdata)
dtopts <- list(scrollX=TRUE)                 # DT options
## /* end load-stuff */

##' This document contains some basic summaries of the dataset 'segdata' 
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
##' Further breaking down the relationship between substrate and what it is found on.
##'
##' ### Flowchart
##' First, a basic flow chart with arrows pointing from overlying substrate to underlying
##' substrate.  The boxes are colored blue if that substrate is found as an overlying
##' substrate, and red if they are only found as underlying (ie. `BLA5`).
##'
##' ![alt text](figures/substrates.png)
##+sub-flow
## Make a graphviz flowchart using dot.exe
## if (nzchar(Sys.which('dot'))) {}

## Or using DiagrammeR, but very slow and not portable
## library(DiagrammeR)
## grViz('substrates.dot')
## /* end sub-flow */

##'
##' ------------------
##'
##' ### Sankey Diagram
##'
##' This takes a riverplot-like approach to visualizing this sort of 
##' hierarchical data.  I find this to be especially useful here.  The diagram
##' is interactive (javascript), so hovering over a component will display the 
##' counts of seedlings along that pathway.  Notice that `BSOIL` is unique in that 
##' it is the only substrate that is a `SUB`, a direct descendant of the other 
##' four main `SUB`'s (`MSS`, `LITM`, `LITC`, and `LITD`), and a `SUBON` (along
##' the pathway from `MSS -> BSOIL`.
##'
##+sub-sankey1

################################################################################
##
##                     A Sankey for YEAR -> SUB -> SUBON
##
################################################################################
## Need to get data in format for sankey
yrs <- dat[!is.na(HT) & !is.na(SUB), .(Count=.N), by=YEAR]
subs <- dat[!is.na(HT) & !is.na(SUB), .(Count=.N), by=c('YEAR', 'SUB')]
subons <- dat[!is.na(HT) & !is.na(SUB) & !is.na(SUBON),
  .(Count=.N), by=c('YEAR', 'SUB', 'SUBON')]

## Get names for each level, to make unique key, paste priors
nms <- c(
  as.character(yrs[, YEAR]), 
  subs[, paste0(YEAR, SUB)], 
  subons[, paste0(YEAR, SUB, SUBON)]
)
nums <- setNames(seq.int(length(nms))-1L, nms)
  
## Nodes are names data.frame, fine if they repeat (they should in fact)
nodes <- data.frame(
  name=c(as.character(yrs[,YEAR]), subs[, SUB], subons[, SUBON]),
  stringsAsFactors = FALSE
)

## Links, care here for names
srcs <- c(
  nums[as.character(subs[, YEAR])],         # source: YEAR -> SUB
  nums[subons[, paste0(YEAR, SUB)]]         # source: SUB -> SUBON
)
targs <- c(
  nums[subs[, paste0(YEAR, SUB)]],          # target: YEAR -> SUB
  nums[subons[, paste0(YEAR, SUB, SUBON)]]  # target: YEAR -> SUB -> SUBON
)
links <- data.frame(
  source = srcs,
  target = targs,
  value  = c(subs[, Count], subons[, Count]),
  stringsAsFactors=FALSE
)

## Sankey, each year with a separate flow
sankeyNetwork(Links=links, Nodes=nodes, Source='source', Target='target',
  Value='value', NodeID='name', units='Seedlings', fontSize=20, nodeWidth=30)

## Trim down the nodes to only those in links
vals <- unique(unlist(links[1:2]))  # these nodes are either source/target
nodes[!seq.int(nrow(nodes)) %in% (vals+1L), 'name'] <- NA_character_

## contour sample counts
conts <- dat[!is.na(HT) & !is.na(SUB), .(Total=.N), by=CONTNAM]

## Look along most sampled contour
cont <- dat[!is.na(HT) & !is.na(SUB), .N, by=CONTNAM][which.max(N), CONTNAM]

## /* end sub-sankey1 */
##'
##' We can also look at how the substrates were samples in different plots.  For example,
##' examining the most sampled contour,
{{prettify(cont)}}
##' , to look at the relationship between `STPACE` -> `SUB` -> `SUBON`
##'
##+sub-sankey2
dt <- dat[CONTNAM == cont & !is.na(HT) & !is.na(SUB), ]
pace <- dt[, .(Count = .N), by=STPACE]
subs <- dt[, .(Count = .N), by=c('STPACE', 'SUB')]
subons <- dt[, .(Count = .N), by=c('STPACE', 'SUB', 'SUBON')][!is.na(SUBON)]

## Make the nodes
nodes <- data.frame(
  name=c(as.character(pace[, STPACE]), subs[, SUB], subons[, SUBON]),
  stringsAsFactors=FALSE
)
nums <- setNames(0:(nrow(nodes)-1L), nodes$name)

## Links
links <- data.frame(
  source=c(nums[as.character(subs[, STPACE])], nums[subons[, SUB]]),
  target=c(nums[subs[,SUB]], nums[subons[,SUBON]]),
  value=c(subs[,Count], subons[,Count]),
  stringsAsFactors=FALSE
)

##'
##' ### Treemaps
##' These treemaps show the hierarchy of relationship between `SUB` and `SUBON` 
##' visually using color and size.  First, the boxes are colored by aggregating variables.
##'
##' In the following graphics, those variables are `YEAR`, `SUB`, and `SUBON`.  So, an attempt
##' is made to color each grouping, with successive groupings being shaded appropriately.
##' Additionally, the boxes are sized by the counts of each grouping.
##' 
##+sub-treemaps
suppressWarnings(library(treemap))
treemap(substats, index=c('SUB', 'SUBON'), vSize='Count',
  fontsize.labels=16, fontsize.title=16,
  title='Size: Counts of subtrate/substrate ON')

tr <- treemap(subyrs, index=c('YEAR', 'SUB', 'SUBON'), vSize='Count',
  title='Size: #YEAR/SUB/SUBON, Color: YEAR/SUB/SUBON',
  fontsize.labels = 16, fontsize.title = 16)
## /* end sub-treemaps */
##'
##' ### Interactive Treemap
##' Hovering with the mouse over the rectangles should show the counts.  Clicking
##' on a rectangle will expand that level of aggregation, and clicking the topbar
##' allows you to go back up a level.
##'
##+sub-tree-interactive
suppressPackageStartupMessages(library(d3treeR))
d3tree3(tr, rootname = 'Click to return to overview', width="75%")
## /* end sub-tree-interactive */


##'
##' -------------------
##'
##' ### Graphs
##+sub-graphs
library(igraph)
## library(DiagrammeR)
## grViz('substrates.dot')

## Make a graph of substats, node attributes will be counts
## agg1 <- substats[, .(Count = sum(Count)), by=SUB]
## g <- graph.empty() + vertices(1:nrow(agg1), label=agg1[, SUB], 
##   level=1, size=agg1[,Count], label.cex=1.1, label.color='dark blue',
##   frame.color='grey20', color='light grey')

## g1 <- graph()
## sg1 <- graph_from_data_frame(substats)
## sg1 <- sg1 - V(sg1)[vertex_attr(sg1)$name=='NA']
## sg1 <- sg1 + vertices(c('Substrate', ''))

## /* end sub-subon */
##'

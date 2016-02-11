### sankey_test.R --- 
## Filename: sankey_test.R
## Description: 
## Author: Noah Peart
## Created: Sat Feb  6 20:50:04 2016 (-0500)
## Last-Updated: Sat Feb  6 20:50:07 2016 (-0500)
##           By: Noah Peart
######################################################################
library(networkD3)
library(data.table)
set.seed(1999)
links <- data.table(
  src = rep(0:4, times=c(1,1,2,3,5)),
  target = sample(1:11, 12, TRUE),
  value = sample(100, 12)
)[src < target, ]  # no loops
nodes <- data.table(name=LETTERS[1:12])

## Need to hover to get counts
sankeyNetwork(Links=links, Nodes=nodes, Source='src', Target='target',
  Value='value', NodeID='name', fontSize=16)

## Add text to label
txt <- links[, .(total = sum(value)), by=c('target')]
nodes[txt$target+1L, name := paste0(name, ' (', txt$total, ')')]

## Displays the counts as part of the labels
sankeyNetwork(Links=links, Nodes=nodes, Source='src', Target='target',
  Value='value', NodeID='name', fontSize=16, width=600, height=300)


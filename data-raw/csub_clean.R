### csub_clean.R --- 
## Filename: csub_clean.R
## Description: Cleaning submas99c
## Author: Noah Peart
## Created: Sat Feb  6 21:53:42 2016 (-0500)
## Last-Updated: Sat Feb  6 21:54:05 2016 (-0500)
##           By: Noah Peart
######################################################################
load('cseed.rda')

library(seedsub.mas)
sub <- copy(submas99c)
seed <- copy(cseed)

setnames(sub, 'CENS', 'YEAR')

table(seed$YEAR, seed$SUB)
seed[, ]
unique(seed[sub, on=c('CONTNAM', 'STPACE', 'YEAR'), .(SUB)])


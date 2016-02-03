### cseed.R --- 
## Filename: cseed.R
## Description: Cleaning contour seedling data, seesapmas11, to make cseed
## Author: Noah Peart
## Created: Tue Feb  2 17:07:06 2016 (-0500)
## Last-Updated: Wed Feb  3 01:45:28 2016 (-0500)
##           By: Noah Peart
######################################################################
## Notes:
## - Documentation: segment data history 1988-2012
## - TAG should be character to avoid floating point issues
## - unique CONTNAM/STPACE designates a plot
## - 1988/1989: 5 PIRU/ABBA measured in each plot and tagged 1:5
## - Locations of plants defined by ALONG/DISUPDN (distance along/distance up/down)

## Things to clean:
## - Duplicated tags from 1989 for seedlings and saplings
## - create unique IDs taking into account the duplicates
## - Should be unique IDs taking into account CONTNAM/STPACE/SPEC
## - A few rows missing SPEC
## - A few rows missing ALONG (a few overlap with those missing SPEC)
## - All heights in cm (check from docs)
## - Separate extension growth into a new dataset
##   - Extenstion growths:
##     - in mm or cm (check)

## Seedling notes:
## - defined under 1m tall (arbitrary here, for analyses)
## - Only seedlings have substrate measurements

## Substrate:
## These procedures are very convoluted and year specific.
## For the current analysis:
## - Only used 1999 data (the most extensive type system)
## - All `ON` variables were ignored
## - Broad types: moss, mixed litter, coniferous litter, deciduous litter
library(data.table)
library(stringi)
library(DT)
dtopts <- list(scrollX=TRUE)

## Master data
library(seedsub.mas)
cseed <- copy(seesapmas11)

## Plot keys:
ckeys <- c('CONTNAM', 'STPACE')

## Convert TAG to character
cseed[, TAG := as.character(TAG)]

## Some variables of interest
hts <- grep('^HT[0-9]+', names(cseed), value=TRUE)  # height columns
ids <- c('TAG', 'SPEC')                             # individual identifies
locs <- c('ALONG', 'DISUPDN')                       # location variables
cols <- c(ckeys, ids, 'YRTAG', locs, hts)           # general cols for tables

################################################################################
##
##                                  Heights
##
################################################################################
## Check heights: types, NAs, missing rows
cseed[, lapply(.SD, typeof), .SDcols=hts]                       # doubles
cseed[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = hts]  # NAs

## Rows where there was no height measurement?
nohts <- cseed[, Reduce('&', lapply(.SD, is.na)), .SDcols=hts]
nohts <- which(nohts)                                           # 11 with no height measurements
################################################################################
##
##                                Duplicates
##
################################################################################
## Check unique tags CONTNAM/STPACE/SPEC
## There are duplicates when not excluding 1988 and 1989
## Notes:
## - duplicates in 1989 were b/c 3 seedlings and 3 saplings
##   of both ABBA and PIRU were sampled in each segment (actually 4 in a couple).
##   These seedlings and saplings were both labelled with tags 1:3 (or 4).
##   However, they are readily separated since only seedlings have substrate measurements
##   and in general the heights will be vastly different (unless a sapling was 
##   totally leaned over).
dupes <- cseed[, .(TAG=TAG[duplicated(TAG)]), keyby=c(ckeys, 'SPEC')]
dupes2 <- cseed[!(YRTAG %in% c(1988, 1989)), TAG[duplicated(TAG)], 
  by=c(ckeys, 'SPEC')]  # no dupes

## Examine these duplicates: 200 of them (100 repeated once each) in 1989
## All have heights, each tag seems to have a height on two scales
dupe1 <- cseed[dupes, on=c(ckeys, 'SPEC', 'TAG')][, 
  NUM_DUPES := .N, keyby=c(ckeys, 'SPEC', 'TAG')][]
dupeyrs <- table(dupe1$YRTAG)  # 200 in 1989

## Table showing these dupes only had heights measured in 1989
dupehts <- dupe1[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=hts]

## Visualize the duplicates in table
dcols <- c(ckeys, 'SPEC', 'TAG', 'HT89', 'NUM_DUPES')             # columns to include
datatable(dupe1[, c(dcols, 'CLASS'), with=FALSE], options=dtopts)
dupe1dt <- datatable(dupe1[, dcols, with=FALSE], options=dtopts)  # indiv. dupes

## Every plot measured in 1989 (25 of them) followed same procedure
## of id'ing the saplings and seedlings with same tag numbers.
cdupes <- dupe1[, .(TOTAL_DUPES=.N), by=ckeys]
dupe2dt <- datatable(cdupes, options=dtopts)                      # number of dupes/plot

## Assign unique IDs, since duplicate TAGs were actually different
## plants in 1989 this is no problem
cseed[, ID := 1:.N]

################################################################################
##
##                             Extension Growth
##
################################################################################
## Pull out extension growth data
## - keys: ID
## Extension growth columns:
patts <- c(
  '^EX[0-9]+',       # extension growth
  '^PEX[0-9]+',      # partial? extension
  '^MINAGE[0-9]+',   # minimum age
  '^ENOTE[0-9]+',    # notes on ext. growths
  '^EXCNT[0-9]+',    # number of ext. measured
  '^AGE[0-9]+',      # Total age for 1988 seedlings
  '^D[0-9]RM[0-9]+', # distance remaining after last countable extension
  '^EXSUM[0-9]+',    # summed extension growth
  'YRREX',           # last measureable year from ext. growths
  '^NL[0-9]+_[0-9]', # Derived from ENOTE
  '^TERM[0-9]+'      # Terminal leader condition
)
patt <- paste(patts, collapse='|')
ecols <- grep(patt, names(cseed), value=TRUE)
ecols <- c('ID', ecols)

## Extract columns
cextgr <- cseed[, ecols, with=FALSE]

################################################################################
##
##                             Remove columns
##
################################################################################
## Remove ext. growth columns (save the ID)
cseed[, setdiff(ecols, 'ID') := NULL, with=FALSE]

## SEG88 is redundant, YRTAG == 88 is the same
cseed[, SEG88 := NULL]

## Don't know what CODE98 is, CLASS is all 'S'
cseed[, CODE98 := NULL]
cseed[, CLASS := NULL]

## Soil columns: keep these here for now?
soilcols <- c(
  'SOILCL[0-9]?',  # soil classes
  'HISTO',
  'SPODO',
  'INCEP',
  'MINER',
  'ROCK',
  'PHISTO',
  'SOILCL2'
)

## Separate SGLEN variables
sampyrs <- cseed[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=hts, by=ckeys]
sglens <- grep('SGLEN', names(cseed), value=TRUE)
cseed[, paste0('SGLEN', c('88', '89')) := SGLEN8889]
cseed[, paste0('SGLEN', c('98', '99')) := SGLEN9899]
cseed[, sglens := NULL, with=FALSE]  # remove old ones

################################################################################
##
##                                  Melting
##
################################################################################
## Gonna do a bunch of melting separately and join back together
## since the years are different for a bunch of variables
ids <- c('ID', 'CONTNAM', 'STPACE')

## SGLENs
sglen <- melt(cseed[, c(ids, grep('SGLEN', names(cseed), value=TRUE)), with=FALSE],
  id.vars=ids, na.rm=TRUE, value.name='SGLEN', measure=patterns('^SGLEN[0-9]+'),
  variable.name='YEAR')
sglen[, YEAR := stri_extract(YEAR, regex='[0-9]+')]

## HTs
ht <- melt(cseed[, c(ids, hts), with=FALSE],
  id.vars=ids, na.rm=TRUE, value.name='HT', measure.vars=hts, 
  variable.name='YEAR')
ht[, YEAR := stri_extract(YEAR, regex='[0-9]+')]

## STATs: set empty string to NA
stats <- grep('STAT[0-9]+', names(cseed), value=TRUE)
stat <- melt(cseed[, c(ids, stats), with=FALSE],
  id.vars=ids, na.rm=TRUE, value.name='STAT', measure.vars=stats, 
  variable.name='YEAR')
stat[, YEAR := stri_extract(YEAR, regex='[0-9]+')]

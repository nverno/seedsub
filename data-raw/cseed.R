## /* cseed.R --- 
## Filename: cseed.R
## Description: Cleaning contour seedling data, seesapmas11, to make cseed
## Author: Noah Peart
## Created: Tue Feb  2 17:07:06 2016 (-0500)
## Last-Updated: Fri Feb  5 16:19:11 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Cleaning seesapmas11"
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
opts_chunk$set(fig.path='figures/', cache=FALSE, echo=TRUE, message=FALSE) # echo=FALSE
## /* end setup */

##' The goal is to separate out extension growth files, and convert 
##' the resulting files to long format, excluding uneccesary columns as well as
##' checking the data for some details.  Note that there is a fair amount of extra
##' code to make summary figures/checking that could be removed.
##'
##' __Notes__:
##'
##'  + More documentation in _Segment Data History 1988-2012.docx_
##'  + `TAG` should be character to avoid floating point issues
##'  + unique `CONTNAM`/`STPACE` designates a plot
##'  + 1988/1989: 5 PIRU/ABBA measured in each plot and tagged 1:5
##'  + Locations of plants defined by `ALONG`/`DISUPDN` (distance along/distance up/down)
##'
##' __Things to clean__:
##'
##'  + Set empty strings to NA
##'  + Duplicated tags from 1989 for seedlings and saplings
##'  + create unique IDs taking into account the duplicates
##'  + Should be unique IDs taking into account `CONTNAM`/`STPACE`/`SPEC`
##'  + A few rows missing `SPEC`
##'  + A few rows missing `ALONG` (a few overlap with those missing `SPEC`)
##'  + All heights in cm (check from docs)
##'  + Separate extension growth into a new dataset:
##'     * Extenstion growths:
##'         * in mm or cm (check)
##'
##' __Seedling notes__:
##'
##'  + defined under 1m tall (arbitrary here, for analyses)
##'  + Only seedlings have substrate measurements
##'
##' __Substrate__:
##'
##' These procedures are very convoluted and year specific.
##' For the current analysis:
##'
##'   + Only used 1999 data (the most extensive type system)
##'   + All `ON` variables were ignored
##'   + Broad types: moss, mixed litter, coniferous litter, deciduous litter
##'
##+load-data
library(data.table)
library(stringi)
library(DT)
library(seedsub.mas)  # storing master data in this package
source('utils.R')

dtopts <- list(scrollX=TRUE)
cseed <- copy(seesapmas11)

## Plot keys:
ckeys <- c('CONTNAM', 'STPACE')
cseed[, TAG := as.character(TAG)]  # TAG as char

## empty strings to NA
chars <- names(cseed)[vapply(cseed, is.character, logical(1))]
cseed[, chars := lapply(.SD, function(x) { x[x==''] <- NA_character_; x}),
  .SDcols=chars, with=FALSE]

## Some variables of interest
hts <- grep('^HT[0-9]+', names(cseed), value=TRUE)  # height columns
ids <- c('TAG', 'SPEC')                             # individual identifies
locs <- c('ALONG', 'DISUPDN')                       # location variables
cols <- c(ckeys, ids, 'YRTAG', locs, hts)           # general cols for tables
## /* end load-data */
##'

##' ------------
##'
##' ## Heights
##' Check heights: 
##' - types
##' - NAs
##' - missing rows
##+heights1
types <- cseed[, lapply(.SD, typeof), .SDcols=hts]                   
nans <- cseed[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = hts]  # NAs
tab <- rbind(types, nans)[, TYPE := c('Type', 'NA count')][]
setcolorder(tab, c('TYPE', names(types)))
knitr::kable(tab, caption='Height column types and counts.')

## Rows where there was no height measurement?
nohts <- cseed[, Reduce('&', lapply(.SD, is.na)), .SDcols=hts]
nohts <- which(nohts)                                           # 11 with no height measurements
## /* end heights check */
##'

##' Rows with missing heights (11):
{{prettify(nohts, type='all')}}
##' 
##' ----------------------
##'
##' ## Duplicates
##'
##' Check unique tags `CONTNAM`/`STPACE`/`SPEC`
##' There are duplicates when not excluding 1988 and 1989
##' Notes:
##' - duplicates in 1989 were b/c 3 seedlings and 3 saplings
##'   of both ABBA and PIRU were sampled in each segment (actually 4 in a couple).
##'   These seedlings and saplings were both labelled with tags 1:3 (or 4).
##'   However, they are readily separated since only seedlings have substrate measurements
##'   and in general the heights will be vastly different (unless a sapling was 
##'   totally leaned over).
##+dupe-setup
dupes <- cseed[, .(TAG=TAG[duplicated(TAG)]), keyby=c(ckeys, 'SPEC')]
dupes2 <- cseed[!(YRTAG %in% c(1988, 1989)), TAG[duplicated(TAG)], 
  by=c(ckeys, 'SPEC')]  # no dupes when excluding 1989

## Examine these duplicates: 200 of them (100 repeated once each) in 1989
## All have heights, each tag seems to have a height on two scales
dupe1 <- cseed[dupes, on=c(ckeys, 'SPEC', 'TAG')][, 
  NUM_DUPES := .N, keyby=c(ckeys, 'SPEC', 'TAG')][]
dupeyrs <- table(dupe1$YRTAG)  # 200 in 1989

## Table showing these dupes only had heights measured in 1989
dupehts <- dupe1[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=hts]
## /* end dupe-setup */

##' ### Visualize the duplicates in table
##' Every plot measured in 1989 (25 of them) followed same procedure
##' of id'ing the saplings and seedlings with same tag numbers.
##+dupe-table
dcols <- c(ckeys, 'SPEC', 'TAG', 'HT89', 'NUM_DUPES')             # columns to include
## datatable(dupe1[, c(dcols, 'CLASS'), with=FALSE], options=dtopts) 
dupe1dt <- datatable(dupe1[, dcols, with=FALSE], options=dtopts)  # indiv. dupes
dupe1dt
## /* end dupe-table */
##' 

##' Numbers of duplicated tags by `CONTNAM`/`STPACE` (ie. plot).
##+dupe2-table
cdupes <- dupe1[, .(TOTAL_DUPES=.N), by=ckeys]
dupe2dt <- datatable(cdupes, options=dtopts)    # number of dupes/plot
dupe2dt
## /* end dupe2-table */
##' 

##+unique-ids
## Assign unique IDs, since duplicate TAGs were actually different
## plants in 1989 this is no problem
cseed[, ID := 1:.N]
## /* end unique-ids */
##'
##' ----------------------
##' 
##' ## Extension Growth
##'
##' Pull out extension growth data:
##' - key by `ID`
##'
##+extension
## matching patterns
patts <- c(
  '^EX[0-9]+',       # extension growth
  '^PEX[0-9]+',      # partial extension (in the yr it was sampled)
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

## Extract columns into ext. growth dataset
cextgr <- cseed[, ecols, with=FALSE]

## Save raw ext. growth for later: needs to be melted
## if (!file.exists('temp')) dir.create('temp')
## save(cextgr, file='temp/cextgr.rda', compress='bzip2')
## /* end extension */

##'
##' The following columns will be removed from `seesapmas11` and used in the new
##' extension growth dataset.
##'
##' **Extension growth columns**:
{{prettify(ecols)}}
##'
##'
##' For actually, cleaning of the ext. growth data see [cextgr_clean.R](cextgr_clean.R).  
##' 
##' --------------------------------
##'
##' ## Remove columns
##' - Remove ext. growth columns (save the `ID`)
##' - `SEG88` is redundant, `YRTAG == 88` is the same information.
##' - `CLASS` is all 'S' (segment) so removing.
##' - Removing unknown columns: `CODE98`, `FLAG`
##'
##+remove-cols
cseed[, setdiff(ecols, 'ID') := NULL, with=FALSE]
others <- c('SEG88', 'CLASS', 'CODE98', 'FLAG')
cseed[, others := NULL, with=FALSE]
## /* end remove-cols */
##'

##' ## Soil columns
##' Keeping these in for now...
##+soil-cols
soilcols <- c(
  'SOILCL[0-9]?',  # soil classes, two soilcl columns...
  'HISTO',
  'SPODO',
  'INCEP',
  'MINER',
  'ROCK',
  'PHISTO'
)
scols <- grep(paste(soilcols, collapse='|'), names(cseed), value=TRUE)
## /* end soil-cols */
##' Columns: 
{{prettify(scols)}}
##'
##' ----------------------------------
##'
##' ## Separate joined variable years
##' - `NOTES9899`
##' - `SGLEN8889`, `SGLEN9899`
##' - `SGDSP9899`
##'
##' These should be separated into individual year columns prior to melting
##+sglen-sep
sampyrs <- cseed[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=hts, by=ckeys]
sglens <- grep('SGLEN', names(cseed), value=TRUE)
cseed[, paste0('SGLEN', c('88', '89')) := SGLEN8889]
cseed[, paste0('SGLEN', c('98', '99')) := SGLEN9899]
cseed[, sglens := NULL, with=FALSE]  # remove old ones
cseed[, paste0('NOTES', c('98', '99')) := NOTES9899]
cseed[, NOTES9899 := NULL]
cseed[, paste0('SGDSP', c('98', '99')) := SGDSP9899]
cseed[, SGDSP9899 := NULL]
## /* end sglen-sep */
##'
##' --------------------------------
##'
##' ## Reshaping
##'
##' Gonna do a bunch of melting separately and join back together
##' since the years are different for all the variables and we want a single 
##' resulting `YEAR` column.
##+melt-vars
ids <- c('ID')  # melt with these ids
jids <- c(ids, 'YEAR')  # join back with these ids

## Constants
consts <- c(ids, 'CONTNAM', 'STPACE', 'SPEC', 'ALONG', 'DISUPDN', 'YRTAG', 
  'TAG', 'YRMORT', 'ASPCL', 'ELEVCL', 'HRB1', 'HRB2', 'HRB3', 'PALONG', 
  'TREE', 'SDSP1', 'DSDSP1', 'NSDSP1', 'SDSP2', 'DSDSP2', 'NSDSP2', 'CHECK', 
  scols)

## varying
sglens <- grep('SGLEN', names(cseed), value=TRUE)
hts <- grep('^HT[0-9]+', names(cseed), value=TRUE)
stats <- grep('STAT[0-9]+', names(cseed), value=TRUE)
notes <- grep('NOTES?[0-9]+', names(cseed), value=TRUE)
decms <- grep('DECM[0-9]+', names(cseed), value=TRUE)
subs <- grep('^SUB[0-9]+', names(cseed), value=TRUE)
subons <- grep('^SUBON[0-9]+', names(cseed), value=TRUE)
sgdsp <- grep('SGDSP[0-9]+', names(cseed), value=TRUE)
stmlens <- grep('STMLEN[0-9]', names(cseed), value=TRUE) # only 2011
tcvrs <- grep('TCVR[0-9]+', names(cseed), value=TRUE)    # only 1988
nfcvrs <- grep('NFCVR[0-9]+', names(cseed), value=TRUE)  # only 1988
brhts <- grep('BRHT[0-9]+', names(cseed), value=TRUE)    # only 1999
ciis <- grep('CII[0-9]+', names(cseed), value=TRUE)      # only 2000

## All year-varying variables
allvary <- c(sglens, hts, stats, notes, decms, subs, subons,
  stmlens, sgdsp, tcvrs, nfcvrs, brhts, ciis)

## Check got them all
inds <- names(cseed) %in% c(consts, allvary)
checkcols <- !length(names(cseed)[!inds])
if (!checkcols) stop(sprintf('Missed some columns: %s', 
  toString(names(cseed)[!inds])))
## /* end melt-vars */
##'
##' By __year__ the following variables are considered:
##'
##' - __Constant__: 
{{prettify(consts)}}
##' - __Varying__: 
{{prettify(allvary)}}
##'
##' The main constraint on the output is that there should be no NA values
##' for the `HT` column.
##+melting
## Melt HT first, then join each additional to it
res <- melt(cseed[, c(ids, hts), with=FALSE],
  id.vars=ids, na.rm=TRUE, value.name='HT', measure.vars=hts, 
  variable.name='YEAR')
res[, YEAR := stri_extract(YEAR, regex='[0-9]+')]

## list of varying variabls
allvary <- list(sglens, stats, notes, decms, subs, subons,
  stmlens, sgdsp, tcvrs, nfcvrs, brhts, ciis)

## For each element, melt by those variables, clean the year column,
## and join back to `ht`
for (v in allvary) {
  coln <- stri_extract(v[[1]], regex='^[A-Z]+', mode='first')
  tmp <- melt(cseed[, c(ids, v), with=FALSE],
    id.vars=ids, na.rm=TRUE, value.name=coln, measure.vars=v,
    variable.name='YEAR')
  tmp[, YEAR := stri_extract(YEAR, regex='[0-9]+')]
  res <- if (all(unique(tmp$YEAR) %in% unique(res$YEAR))) {
    tmp[res, on=c(ids, 'YEAR')]  # simple join on ID and YEAR
  } else {
    merge(res, tmp, by=c(ids, 'YEAR'), all.y=TRUE)
  }
}

## Add constants
res <- (cseed[, consts, with=FALSE][res, on=ids])

## Put some of the columns up front, soil cols at the back
coln <- c(  # these will go up front for visual convenience
  'ID', 'CONTNAM', 'STPACE', 'SPEC', 'HT', 'SUB', 'SUBON', 'YEAR', 
  'YRTAG', 'ALONG', 'DISUPDN', 'YRMORT', 'STAT', 'DECM', 'ELEVCL', 
  'ASPCL', 'TAG'
)
midcols <- sort(setdiff(names(res), c(coln, scols)))
ord <- c(coln, midcols, sort(scols))  # soil columns in the back
setcolorder(res, ord)

## Convert year column to full years (not abbreviations) and to integers
res[, YEAR := paste0(ifelse(YEAR %in% c('00', '11'), '20', '19'), YEAR)]
res[, YEAR := as.integer(YEAR)]
resDims <- dim(res)
## /* end melting */
##'

##+type-conversions
intInds <- unlist(lapply(res, function(x) {
  if (is.character(x)) return(FALSE)
  all(x == as.integer(x), na.rm=TRUE)
}))

## Convert to integers
intCols <- names(res)[intInds]
res[, intCols := lapply(.SD, as.integer), 
  .SDcols=intCols, with=FALSE]
## /* end type-conversions */
##'
##' ## Type Conversions
##' Many of the columns are being stored as doubles, but are just integer values,
##' so here some of these will be converted.
##' The following are converted to integers:
##'
##' + Columns: 
{{prettify(names(intInds))}}
##' 
##+results
dims <- dim(res)
restab <- datatable(head(res, 200), options=dtopts)

## If saving
## cseed <- res
## save(cseed, file='temp/cseed.rda', compress='bzip2')

## /* end results */
##' ----------------------
##'
##' ## Results
##'
##' The resulting dataset has
{{dims[[1]]}}
##' rows and 
{{dims[[2]]}}
##' columns:
##'
##' + Column names: 
{{prettify(names(res))}}
##'
##' See `cseed_summary.R` for some summaries of the data
##'


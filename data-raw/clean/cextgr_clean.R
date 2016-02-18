## /* cextgr_clean.R --- 
## Filename: cextgr_clean.R
## Description: Clean extension growth data
## Author: Noah Peart
## Created: Fri Feb  5 16:13:50 2016 (-0500)
## Last-Updated: Thu Feb 18 07:14:08 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Clean Extension Growth"
##' output_format: 
##'   html_document:
##'     theme: readable
##'     highlight: zenburn
##'     toc: true
##' ---
##' 
##+setup, include=FALSE, message=FALSE, echo=FALSE
library(knitr)
opts_chunk$set(cache=FALSE, echo=TRUE, message=FALSE)

library(data.table)
library(stringi)
library(DT)

## /* end setup */
##'
##+load-data, echo=FALSE
## Loading data, extract some variables/years
load('../temp/cextgr.rda')
setDT(cextgr)
if (!inherits(cextgr, "data.table"))
  cextgr <- as.data.table(cextgr)

## Describe these variables
## nms <- unique(stri_extract_first(names(cextgr), regex="[A-Z]+[0-9]?[A-Z]+"))

## Column groups
nms <- names(cextgr)
patts <- list(                        # patterns to match groups
  id      = '^ID$',
  consts  = '^YRREX$',                # won't melt these by year
  age     = '^AGE88$',                # total age, only measured in 1988
  minages = '^MINAGE[0-9]+',
  pexs    = '^PEX[0-9]+', 
  exs     = '^EX[0-9]+',
  terms   = '^TERM[0-9]+',
  d1rms   = '^D1RM[0-9]+',
  d2rms   = '^D2RM[0-9]+',
  enotes  = '^ENOTE[0-9]+',
  nl99s   = '^NL99_[1-3]',
  excnts  = '^EXCNT[0-9]+',
  exsums  = '^EXSUM[0-9]+'
)
## others  = '^EX(?:CNT|SUM)[0-9]+'    # probably gonna ignore these

allCols <- lapply(patts, grep, nms, perl=TRUE, value=TRUE)
if (!all(nms %in% unlist(allCols))) stop( 'Missed some columns.' )

## Columns that will remain constant across years
consts <- with(allCols, c(id, consts))

## /* end load-data */
##'
##' # Primary Goals
##'
##' The goal of this script is to clean the extension growth data extracted from 
##' __seesapmas11__ file in [segments_clean.R](segments_clean.R).
##' This involves mostly transforming the data into a useable 'long' format.  
##' The temporary dataset `cextgr.rda` should have been saved in `../temp/cextgr.rda` 
##' following execution of `segments_clean.R`.
##' See [ext_growth.R](../summaries/ext_growth.R) for summaries of the transforming.
##'
##' # Columns
##'
##' **Constants**: {
{{prettify(consts)}}
##' }.  Note that `YRREX` is the year calculated by Lixi after which we can safely assume the 
##' extension growth is accurate (taking into account the problems with browsing/broken/etc.
##' leaders).  `ID` is the key to link with plants in segplants data (unique plants).
##' 
##' **Columns to transform**:
##'
##'   + `ENOTE[0-98]`: All of these except for `ENOTE99` go to long.  `ENOTE99` has different
##' info that has already been split up into the `NL` columns (new leaders)
##'   + `AGE88`: Total age (88)
##'   + `MINAGE[0-9]+`: Minimum ages (88, 89, 98, 99).
##'   + `EX[0-9]+`: The main target, the extension growths (44-98).
##'   + `PEX[0-9]+`: Partial extension growths (89, 98, 99).
##'   + `TERM[0-9]+`: Terminal conditions (99, 00)
##'   + `D[1-2]RM[0-9]+`: Distance remaining (98, 99)
##'   + `NL99_[1-3]`: Years of new leaders, up to 3, only measured in 99.
##'
##' **Remove**:
##'
##'   + `ENOTE99`: This information is split between `NL` columns.
##'   + `EXCNT`: Count of extension growths (88, 98, 99).
##'   + `EXSUM`: Sum of extension growths (98, 99).
##' 
##' # Transform
##'
##' Going to transform each variable separately and merge back together, since the 
##' years to melt by vary amongst all the variables and I cant think of a better way.
##'
##' ## Special Transforms
##'
##' Some of the variables require special treatment: 
##'
##'   + `NL_` will become an indicator variable, 1 where a new leader was determined.
##'   + `PEX` variables need to merged with `EX` and an indicator column used for 
##' partial growth instead.  Check for conflicts where we have both partial and full
##' extension measurements.
##' 
##+special-transforms, echo=FALSE
################################################################################
##
##                                    EX
##
################################################################################
## Melt EX first, then join each additional melted variable to it
id <- allCols$id
res <- data.table::melt(cextgr[, c(id, allCols$exs), with=FALSE], id.vars=id,
  na.rm=TRUE, value.name="EX", measure.vars=allCols$exs,
  variable.name="YEAR")
res[, YEAR := stri_extract(YEAR, regex='[0-9]+')]

## We have extension growth measure at least once in all years in the range
## 1944 - 1998:
stopifnot(all(diff(as.numeric(rownames(table(res$YEAR)))) == 1L))

################################################################################
##
##                                    PEX
##
################################################################################
## Check that `PEX` doesn't overlap with `EX` variables
exs <- substr(allCols$pexs, 2, nchar(allCols$pexs))
inds <- exs %in% names(cextgr)
exs <- exs[inds]  # dont check where EX doesn't exist
pexs <- allCols$pexs[inds]
tst <- lapply(seq_along(exs), function(i) {
  cextgr[!is.na(get(exs[[i]])) & !is.na(get(pexs[[i]])), .N]
})
stopifnot(all(unlist(tst) == 0))

## Merge PEXs with EXs and add an indicator variable `PEX`
pexs <- data.table::melt(cextgr[, c(id, allCols$pexs), with=FALSE],
  id.vars=id, measure.vars=allCols$pexs, na.rm=TRUE,
  value.name="EX", variable.name="YEAR")
pexs[, PEX := 1]                                       # add indicator column
pexs[, YEAR := stri_extract(YEAR, regex='[0-9]+')]     # clean YEAR
res <- merge(res, pexs, by=c("ID", "YEAR"), all=TRUE)  # merge

## Check EX.x and EX.y don't overlap
stopifnot(res[!is.na(EX.x) & !is.na(EX.y), .N] == 0L)

## merge EX columns into one, remove others
res[, EX := ifelse(!is.na(EX.x), EX.x, EX.y)][, c("EX.x", "EX.y") := NULL]

## Should only have PEX in 89, 98, 99
stopifnot(all(res[!is.na(PEX), .N, by=YEAR][, 
  YEAR] %in% c("89", "98", "99")))

################################################################################
##
##                                    NL
##
################################################################################
## New leader variables from ENOTE99, in NL99_[1-3] columns
## - Melt then shorten years to merge
nl99s <- data.table::melt(cextgr[, c(id, allCols$nl99s), with=FALSE],
  id.vars=id, measure.vars=allCols$nl99s, na.rm=TRUE,
  value.name="YEAR", variable.name="NL", variable.factor=FALSE)
nl99s[, NL := "1"]                     # indicator
nl99s[, YEAR := as.character(YEAR)]
nl99s[, YEAR := sub("^19", '', YEAR)]  # shorten year to merge

## Sanity checks:
## Should have one tree with 3 new leaders
stopifnot(nl99s[, .N, by=ID][, max(N)] == 3L)
stopifnot(nl99s[, .N, by=ID][N == 3L, .N] == 1L)

## Merge into res
## Note: there are 4 cases where there is no ext. growth measured,
## but a new leader was determined (all 1999 except one 1995)
res <- merge(res, nl99s, by=c(id, "YEAR"), all=TRUE) 

## /* end special-treatment */
##'

##'
##' ## Regular Transforms
##' 
##' The following will be melted, the YEAR extracted, then merged back into the 
##' resulting long dataset.
##'
##' **Columns to transform**:
##'
##'   + `ENOTE[0-98]`: All of these except for `ENOTE99` go to long.  `ENOTE99` has different
##' info that has already been split up into the `NL` columns (new leaders)
##'   + `AGE88`: Total age (88)
##'   + `MINAGE[0-9]+`: Minimum ages (88, 89, 98, 99).
##'   + `TERM[0-9]+`: Terminal conditions (99, 00)
##'   + `D[1-2]RM[0-9]+`: Distance remaining (98, 99)
##'
##+regular-transforms, echo=FALSE
## list of varying variables (remove ENOTE99)
allvary <- allCols[c("age", "minages", "terms", "d1rms", "d2rms", "enotes")]
allvary$enotes <- setdiff(allvary$enotes, "ENOTE99")

## For each element, melt by those variables, clean the year column,
## and join back to `res`
yrs <- unique(res$YEAR)  # should alread have all possible years
for (v in allvary) {
  coln <- stri_extract(v[[1]], regex='^[A-Z]+[1-2]?[A-Z]+', mode='first')
  tmp <- data.table::melt(cextgr[, c(id, v), with=FALSE],
    id.vars=id, na.rm=TRUE, value.name=coln, measure.vars=v,
    variable.name='YEAR')
  tmp[, YEAR := stri_replace_first_fixed(YEAR, coln, '')]
  res <- merge(res, tmp, by=c(id, 'YEAR'), all=TRUE)  # many variables have no EX
}

## Add constants (YRREX)
res <- cextgr[, consts, with=FALSE][res, on=id]

## /* end regular-transforms */
##'

##' 
##' ## Cleanup
##'
##' + Rearrange column order for convenience
##' + Clean the YEAR column to be full years
##' + Convert types to integer where possible
##' + Convert `YRREX` to indicator after years are cleaned up
##'
##+cleanup, echo=FALSE
## Column ordering
coln <- c(  # these will go up front for visual convenience
  "ID", "EX", "YEAR", "YRREX", "PEX", "NL", "TERM", "AGE", "MINAGE",
  "D1RM", "D2RM", "ENOTE"
)
setcolorder(res, coln)

## Convert year column to full years (not abbreviations) and to integers
res[, YEAR := paste0(ifelse(YEAR %in% c('00'), '20', '19'), YEAR)]
res[, YEAR := as.integer(YEAR)]

## Convert to integers
intCols <- c("YRREX", "PEX", "NL", "AGE", "MINAGE", "ENOTE")
res[, intCols := lapply(.SD, as.integer), .SDcols=intCols, with=FALSE]

## YRREX can just be an indicator variable, it is constant for each individual
res[, YRREX := ifelse(YRREX == YEAR, 1, NA)]

## /* end cleanup */
##'
##' # Save
##'
##' Saving as ../temp/extgr.rda with bzip2 compression
##'
##' The first 300 rows of the result:
##'
##+save, echo=FALSE

## When saving:
extgr <- res
## save(extgr, file="../temp/extgr.rda", compress="bzip2")

datatable(head(extgr, 300))

## /* end save */
##'

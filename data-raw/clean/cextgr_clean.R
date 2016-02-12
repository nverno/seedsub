## /* cextgr_clean.R --- 
## Filename: cextgr_clean.R
## Description: Clean extension growth data
## Author: Noah Peart
## Created: Fri Feb  5 16:13:50 2016 (-0500)
## Last-Updated: Thu Feb 11 23:57:49 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Extension Growth"
##' output_format: 
##'   html_document:
##'     theme: readable
##'     highlight: zenburn
##'     toc: true
##' ---
##' 
##+setup, include=FALSE, message=FALSE, echo=FALSE
library(knitr)
opts_chunk$set(fig.path='Figures/', cache=FALSE, echo=TRUE, message=FALSE)

library(data.table)
library(stringi)
source('utils.R')

## Optional:
has_dt <- require(DT)
has_dyg <- require(dygraphs)
has_xtab <- require(xtable)

dtopts <- list(scrollX=TRUE)
## /* end setup */
##'
##+load-data
## Loading data, extract some variables/years
load('temp/cextgr.rda')

## Describe these variables
nms <- unique(stri_extract_first(names(cextgr), regex="[A-Z]+[0-9]?[A-Z]+"))

## Ext. growth years, convert to a range format
yrs <- stri_extract_last(names(cextgr)[
  stri_detect(names(cextgr), regex='^EX[0-9]+')], regex='[0-9]+')
yrs <- sort(as.numeric(yrs))
extyrs <- sprintf('[%s]', 
  sub('([0-9]+):([0-9]+)', '19\\1 - 19\\2', deparse(as.integer(yrs))))

## Terminal conditions
term00 <- cextgr[!is.na(TERM00), unique(TERM00)]
term99 <- cextgr[!is.na(TERM99), unique(TERM99)]

## Column groups
nms <- names(cextgr)
patts <- list(                        # patterns to match groups
  id      = '^ID$',
  consts  = '^AGE88$|^YRREX$',        # won't melt these by year
  minages = '^MINAGE[0-9]+',
  pexs    = '^PEX[0-9]+', 
  exs     = '^EX[0-9]+',
  terms   = '^TERM[0-9]+',
  d1rms   = '^D1RM[0-9]+',
  d2rms   = '^D2RM[0-9]+',
  enotes  = '^ENOTE[0-9]+',
  nl99s   = '^NL99_[1-3]',
  others  = '^EX(?:CNT|SUM)[0-9]+'    # probably gonna ignore these
)

allCols <- lapply(patts, grep, nms, perl=TRUE, value=TRUE)
if (!all(nms %in% unlist(allCols))) stop( 'Missed some columns.' )
## /* end load-data */
##'

##' The goal of this script is to clean the extension growth data extracted from 
##' __seesapmas11__ file in [cseed.R](cseed.R).  This involves mostly transforming the
##' data into a ussable 'long' format.  The temporary dataset `cextgr.rda` should
##' have been saved in `./temp/cextgr.rda` following execution of `cseed.R`.
##'
##' ## Table: Head of raw data
##+raw-table
if (has_dt) {
  datatable(head(cextgr), options=dtopts)
} else kable(head(cextgr))
## /* end raw-table */
##'
##' ## Columns descriptions
##'
##' + `ID`: Unique id linking extension growth back to `cseed` dataset.
##' + `AGE88`: Total age for 1988 seedlings.  Cases where `AGE88` was double-checked:
##'     - __== 1__: left alone
##'     - __Missing__: left alone
##'     - __<= number of EXs__: If __less__, set to __missing__, if __equal__ it was 
##' left alone
##' + `MINAGE`: Minimum age for tagged seedlings.  This variable was checked the same
##' as `AGE88`.  Measured in 1988, 1989, 1998, and 1999.
##' + `PEX`: Partial growth columns, ie they only include part of years growth.  Measured
##' in 1989, 1998, and 1999.
##' + `EX`: Extension growth collected for years:
{{extyrs}}
##' .  However, protocols for different collection years (1988, 1989, 1998, 199) differed.
##'     - __1988__: For seedlings/saplings, max __10 years__ measured (ie. 1978-1987). 
##'     - __1989__: For seedlings, __no limit__ to number of measurements.  For saplings,
##' it was measured for as many years as possible for the __first__ sapling of each species
##' encountered.  The __second__ and __third__ sapling of each species in each segment
##' has only up to __10 yrs__ of ext. growth measured.
##'     - __1998__: For seedlings, __no limit__ to number of measurements.  For saplings, 
##' max __10 years__.
##'     - __1999__: For seedlings, max __11 years__ measured.  For saplings, max __11 years__.
##' + [`ENOTE`](#enote): These were notes related to extension growths. 
##' These values were used to make some corrections to `EX`, documented extensively in 
##' _Segment Data History 1988-2012.docx_.  Possible values are enumerated as follows:
##'     - __1__: New leader
##'     - __2__: Browse off, no extension growth
##'     - __3__: Not browsed, just no growth.
##'     - __4__: Dead top.
##'     - __5__: No dominant leader.
##' + [`ENOTE99`](#enote99): Different than the other `ENOTE` columns, 
##' this one is a character column containing up to three years in the past when a 
##' lateral became a new leader.
##' + `NL_[1-3]`: Variables derived from `ENOTE99`.  They refer to the first, second, and
##' third years when there was a new leader for 1999 tagged plants.
##' + `EXSUM`: Sum of `EX` measured (derived).
##' + `EXCNT`: Number of `EX` measured (derived).
##' + `YRREX`: A new variable indicating the year until which the `EX` data are reliable.
##' This would be the last year a new leader came out or a top was browsed off; for other 
##' plants, this is the earliest year an `EX` was estimated.
##' + [`TERM`](#terminal-conditions): Terminal leader condition, recorded in 1999 and 2000.  
##' However, the values for each year are recorded differently:
##'     - __1999__: Mostly documented
##'         - __HE__: Healthy
##'         - __UN__: Unhealthy
##'         - __IN__: Insects
##'         - __ND__: No dominant leader
##'         - __NG__: No growth
##'         - __DE__: Dead top
##'         - __BRS__: Browsed off
##'         - __BR__: Browsed off (? documented as being changed to `BRS`)
##'         - __BK__: Broken
##'         - __DEFOLIAT__: Defoliated
##'         - __LITTLE F__: Little foliage?
##'         - __CANT ASS__: Can't access (or assess?)
##'         - __TREE LAY__: ?
##'         - __1__: ?
##'     - __2000__: Can't find documentation, but they seem to straightforward.
##'         - __HE__: Healthy (1)
##'         - __H__: Healthy? (68)
##'         - __BR__: Browsed (1)
##'         - __NEW DOM__: New dominant leader (2)
##'         - __NO DOM/BR__: No dominant leader/browsed (1)
##'         - __NO DOM__: No dominant leader (9)
##'         - __NG__: No growth (5)
##' + `D1RM`: Distance remaining from the last positively identified extension
##' growth measurement to the root collar for ABBA/PIRU seedlings (note on years where
##' a limited number of measurements were taken, this would be the distance remaining
##' from the last measurement point).  Collected in 1998/1999.
##' + `D2RM`: Distance remaining after counting the minimal age for ABBA/PIRU.  Collected
##' for __saplings only__ in 1998, and for __seedlings and saplings__ in 1999.
##'
##'
##' ### ENOTE
##' `ENOTE[77-89]` are coded 1:5.  `ENOTE99` was separated into `NL99_[1-3]` columns.
##' Below, the `ENOTE[77-89]` variables are shown as counts of each value/year 
##' in tabular form (summarized as heatmap - hover over cell for value).
##'
##+enotes
## Note: ENOTE99 is character, the rest are doubles
ids <- allCols$enotes
ids <- setdiff(ids, 'ENOTE99')
enotes <- cextgr[, c('ID', ids), with=FALSE]
enotes <- melt(enotes, measure=patterns("^ENOTE"),
  value.name='ENOTE', variable.name='YEAR', na.rm=TRUE)
enotes[, YEAR := stri_extract(YEAR, regex='[0-9]+')]
enotes[, YEAR := as.numeric(paste0("19", YEAR))]

## Ignore value of 6 in 1998
enotes <- enotes[ENOTE < 6, ]
tab <- dcast(enotes[,-1,with=FALSE], YEAR ~ ENOTE)[order(-YEAR)]

if (requireNamespace('d3heatmap', quietly = TRUE)) {
  d3heatmap(tab[,-1,with=FALSE], Rowv=FALSE, Colv=FALSE, colors='Blues', revC=FALSE,
    dendrogram='none', labRow=tab[['YEAR']], show_grid=2)
}
if (has_dt) {
  datatable(tab, options=dtopts, 
    caption='Table: Counts of each value of ENOTE/YEAR.')
}
## /* end enotes */
##'
##' ### ENOTE99
##' Examine `ENOTE99` and the `NL99_[1-3]` variables.  `ENOTE99` was separated into
##' the three `NL99` columns, so should be redundant.  There was only one plant that had
##' three new leaders (`ID` 431).  This checks that the separated
##' variables are as expected and plots:
##'   1. The cumulative extension growth of all individuals with at least two new leaders.
##'   2. The non-cumulative extension growth of the above.
##'   3. The cumulative/non-cumulative ext. growth for the plant with 
##' three leader changes, with vertical lines showing the events of leader changes.
##'
##+enote99
## Convert ENOTE99 to NL columns
nlcols <- allCols$nl99s
tst <- cextgr[!is.na(ENOTE99), c('ENOTE99', nlcols), with=FALSE]
tst[, paste0('N', 1:3) := lapply(tstrsplit(ENOTE99, ' ', fixed=TRUE), 
  function(x) suppressWarnings(as.numeric(paste0('19', x))))]

## Check new ones against NL
chk <- tst[, all(N1 == NL99_1 & N2 == NL99_2 & N3 == NL99_3, na.rm=TRUE)]
if (!chk) stop( 'ENOTE99 -> NL columns check failed' )  # no problems

## Take a look at some of these multiple nl cols
tst <- cextgr[!is.na(NL99_1) & !is.na(NL99_2), ]
n3 <- tst[, sum(!is.na(NL99_3))]  # only one has three leader changes

## Make a graphic to see the growth of these guys (19 of em)
dat <- tst[, with(allCols, c(id, exs)), with=FALSE]
dat <- melt(dat, id.vars=allCols$id, measure=allCols$exs, value.name='EX', 
  variable.name='YEAR', na.rm=TRUE)
dat[, YEAR := as.numeric(paste0('19', stri_extract(YEAR, regex='[0-9]+')))]

## Prepare as time column followed by y-value columns
out <- suppressWarnings(dcast(dat, YEAR ~ ID, fill='EX', value.var='EX'))

## Plot 1: non-cumulative ext. growth for all indiv. with at least two NL99
library(dygraphs)
dygraph(out, xlab='Year', ylab='Extension Growth',
  main='Extension Growth for indiv. with at least\nNL99_1 and NL99_2')

## Get cumulative growth
f <- function(x) c(rep(NA, sum(is.na(x))), cumsum(x[!is.na(x)]))
res <- out[, lapply(.SD, f), .SDcols=setdiff(names(out), 'YEAR')][, YEAR:=out$YEAR]
setcolorder(res, c('YEAR', setdiff(names(res), 'YEAR')))

## Plot 2: cumulative
dygraph(res, xlab='Year', ylab='Cumulative Extension Growth',
  main='Extension Growth for indiv. with at least\nNL99_1 and NL99_2')

## Get dates of events
events <- tst[list(ID=dat[,unique(ID)]), 
  c('ID', allCols$nl99s), on='ID', with=FALSE][ID == 431, ]

dt <- out[, 1:2, with=FALSE][, `Cumulative Growth` := res[,2,with=FALSE]]
setnames(dt, '431', 'Non-cumulative Growth')

## Plot 3: growth with 3 NL
dygraph(dt, ylab='Extension Growth', xlab='Year',
  main='Extension Growth for only plant with 3 NLs (431)') %>%
  dyEvent(events$NL99_1, 'Third new leader', color='red') %>%
  dyEvent(events$NL99_2, 'Second new leader', color='red') %>%
  dyEvent(events$NL99_3, 'First new leader', color='red')

## /* end enote99 */
##'
##' ### Terminal conditions
##' Tabulated values of `TERM99` and `TERM00`
##'
##+term, results='as.is'
terminal <- melt(cextgr[, .(ID, TERM99, TERM00)], measure=patterns("^TERM"),
  value.name='TERM', variable.name='YEAR')
terminal[, YEAR := stri_extract(YEAR, regex='[0-9]+')]
terminal[, YEAR := ifelse(YEAR == '00', '2000', '1999')]

tab <- table(terminal$TERM, terminal$YEAR)
kable(tab, caption='Table: TERM labels by collection year.')

## /* end term */
##'

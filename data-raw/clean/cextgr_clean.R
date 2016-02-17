## /* cextgr_clean.R --- 
## Filename: cextgr_clean.R
## Description: Clean extension growth data
## Author: Noah Peart
## Created: Fri Feb  5 16:13:50 2016 (-0500)
## Last-Updated: Wed Feb 17 05:52:57 2016 (-0500)
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
## /* end setup */
##'
##+load-data, echo=FALSE
## Loading data, extract some variables/years
load('../temp/cextgr.rda')

## Describe these variables
nms <- unique(stri_extract_first(names(cextgr), regex="[A-Z]+[0-9]?[A-Z]+"))

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
##' Some of the variables require special treatment: 
##'
##'   + `NL_`, the names need to be stripped of 99 and then merged only for 99.
##'   + `PEX` variables need to merged with `EX` and an indicator column used for 
##' partial growth instead.  Check for conflicts where we have both partial and full
##' extension measurements.
##' 
##+transform, echo=FALSE
## Melt EX first, then join each additional melted variable to it
id <- allCols$id
res <- melt(cextgr[, c(id, allCols$exs), with=FALSE], id.vars=id,
  na.rm=TRUE, value.name="EX", measure.vars=allCols$exs,
  variable.name="YEAR")
res[, YEAR := stri_extract(YEAR, regex='[0-9]+')]

## We have extension growth measure at least once in all years in the range
## 1944 - 1998:
tst <- all(diff(as.numeric(rownames(table(res$YEAR)))) == 1L)

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
pexs <- cextgr[, allCols$pexs, with=FALSE]

## list of easy varying variables (remove ENOTE99)
allvary <- allCols[c("age", "minages", "terms", "d1rms", "d2rms", "enotes",
  "excnts", "exsums")]
allvary$enotes <- setdiff(allvary$enotes, "ENOTE99")

## For each element, melt by those variables, clean the year column,
## and join back to `res`
yrs <- unique(res$YEAR)
for (v in allvary) {
  coln <- stri_extract(v[[1]], regex='^[A-Z]+[1-2]?[A-Z]+', mode='first')
  tmp <- melt(cextgr[, c(id, v), with=FALSE],
    id.vars=id, na.rm=TRUE, value.name=coln, measure.vars=v,
    variable.name='YEAR')
  tmp[, YEAR := stri_replace_first_fixed(YEAR, coln, '')]
  res <- if (all(unique(tmp$YEAR) %in% yrs)) {
    tmp[res, on=c(id, 'YEAR')]                     # simple join on ID and YEAR
  } else {
tst <- merge(res, tmp, by=c(id, 'YEAR'), all=TRUE)  # blank rows / new years
  }
}

## Add constants
res <- cseed[, consts, with=FALSE][res, on=ids]

## Add a unique plot identifier
res[,PID := .GRP, by=c('CONTNAM', 'STPACE')]

## Put some of the columns up front, soil cols at the back
coln <- c(  # these will go up front for visual convenience
  'PID', 'ID', 'CONTNAM', 'STPACE', 'SPEC', 'HT', 'SUB', 'SUBON', 'YEAR', 
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
input <- list(temp='system2("echo", "Nice...")')
commandString <- paste0("qplot(", input$temp, ", data=., geom='density')")

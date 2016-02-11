##' Contour segments plot-level data.
##'
##' For more details, see \code{segdata}.  This data links to 
##' individual-level data in \code{segplants} by the `PID` variable.
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 23680 rows and 13 columns.
##' \itemize{
##'    \item PID: Plot identifier, this is determined by unique combinations of \code{CONTNAM} and \code{STPACE}.
##'    \item CONTNAM: Contour name
##'    \item STPACE: Beginning pace of a segment.
##'    \item ELEVCL: Elevation class
##'    \item ASPCL: Aspect class
##'    \item SOILCL[1-2]: Soil class
##'    \item HISTO: Soil type.
##'    \item SPODO: Soil type.
##'    \item INCEP: Soil type.
##'    \item MINER: ?
##'    \item ROCK: Soil type
##'    \item PHISTO: Soil type.
##' }
"segplots"

##' Contour segments individual-level data.
##'
##' For more details, see help for \code{segdata}.  This data links to 
##' plot-level data in \code{segplots} by the `PID` variable.
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 23680 rows and 34 columns.
##' \itemize{
##'    \item PID: Plot identifier, this is determined by unique combinations of \code{CONTNAM} and \code{STPACE}.
##'    \item ID: Unique plant ID
##'    \item SPEC: Species abbreviation.
##'    \item HT: Height of plant (cm).
##'    \item STMLEN: Stem length (may differ from \code{HT} for plants that are leaning significantly).
##'    \item YEAR: Year
##'    \item YRTAG: Year a plant was initially tagged.
##'    \item ALONG: Distance from the start of the segment to a plant (cm?).
##'    \item DISUPDN: Distance up/down from segment baseline to a plant (cm?).
##'    \item YRMORT: Year of mortality
##'    \item STAT: Status of plant (live or dead)
##'    \item DECM: Decline class
##'    \item TAG: Tag assigned to plant in field (not unique in dataset).
##'    \item CHECK: ???
##'    \item CII: Crown illumination index
##'    \item PALONG: Keeper's pace from the start of the segment to the seedling/sapling, which is accurate to +/-1 pace.  Similar to \code{ALONG} used in 1998/1999, but in units of 'pace'.  
##'    \item SGDPS: Segment displacement from the contour in paces ('U'=up, 'D'=down).  Note that these can vary across years.
##'    \item SGLEN: Segment length (m).  Note this can vary between years.
##'    \item NOTE: Notes from the field.
##'
##'    \item Data for Seedlings Only:
##'    \itemize{
##'       \item SUB: Substrate seedling was growing on.
##'       \item SUBON: Underlying substrate (ie. the substrate under \code{SUB}).
##'       \item BRHT: Browsing height
##'       \item SDSP[1-2]: The first/second species where tagging of seedling ceased before the end of the segment.  See also \code{NSDSP[1-2]} and \code{DSDSP[1-2]}.
##'       \item NSDSP[1-2]: Total counts of species in \code{SDSP1}/\code{SDSP2}.
##'       \item DSDSP[1-2]: Distance from beginning of segment to where tagging ceased in a segment for species defined in \code{SDSP1}/\code{SDSP2}.
##'       \item HRB[1-3]: Abbreviations of the three species of herbs, shrubs, or trees with the highest cover in the 50cm radius circle around a seedling.  If it was a shrub ACSP or ACPE, it was recorded as 'ACSP_SH' or 'ACPE_SH'.
##'       \item NFCVR: Percent cover in herb layer in a 50cm radius circle centered on the seedling, excluding the focal seedling. 
##'       \item TCVR: Total percent cover in herb layer (including herb, shrub, tree species <= 1m) including the seedlings in a 50cm radius circle centered on the seedling.  This was collected for 1988 tagged seedlings.
##'     }
##' }
"segplants"

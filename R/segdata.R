##' Contour segments seedling and sapling data.
##'
##' This data is the 'long' version of the raw master file.  There is a single year
##' column, and all the variables that vary by year are now each in a single column.
##' This dataset contains both plot-level and individual-level data.  The two complementing
##' datasets in this package (\code{segplants} and \code{segplots})are the same as this, 
##' only the plot-level and individual-level variables have been separated 
##' (and are linked by a plot identifier `PID`).
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 23680 rows and 45 columns.
##' \itemize{
##'
##'     \item Plot-level:
##'     \itemize{
##'        \item PID: Plot identifier, this is determined by unique combinations of \code{CONTNAM} and \code{STPACE}.
##'        \item CONTNAM: Contour name
##'        \item STPACE: Beginning pace of a segment.
##'        \item ELEVCL: Elevation class
##'        \item ASPCL: Aspect class
##'        \item SOILCL[1-2]: Soil class
##'        \item HISTO: Soil type.
##'        \item SPODO: Soil type.
##'        \item INCEP: Soil type.
##'        \item MINER: ?
##'        \item ROCK: Soil type
##'        \item PHISTO: Soil type.
##'     }
##'
##'     \item Individual-level:
##'     \itemize{
##'        \item ID: Unique plant ID
##'        \item SPEC: Species abbreviation.
##'        \item HT: Height of plant (cm).
##'        \item STMLEN: Stem length (may differ from \code{HT} for plants that are leaning significantly).
##'        \item YEAR: Year
##'        \item YRTAG: Year a plant was initially tagged.
##'        \item ALONG: Distance from the start of the segment to a plant (cm?).
##'        \item DISUPDN: Distance up/down from segment baseline to a plant (cm?).
##'        \item YRMORT: Year of mortality
##'        \item STAT: Status of plant (live or dead)
##'        \item DECM: Decline class
##'        \item TAG: Tag assigned to plant in field (not unique in dataset).
##'        \item CHECK: ???
##'        \item CII: Crown illumination index
##'        \item PALONG: Keeper's pace from the start of the segment to the seedling/sapling, which is accurate to +/-1 pace.  Similar to \code{ALONG} used in 1998/1999, but in units of 'pace'.  
##'        \item SGDPS: Segment displacement from the contour in paces ('U'=up, 'D'=down).  Note that these can vary across years.
##'        \item SGLEN: Segment length (m).  Note this can vary between years.
##'        \item NOTE: Notes from the field.
##'
##'        \item Data for Seedlings Only:
##'        \itemize{
##'           \item SUB: Substrate seedling was growing on.
##'           \item SUBON: Underlying substrate (ie. the substrate under \code{SUB}).
##'           \item BRHT: Browsing height
##'           \item SDSP[1-2]: The first/second species where tagging of seedling ceased before the end of the segment.  See also \code{NSDSP[1-2]} and \code{DSDSP[1-2]}.
##'           \item NSDSP[1-2]: Total counts of species in \code{SDSP1}/\code{SDSP2}.
##'           \item DSDSP[1-2]: Distance from beginning of segment to where tagging ceased in a segment for species defined in \code{SDSP1}/\code{SDSP2}.
##'           \item HRB[1-3]: Abbreviations of the three species of herbs, shrubs, or trees with the highest cover in the 50cm radius circle around a seedling.  If it was a shrub ACSP or ACPE, it was recorded as 'ACSP_SH' or 'ACPE_SH'.
##'           \item NFCVR: Percent cover in herb layer in a 50cm radius circle centered on the seedling, excluding the focal seedling. 
##'           \item TCVR: Total percent cover in herb layer (including herb, shrub, tree species <= 1m) including the seedlings in a 50cm radius circle centered on the seedling.  This was collected for 1988 tagged seedlings.
##'     }
##'   }
##' }
"segdata"

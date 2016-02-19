##' Contour segments substrate data
##'
##' This dataset contains the substrate data for segment plots.  It was recorded
##' for different plots in 1989, 1998, 1999, and 2003.  Different protocols were followed
##' in different years, but the general idea was to record substrate as being 
##' moss, bare rock/soil, or various types of litter (mixed, deciduous, or coniferous).
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 352 rows and 36 columns.
##' \itemize{
##'     \item PID: Plot identifier, links to `segplots`.  Some are NA, where only substrates
##' were measured.
##'     \item CONTNAM: Contour name
##'     \item STPACE: Pace along contour
##'     \item YEAR: Year of census.
##'     \item SGDSP: Segment displacement from contour ("U"=up, "D"=down) in paces (or m?)
##'     \item QPOS: Quadrat
##'     \item Substrate types:
##'     \itemize{
##'        \item BLA5/BLD5: Bole aerial/Bole dead on the ground (at least 5cm diameter)
##'        \item BSOIL: Bare soil
##'        \item RCK: Rock
##'        \item WATER: Water
##'        \item WDG5/WDG1: Dead wood on the ground (at least 5cm/1cm diameter)
##'        \item MSSG: Moss
##'        \item TIPA: Tip-up
##'        \item STPA: Stumps
##'        \item LITT/LITM/LITC/LITD: Total General/Mixed/Coniferous/Deciduous litters
##' on the ground.  In 1999, this is the sum of \code{LITM}, \code{LITC} and \code{LITD}.
##' In 2003, this is the sum of \code{LITM}, \code{LITC}, \code{LITD}, \code{LITCRCK}, and 
##' \code{LITMRCK}.
##'        \item WDA5/WDA1: Wood aerial (at least 5cm/1cm diameter)
##'        \item MSWDA5/MSWDA1: Moss on aerial wood (at least 5cm/1cm diameter)
##'        \item LITCRCK/LITMRCK/MSRCK: Coniferous/Mixed/Moss on rock
##'        \item MSBLA5: Moss on standing live bole (at least 5cm diameter)
##'        \item MSWDG5: Moss on dead wood on the ground (at least 5cm diameter)
##'     }
##'     \item ASPCL: Aspect class
##'     \item ELEVCL: Elevation class
##'     \item SEASON: Season data was collected (only for summer of 2003)
##'     \item DATE: Date data was collected
##'     \item SUMG: Derived variable (sum of everythin on the ground). For the various
##' years this included the following:
##'     \itemize{
##'        \item 1989: \code{BLA5}, \code{BLD5}, \code{BSOIL}, \code{RCK}, \code{WATER},
##' \code{WDG5}, \code{MSSG}, \code{LITT}.  When \code{SUMG} was <95 or >105, \code{LITT}
##' was adjusted to make \code{SUMG} equal to 100.
##'        \item 1998: \code{BLA5}, \code{BLD5}, \code{BSOIL}, \code{RCK}, \code{WATER},
##' \code{TIPA}, \code{STPA}, \code{MSSG}, \code{LITT}, \code{WDG1}.  No corrections made.
##'        \item 1999: \code{BLA5}, \code{BLD5}, \code{BSOIL}, \code{RCK}, \code{WATER},
##' \code{TIPA}, \code{STPA}, \code{MSSG}, \code{LITT} (the sum of \code{LITM}, \code{LITC},
##' and \code{LITD}), \code{WDG5}.  No corrections made.
##'        \item 2003: Only substrates on the ground were recorded.  These were \code{BLA5}, 
##' \code{BLD5}, \code{BSOIL}, \code{LITM}, \code{LITD}, \code{LITC}, \code{LITCRCK},
##' \code{LITMRCK}, \code{WDG5}, \code{MSSG}, \code{MSRCK}, \code{MSBLA5}, and \code{MSWDG5}. 
##' No corrections made.
##'     }
##'     \item SUMA: Derived variable (sum of everything including aerial). In years:
##'     \itemize{
##'        \item 1998: \code{SUMG} + \code{WDA1}
##'        \item 1999: \code{SUMG} + \code{WDA5}
##'     }
##'     \item CORRECT: Indicates if a correction was made to adjust for \code{SUMG}.
##' }
"segsub"

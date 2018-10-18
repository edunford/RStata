#' set_stata_config
#'
#' Point R session to local copy of Stata.
#'
#' @param StataPath local path to stata configuraion
#' @param StataVersion integer denoting the version of stata
#'
#' @return Printed statement
#' @export
#'
#' @examples
#' \dontrun{
#' set_stata_config(StataPath = '/Applications/Stata/StataSE.app/Contents/MacOS/StataSE',StataVersion = 15)
#' }
set_stata_config = function(StataPath='/Applications/Stata/StataSE.app/Contents/MacOS/StataSE',
                            StataVersion=15){
  invisible(options("RStata.StataPath" = StataPath))
  invisible(options("RStata.StataVersion" = StataVersion))
}

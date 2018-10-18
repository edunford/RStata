#' Send commands to a Stata process
#'
#''
#' Function that sends commands to a Stata process.
#'
#'
#' @param src character vector of length 1 (path to \code{.do} file) or more
#' (a set of stata commands). See examples.
#'
#' @param data_in \code{\link{data.frame}} to be passed to Stata
#'
#' @param data_out logical value. If \code{TRUE}, the data at the end of
#' the Stata command are returned to R.
#'
#' @param stata_path Stata command to be used
#'
#' @param stata_version Version of Stata used
#'
#' @param stata_echo logical value. If \code{TRUE} stata text output will be printed
#'
#' @param text_out logical value. If TRUE, consol text is exportable
#'
#' @importFrom haven read_dta write_dta
#'
#' @examples
#' \dontrun{
#' ## Single command
#' stata("help regress") #<- this won't work in Windows dued to needed
#'                       #   batch mode
#'
#' ## Many commands
#' stata_src <- '
#'
#' version 10
#' set more off
#' sysuse auto
#' reg mpg weight
#'
#' '
#' stata(stata_src)
#'
#' ## External .do file
#' stata("foo.do")
#'
#' ## Data input to Stata
#' x <- data.frame(a = rnorm(3), b = letters[1:3])
#' stata( "sum a", data.in = x)
#'
#' ## Data output from Stata (eg obtain 'auto' dataset)
#' auto <- stata("sysuse auto", data.out = TRUE)
#' head(auto)
#'
#' ## Data input/output
#' (y <- stata("replace a = 2", data.in = x, data.out = TRUE))
#' }
#' @export
stata <- function(src = stop("At least 'src' must be specified"),
                  data_in = NULL,
                  data_out = FALSE,
                  stata_path = getOption("RStata.StataPath", stop("You need to set up a Stata path; ?chooseStataBin")),
                  stata_version = getOption("RStata.StataVersion", stop("You need to specify your Stata version")),
                  stata_echo = FALSE,
                  text_out = FALSE,
                  ...
                  )
{
    ## -------------------------
    ## Data validation and setup
    ## -------------------------
    if (!is.character(src))
        stop("src must be a character")

    if (!(is.null(data_in) | is.data.frame(data_in)))
        stop("data_in must be NULL or a data.frame")

    if (!is.logical(data_out))
        stop("data_out must be logical")

    if (!is.numeric(stata_version))
        stop("stata_version must be numeric")

    if (!is.logical(stata_echo))
        stop("stata_echo must be logical")

    OS <- Sys.info()["sysname"]
    OS.type <- .Platform$OS.type
    SRC <- unlist(lapply(src, strsplit, '\n'))
    dataIn <- is.data.frame(data_in)
    dataOut <- data_out[1L]
    textOut <- text_out[1L]
    stataVersion <- stata_version[1L]
    stataEcho <- stata_echo[1L]

    ## -----------------
    ## OS related config
    ## -----------------
    ## in Windows and batch mode a RStata.log (naming after RStata.do
    ## below) is generated in the current directory

    if (OS %in% "Windows") {
        winRStataLog <- "RStata.log"
        on.exit(unlink(winRStataLog))
    }

    ## -----
    ## Files
    ## -----

    ## tempfile could be misleading if the do source other dos
    ## with relative paths
    doFile <- "RStata.do"
    on.exit(unlink(doFile), add = TRUE)

    if (dataIn){
        ## dtaInFile <- tempfile("RStataDataIn", fileext = ".dta")
        ## Windows/Stata8 unhappy?
        dtaInFile <- "RStataDataIn.dta"
        on.exit(unlink(dtaInFile), add = TRUE)
        haven::write_dta(data = data_in,
                  path = dtaInFile)
    }

    if (dataOut) {
        ## dtaOutFile <- tempfile("RStataDataOut", fileext = ".dta")
        ## Windows/Stata8 unhappy?
        dtaOutFile <- "RStataDataOut.dta"
        on.exit(unlink(dtaOutFile), add = TRUE)
    }

    ## -------------------------
    ## Creating the .do file ...
    ## -------------------------

    ## External .do script 'support': KIS
    if (file.exists(SRC[1L]))
        SRC <- readLines(SRC[1L])

    ## put a placeholder around the part of interest, in order to find
    ## it easily (when removing overhead/setup code for each run)
    cut_me_here <- 'RSTATA: cut me here'
    cut_me_comment <- paste0('/*', cut_me_here, '*/')

    ## capture noisily and set cut points
    SRC <- c(
    {if (dataIn) sprintf("use %s", tools::file_path_sans_ext(dtaInFile))
     else ''},
    'capture noisily {',
    cut_me_comment,
    SRC,
    cut_me_comment,
    '} /* end capture noisily */')

    ## set more off just to be sure nothing will freeze (hopefully :) )
    SRC <- c('set more off', SRC)

    ## put a save or saveold at the end of .do if data.out == TRUE
    ## for Stata 14, saveold defaults to a Stata 13 dta file
    ## -> use the (Stata 14 only) saveold option: "version(12)" to allow
    ## foreign::read.dta() read compatibility
    if (dataOut){
        save_cmd <- sprintf("%s %s%s",
                            if (stataVersion >= 13) "saveold" else "save",
                            tools::file_path_sans_ext(dtaOutFile),
                            if (stataVersion >= 14) ", version(12)" else "")
        SRC <- c(SRC, save_cmd)
    }

    ## adding this command to the end simplify life if user make changes but
    ## doesn't want a data.frame back
    SRC <- c(SRC, "exit, clear STATA")

    ## -------------
    ## Stata command
    ## -------------

    ## With Windows version, /e is almost always needed (if Stata is
    ## installed with GUI)
    stataCmd <- paste(stata_path,if (OS %in% "Windows") "/e" else "","-q -e do",doFile)

    ## ---
    ## IPC
    ## ---
    ## setup the .do file
    ## con <- fifo(doFile, "w+") # <- freeze with fifo in Window
    con <- file(doFile, "w")
    writeLines(SRC, con)
    close(con)

    ## execute Stata
    cat('Running Stata code...\n')
    rdl <- pipe(stataCmd,open = "rt")
    close(rdl)
    cat("Code Completed!\n")

    ## Compile log
    stataLog = readLines('RStata.log')
    cutpoints <- grep(cut_me_here, stataLog)
    stataLog <- stataLog[seq.int(cutpoints[1] + 1, cutpoints[2] - 1)]
    if (stataEcho) {
      cat(stataLog, sep = "\n")
      file.remove('Rstata.log')
    }else{
      file.remove('Rstata.log')
    }

    ## ------------------
    ## Get data outputted
    ## ------------------
    if(textOut & !dataOut){
      invisible(stataLog)
    }else{
      if (dataOut){
        res <- read_dta(dtaOutFile, ...)
        invisible(res)
      }
    }

}

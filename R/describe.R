##' @include format.R
NULL

##' Roxygen tabulate
##' Similar to http://r-pkgs.had.co.nz/man.html to make tabular roxy output.
##'
##' @param df data.frame
##' @param ... arguments passed to format
##' @return character vector of lines for table
roxy_tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- paste(do.call("paste", c(cols, list(sep = " \\tab "))),
    rep(c('\\cr', ''), times=c(nrow(df)-1L, 1L)), sep="")
  
  ns <- names(df)
  ns[!nzchar(ns)] <- '""'
  headers <- paste0(paste(paste0("\\strong{", ns, "}"),
    collapse=" \\tab "), '\\cr')
  
  structure(
    c(paste("\\tabular{", paste(col_align, collapse = ""), "}{", sep=""),
      headers, contents, "}"), class='tabular')
}

##' Create dataset descriptions for roxygen.  The workhorse.
##'
##' @param data Input data
##' @param text Optional text describing dataset (to go in description area).
##' @param file Optional location of file with description of data
##' @param outfile Optional location of file to write output
##' @param fileEncoding File encoding
##' @param envir Environment to find data
##' @param encoding encoding
##' @param comment.char prefix character for roxygen template (default )
##' @param start Start of itemized list (default "\\\\itemize \{")
##' @param end End of itemized list (default "\}")
##' @param item Format for an item in list as regex to pass to sprintf.
##' @param indent How many spaces to indent itemized lists (default: 3)
##' @param sublist Optional list of columns (factors/characters) that should be sublisted.
##' @param add_tables List of columns to add tabular counts in description (or TRUE for all).
##' @param ... further arguments passed to roxy_format
##' @examples
##' ## run devtools::document() afterward for example
##' \dontrun{
##'   roxy_itemize(mtcars, text='Dataset about cars', outfile='temp.R', add_tables=TRUE)
##' }
##' @importFrom roxygen2 object_format
##' @return Vector of lines for roxygen metadata, or write to file if \code{outfile}.
##' @export
describe <- function(data, text, file, outfile, fileEncoding="", envir=NULL,
                         encoding="unknown", comment.char="##'",
                         start="\\itemize{", end="}", item="\\item %s:",
                         indent=3, sublist=NULL, add_tables=NULL, ...) {
  if (!is.null(envir)) {
    out_name <- data
    data <- get(data, envir=envir)
  }
  if (!inherits(data, 'data.frame')) stop('Data doesn\'t inherit from data.frame.')
  if (missing(file) && missing(text)) {
    file <- textConnection(deparse(substitute(data)), encoding="UTF-8")
    encoding <- "UTF-8"
    on.exit(close(file))
  }
  if (missing(file) && !missing(text)) {
    file <- textConnection(text, encoding="UTF-8")
    encoding <- "UTF-8"
    on.exit(close(file))
  }
  if (is.character(file)) {
    file <- if (nzchar(fileEncoding)) {
              file(file, "rt", encoding=fileEncoding)
    } else file(file, "rt")
    on.exit(close(file))
  }
  if (!inherits(file, 'connection'))
    stop("'file' must be a character string or connection")
  if (!isOpen(file, 'rt')) {
    open(file, 'rt')
    on.exit(close(file))
  }
  
  ## Initial description (if no text or file, then just the data name)
  preamble <- strwrap(paste(readLines(con=file, encoding=encoding), 
    collapse='\n'), prefix=comment.char, indent=1, exdent=1)
  
  ## Add the @format part (roxygen2::object_format)
  form <- strwrap(paste('@format', roxygen2::object_format(data)),
    indent=1, exdent=indent, prefix=comment.char)

  ## Create the items -- this can contain sublists for factor or character items
  in1 <- paste(c(comment.char, rep(' ', 1L+indent)), collapse='')
  in2 <- paste(c(comment.char, rep(' ', 1L+2L*indent)), collapse='')
    
  ## Create column items/descriptions
  all_tables <- identical(add_tables, TRUE)
  res <- setNames(lapply(names(data), function(i) {
    ff <- roxy_format(data[[i]], add_table=all_tables || i %in% add_tables, ...)
    cs <- vapply(ff, class, character(1), USE.NAMES = FALSE)
    desc <- which(cs == 'description')
    itm <- paste(in1, sprintf(item, i), ff[desc], collapse='')
    c(itm, lapply(ff[-desc], function(x) paste(in1, x)))
  }), names(data))

  ## Create sublists for specified factor/character variables
  if (!is.null(sublist)) {
    ns <- sublist
    res[ns] <- lapply(ns, function(v) {
      dd <- data[[v]]
      x <- if (is.factor(dd)) levels(dd) else unique(dd)
      c(res[[v]],
        paste(in1, start, collapse=''),
        paste(in2, sprintf(item, x), sep=''),
        paste(in1, end, collapse=''))
    })
  }

  out <- c(preamble, 
    comment.char,
    form,
    paste(comment.char, start),
    unlist(res, use.names=FALSE),
    paste(comment.char, end),
    sprintf('"%s"', if (!is.null(envir)) out_name else deparse(substitute(data))))
  
  if (!missing(outfile)) {
    if (!file.exists(outfile) || file.access(outfile, mode=2) == 0L) {
      writeLines(out, con=outfile, sep="\n")
      return()
    } else {
      stop(gettextf("%s is not writeable.", outfile))
    }
  }
  out
}


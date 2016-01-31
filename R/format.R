################################################################################
##
##                  Format variable types for roxy_itemize
##
################################################################################
##' Generic formatter for roxy_itemize variable types
##'
##' @param x vector
##' @param ... passed to dispatchers
##' @export
roxy_format <- function(x, ...) UseMethod('roxy_format')

##' @export
roxy_format.default <- function(x, ...) 
  list(structure('', class='description'))

##' @export
roxy_format.numeric <- function(x, digits=3, ...) {
  if (all(is.na(x))) {
    return (list(structure('"%s" variable, but all values are NA.', 
      class='description')))
  }
  rr <- toString(round(range(x, na.rm=TRUE), digits))
  tt <- typeof(x)
  uni <- length(unique(x))
  nna <- sum(is.na(x))
  list(
    structure(
      sprintf('"%s" variable with %g unique values in the range [%s], %g NA value%s.', 
        tt, uni, rr, nna, if (nna!=1L) 's' else ''), class='description'))
}

##' @export
roxy_format.factor <- function(x, add_table=TRUE, ...) {
  hasna <- anyNA(levels(x))
  counts <- table(x, useNA='ifany')
  levs <- levels(x)
  if (hasna) {
    if (anyNA(names(counts))) 
      names(counts)[is.na(names(counts))] <- 'NA'
    levs[is.na(levs)] <- 'NA'
  }
  vals <- as.vector(counts)
  uni <- length(unique(x))
  nna <- sum(is.na(x))
  desc <- structure(
    sprintf('"factor" variable with %g unique values and %g NA values.  NA %s included as a level.',
      uni , nna, if (hasna) 'is' else 'is not'), class='description')
  if (add_table) {
    tab <- roxy_tabular(setNames(data.frame(as.list(vals)), levs))
    return ( list(desc, tab) )
  }
  list(desc)
}

##' @export
roxy_format.character <- function(x, add_table=TRUE, max_size=20, ...) {
  hasna <- anyNA(levels(x))
  uni <- length(unique(x))
  counts <- if (uni < max_size) table(x, useNA='ifany') else NA
  if (!is.na(counts) && anyNA(names(counts))) 
    names(counts)[is.na(names(counts))] <- "NA"
  vals <- as.vector(counts)
  nna <- sum(is.na(x))
  desc <- structure(
    sprintf('"character" with %g unique values and %g NA values.',
      uni, nna), class='description')
  if (!is.na(counts) && add_table) {
    tab <- roxy_tabular(setNames(data.frame(as.list(vals)), names(counts)))
    return ( list(desc, tab) )
  }
  list(desc)
}

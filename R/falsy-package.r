#' Falsy package.
#'
#' We define a set of objects as \sQuote{falsy}. Every other values are
#' \sQuote{truthy}. This is inspired by other scripting languages like
#' Python and Ruby. See the \code{\%||\%} and the \code{\%&&\%} operators
#' on how to write concise conditional code using truthy and falsy values.
#'
#' See \code{\link{is_falsy}} for the definition of what is \emph{falsy}
#' and what is \emph{truthy}.
#'
#' @name Falsy package
#' @docType package
NULL

#' A constant that is falsy
#'
#' @export
#' @examples
#' is_falsy(FALSY)
#' is_truthy(FALSY)

FALSY <- FALSE

#' A constant that is truthy.
#'
#' @export
#' @examples
#' is_truthy(TRUTHY)
#' is_falsy(FALSY)

TRUTHY <- TRUE

#' Is an object FALSY?
#'
#' Everything that is \code{FALSE} is also \emph{falsy}. Everything that is
#' \code{TRUE}, is  also \emph{truthy}. Other things are either falsy, or
#' truthy, see the details below.
#'
#' @details
#'
#' Falsy objects are:
#' \itemize{
#'  \item \code{NULL}
#'  \item \code{FALSE}
#'  \item \code{0L}, integer zero value.
#'  \item \code{0}, real zero value.
#'  \item \code{0+0i}, complex zero value.
#'  \item \code{""}, character scalar that is the empty string.
#'  \item \code{00}, one byte raw vector with zero value.
#'  \item Empty vectors. I.e. \code{logical()}, \code{integer()},
#'        \code{double()}, \code{complex()}, \code{character()}
#'        and \code{raw()}.
#'  \item Empty list.
#'  \item Object from the \code{try-error} class.
#' }
#' Everything else is truthy.
#'
#' @param object The object.
#' @return Logical scalar.
#'
#' @export
#' @examples
#' ## NULL is falsy
#' is_falsy(NULL)
#'
#' ## So is the empty string
#' is_falsy("")
#'
#' ## But the zero string is not
#' is_falsy("0")
#'
#' ## Clasess are truthy
#' is_falsy(factor())
#'
#' ## Errors reported by try() are falsy
#' is_falsy(try(a + "string", silent = TRUE))

is_falsy <- function(object) {
  is.null(object) ||
    identical(object, FALSE) ||
    identical(object, 0L) ||
    identical(object, 0.0) ||
    identical(object, 0+0i) ||
    identical(object, "") ||
    identical(object, as.raw(0)) ||
    identical(object, logical()) ||
    identical(object, integer()) ||
    identical(object, double()) ||
    identical(object, complex()) ||
    identical(object, character()) ||
    identical(object, raw()) ||
    identical(object, list()) ||
    inherits(object, "try-error")
}

#' Is an object TRUTHY?
#'
#' @param object The object.
#'
#' @export
#' @examples
#' ## Non-zero numbers are truthy
#' is_truthy(1)
#'
#' ## So are non-empty vectors that are not the zero scalar
#' is_truthy(c(1))
#'
#' ## Matrices are truthy, even if they are empty,
#' ## because they are a class
#' is_truthy(matrix(nrow=0, ncol=0))
#'
#' ## But empty vectors are falsy
#' is_truthy(numeric())

is_truthy <- function(object) {
  ! is_falsy(object)
}

#' Robust 'or' operator working with truthy and falsy values.
#'
#' @rdname or
#' @name or
#' @param lhs Left hand side, always evaluated.
#' @param rhs Right hand side, only evaluated if \code{lhs}
#' evaluates to FALSY.
#'
#' @export
#' @examples
#'
#' \donttest{
#' ## Message if list is empty
#' l <- list(1,2,3)
#' l %||% message("list is empty")
#'
#' l <- list()
#' l %||% message("list is empty")
#' }

`%||%` <- function(lhs, rhs) {
  lres <- withVisible(eval(lhs, envir = parent.frame()))
  if (is_falsy(lres$value)) {
    eval(rhs, envir = parent.frame())
  } else {
    if (lres$visible) { lres$value } else { invisible(lres$value) }
  }
}

#' Robust 'and' operator working with truthy and falsy values.
#'
#' @rdname and
#' @name and
#' @param lhs Left hand side, always evaluated.
#' @param rhs Right hand side, evaluated only if \code{lhs}
#' evaluates to TRUTHY.
#'
#'@export
#' @examples
#'
#' ## Shift to zero
#' v <- 5:10
#' v %&&% { v <- v - min(v) }
#' v
#'
#' ## It works if the vector is empty
#' v <- numeric()
#' v %&&% { v <- v - min(v) }
#' v

`%&&%` <- function(lhs, rhs) {
  lres <- withVisible(eval(lhs, envir = parent.frame()))
  if (is_truthy(lres$value)) {
    eval(rhs, envir = parent.frame())
  } else {
    if (lres$visible) { lres$value } else { invisible(lres$value) }
  }
}

#' Negate the falsyness of a value.
#'
#' @param rhs Value. If it is truthy, a falsy value (\code{FALSY}) is
#' returned, otherwise a truthy value (\code{TRUTHY}) is returned.
#' @return Logical scalar.
#'
#' @export
#' @examples
#' nay(FALSY)
#' nay(TRUTHY)
#'
#' ## Check if directory is empty
#' tmp <- tempdir()
#' nay(dir(tmp, all.files = TRUE, no.. = TRUE)) %||% message("Not empty")
#'
#' cat("Hello!", file = tempfile())
#' nay(dir(tmp, all.files = TRUE, no.. = TRUE)) %||% message("Not empty")

nay <- function(rhs) {
  if (is_falsy(rhs)) { TRUTHY } else { FALSY }
}

#' Try quietly
#'
#' We just call \code{try} and set its \code{silent} argument to
#' \code{TRUE}.
#'
#' @param expr The R expression to evaluate.
#'
#' @export

try_quietly <- function(expr) {
  try(expr, silent = TRUE)
}

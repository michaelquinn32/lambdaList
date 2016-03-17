#' Check whether a list is empty
#'
#' This function is part of the \link[=lambdaList]{List out of Lambda} set of functions.
#' Like \code{\link{head}} and \code{\link{tail}}, this uses an anoymous function to
#' return a particular component of a functional list. Here, that's the empty status
#' of the functional list.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#'
#' @return The empty status of a functional list
#'
#' @export

is_empty <- function(ls) {
    ls(function(h, t, e) e)
}
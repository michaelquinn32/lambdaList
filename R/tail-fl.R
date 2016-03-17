#' Access the tail component of a function list object
#'
#' This function is part of the \link[=lambdaList]{List out of Lambda} set of functions.
#' Like \code{\link{head}} and \code{\link{is_empty}}, this uses an anoymous function to
#' return a particular component of a functional list. Here, that's the tail argument.
#'
#' @param x A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param ... Additional arguments to \code{tail} (for argument consistency)
#'
#' @return The tail argument of a functional list
#'
#' @importFrom utils tail
#' @export

tail.fl <- function(x, ...) {
    x(function(h, t, e) t)
}
#' Reduce
#'
#' Recursively fold a function across a list, yielding a scalar.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A binary function, i.e. a function that takes two arguments
#' @param ... Additional arguments to \code{fn}
#'
#' @return A scalar
#'
#' @seealso \code{\link[base]{Reduce}}
#'
#' @export

reduce <- function(ls, fn, ...) {
    if (is_empty(tail(ls))) head(ls)
    else fn(head(ls), reduce(tail(ls), fn, ...), ...)
}
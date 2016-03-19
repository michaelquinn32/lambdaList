#' Reduce
#'
#' Recursively fold a function across a list, yielding a scalar.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A binary function, i.e. a function that takes two arguments
#' @param init The initial value for the recursive
#' @param ... Additional arguments to \code{fn}
#'
#' @return A scalar
#'
#' @seealso \code{\link[base]{Reduce}}
#'
#' @export

reduce <- function(ls, fn, init = 0, ...) {
    if (is_empty(ls)) init
    else reduce(tail(ls), fn, fn(init, head(ls)))
}


#' @describeIn reduce Reduce the list from the other direction
#' @export

reduce_right <- function(ls, fn, init = 0, ...) {
    if (is_empty(ls)) init
    else reduce_right(tail(ls), fn, fn(head(ls), init), ...)
}

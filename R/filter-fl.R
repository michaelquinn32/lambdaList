#' Return values that fulfill a predicate function
#'
#' A predicate function is one that takes a single argument returns either true or
#' false. \code{filter} takes such a function, applies it to a functional list,
#' and keeps those values that return true in the predicate.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A predicate function, as described above
#' @param ... Additional arguments to \code{fn}
#'
#' @return A functional list whose data elements return \code{true} in the predicate
#' function.
#'
#' @seealso \code{\link{remove}}, \code{\link[base]{Filter}}
#'
#' @export

filter.fl <- function(ls, fn, ...) {
    if (is_empty(ls)) empty_list
    else if (fn(head(ls), ...)) prepend(filter(tail(ls), fn, ...), head(ls))
    else filter(tail(ls), fn, ...)
}

#' @describeIn filter Preserve original function
#' @importFrom stats filter
#' @export

filter.default <- filter

#' New generic filter to keep both versions
#'
#' @param ... Arguments to different versions of filter
#'
#' @seealso \code{\link[stats]{filter}}
#'
#' @export

filter <- function(...) {
    UseMethod('filter')
}

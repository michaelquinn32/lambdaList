#' Apply a function to a list
#'
#' One of the most obvious extensions of a functional list is a tool to iterate
#' over that "object."
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A function be applied to each of the scalars contained in the arguments
#' of the functional list object
#' @param ... Additional arguments to \code{fn}
#'
#' @return A functional list of the same length as \code{ls}, where each new element
#' is equal to \code{fn(x)} of each scalar contained in \code{ls}
#'
#' @seealso \code{\link[base]{lapply}}, \code{\link[base]{Map}}
#'
#' @export

map <- function(ls, fn, ...) {
    if (is_empty(ls)) empty_list
    else prepend(map(tail(ls), fn, ...), fn(head(ls), ...))
}
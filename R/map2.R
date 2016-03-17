#' Two-list map
#'
#' Apply a two-argument function to two lists. This can be used to implement
#' vectorized versions of common functions.
#'
#' @param lsa A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param lsb A function, with \emph{class} \code{fl}, that operates as a functional
#' list object. This needs to be the same length as \code{lsa}
#' @param fn A function be applied to each of the scalars contained in the arguments
#' of the functional list object
#' @param ... Additional arguments to \code{fn}
#'
#' @return A functional list of the same length as \code{lsa} and \code{lsb}, where each new element
#' is equal to \code{fn(x)} of each scalar contained in \code{ls}
#'
#' @export

map2 <- function(lsa, lsb, fn, ...) {
    if (is_empty(lsa)) empty_list
    else prepend(map2(tail(lsa), tail(lsb), fn, ...), fn(head(lsa), head(lsb), ...))
}
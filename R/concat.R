#' Combine two lists into one
#'
#' @param lsa A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param lsb A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#'
#' @return A closure with the \code{fl} class added. The closure can be used to access
#' different properties of the list: either the head, tail or its empty status.
#'
#' @seealso \code{\link[base]{c}}
#'
#' @export

concat <- function(lsa, lsb) {
    if (is_empty(lsa)) lsb
    else prepend(concat(tail(lsa), lsb), head(lsa))
}
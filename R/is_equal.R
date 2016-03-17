#' Test if two functional lists are equal
#'
#' @param lsa A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param lsb A function, with \emph{class} \code{fl}, that operates as a functional
#' list object. This needs to be the same length as \code{lsa}
#'
#' @return A logical: TRUE or FALSE
#'
#' @export

is_equal <- function(lsa, lsb) {
    if (is_empty(lsa) && is_empty(lsb)) TRUE
    else if (length(lsa) != length(lsb)) FALSE
    else if (head(lsa) != head(lsb)) FALSE
    else is_equal(tail(lsa), tail(lsb))
}
#' Find the length of a list
#'
#' Here, the length equals the number of scalar data objects that it contations.
#'
#' @param x A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#'
#' @return A scalar equal to the number of scalars contained in the functional list.
#'
#' @export

length.fl <- function(x) {
    if (is_empty(x)) 0
    else length(tail(x)) + 1
}
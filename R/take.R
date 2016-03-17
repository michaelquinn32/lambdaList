#' Keep all elements up to but not including n
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0 and continues up to, but not
#' including the \emph{nth} number.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param n An integer referring the cardinal position of a scalar in the
#' functional list object
#'
#' @return A functional list containing all of the scalars before the \emph{nth}
#' position
#'
#' @export

take <- function(ls, n) {
    if (n <= 0) empty_list
    else prepend(take(tail(ls), n - 1), head(ls))
}
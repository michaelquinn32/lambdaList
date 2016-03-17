#' Select an element of a functional list
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param n An integer referring the cardinal position of a scalar in the
#' functional list object
#'
#' @return A scalar contained in the functional list object.
#'
#' @export

nth <- function(ls, n) {
    if (n <= 0) head(ls)
    else nth(tail(ls), n - 1)
}
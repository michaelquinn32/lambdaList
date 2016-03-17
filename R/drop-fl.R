#' Drop all elements before nth element in a list
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0 and continues up to, but not
#' including the \emph{nth} number.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param n An integer referring the cardinal position of a scalar in the
#' functional list object
#' @param ... Used for S3 consistency
#'
#' @return A functional list containing all of the scalars including and after
#' the \emph{nth} position
#'
#' @export

drop.fl <- function(ls, n) {
    if (n <= 0) ls
    else drop(tail(ls), n - 1)
}


#' @describeIn drop Preserve original version
#' @export

drop.default <- drop


#' New Drop Generic
#'
#' So that we can have both the original and new version in our package
#'
#' @param ... Arguments to various versions of \code{drop}
#'
#' @seealso \code{\link[base]{drop}}
#' @export

drop <- function(...) {
    UseMethod('drop')
}

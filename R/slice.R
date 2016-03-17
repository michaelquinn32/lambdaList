#' Select all of the elements between two positions in a list
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0 and continues up to, but not
#' including the \emph{nth} number.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param start An integer referring the cardinal position of the first element in the slice
#' @param end An integer referring to the position after the final element in the slice
#'
#' @return A functional list containing all of the scalars from the \code{start}
#' position but before the \code{end} position
#'
#' @export

slice <- function(ls, start, end) {
    take(drop(ls, start), end - start)
}
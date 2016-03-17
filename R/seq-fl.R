#' Create a sequence of integers
#'
#' This function generates a functional list that generates a sequence of
#' integers in a manner similar to \code{range} in \code{Python}.
#'
#' @param empty An empty functional list, used to take advance of S3
#' @param start An integer specifying the begin of the sequence
#' @param end An integer that is the end of the sequence (but not included)
#'
#' @return A functional list that contains the sequence of integers
#'
#' @export

seq.fl <- function(empty, start, end) {
    if (end <= start) empty
    else prepend(seq(empty, start + 1, end), start)
}
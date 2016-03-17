#' Append adds an element to the end of the list.
#'
#' This is the more useful function from a user's perspective, since the
#' organization of the arguments makes it easy to pipe several calls of \code{append}
#' together. This function combines recursion and the function \code{\link{prepend}}
#' to make sure the base \code{\link{empty_list}} stays at the end of the list.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param el A data element (from R's standard data types) to add to the list
#' @param ... Used for S3 consistency
#'
#' @return A closure with the \code{fl} class added. The closure can be used to access
#' different properties of the list: either the head, tail or its empty status.
#'
#' @seealso \code{\link[base]{append}}
#'
#' @export

append.fl <- function(ls, el) {
    if (is_empty(ls)) prepend(ls, el)
    else prepend(append(tail(ls), el), head(ls))
}

#' @describeIn append Preserve Original Function
#' @export

append.default <- append

#' New Append Generic
#'
#' So that we can have both the original and new version in our package
#'
#' @param ... Arguments for different versions of \code{append}
#'
#' @seealso \code{\link[base]{append}}
#'
#' @export

append <- function(...) {
    UseMethod('append')
}

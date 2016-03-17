#' Access the head of a functional list
#'
#' Our first selector. Like \code{\link{tail}} and \code{\link{is_empty}},
#' it uses an anonymous function that returns a particular component of the functional
#' list. Here that component is the functional list's \emph{head}.
#'
#' @param x A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param ... Additional arguments to \code{head} (for argument consistency)
#'
#' @return The element stored in the head argument of the functional list
#'
#' @importFrom utils head
#' @export

head.fl <- function(x, ...) {
    x(function(h, t, e) h)
}
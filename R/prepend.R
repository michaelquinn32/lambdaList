#' Prepend adds an element to the list through recursion.
#'
#' This is the primary list building tool. Although the user might usually prefer
#' to work with \code{\link{append}}, it is incredibly useful to use \code{prepend}
#' in other functions that work on functional lists. In fact, we need this function
#' to implement \code{\link{append}}.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param el A data element (from R's standard data types) to add to the list
#'
#' @return A closure with the \code{fl} class added. The closure can be used to access
#' different properties of the list: either the head, tail or its empty status.
#'
#' @export

prepend <- function(ls, el) {
    structure(function(selector) selector(el, ls, FALSE), class = c('fl', 'function'))
}
#' Construct an empty list
#'
#' This is a base "object" for building lists.
#'
#' To quote Steve Losh, a list is a function
#' that knows how to return its head, tail and whether it's empty. Empty lists serve an
#' important part of this construction, since it gives us the ability to mark the
#' "end" of our nested functions that create the list. In practice, we don't usually
#' call the function with its argument. Instead, other functions will need to access
#' this argument to implement various properties of lists.
#'
#' @param selector A function to return either the head, tail or empty status of a list
#' @return A closure with the \code{fl} class added. The closure can be used to access
#' different properties of the list: either the head, tail or its empty status.
#'
#' @export

empty_list <- function(selector) {
    selector(NULL, NULL, TRUE)
}

empty_list <- structure(empty_list, class = c('fl', 'function'))

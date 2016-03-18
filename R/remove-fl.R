#' Return values that don't fulfill a predicate function
#'
#' A predicate function is one that takes a single argument returns either true or
#' false. \code{remove} takes such a function, applies it to a functional list,
#' and keeps those values that return false in the predicate.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A predicate function, as described above
#' @param ... Additional arguments to remove, for S3 consistency
#'
#' @return A functional list whose data elements return \code{false} in the predicate
#' function.
#'
#' @seealso \code{\link{filter}}, \code{\link[base]{Filter}}
#'
#' @export

remove.fl <- function(ls, fn, ...) {
    if (is_empty(ls)) empty_list
    else if (fn(head(ls), ...)) remove(tail(ls), fn)
    else prepend(remove(tail(ls), fn, ...), head(ls))
}


#' @describeIn remove Preserve original function
#' @export

remove.default <- remove


#' New Remove Generic
#'
#' So that we can have both the original and new version in our package
#'
#' @param ... Arguments to different versions of \code{remove}
#'
#' @seealso \code{\link[base]{remove}}
#'
#' @export

remove <- function(...) {
    UseMethod("remove")
}

#' Return the first object in a list that fulfills a predicate
#'
#' A predicate function is one that takes a single argument returns either true or
#' false. \code{find} takes such a function, applies it to a functional list,
#' and returns the first object where the function returns \code{TRUE}.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A predicate function, as described above
#' @param ... Additional arguments to remove, for S3 consistency
#'
#' @return Either the first object that fulfills the predicate or \code{FALSE}.
#'
#' @seealso  \code{\link[base]{funprog}}
#' @importFrom utils find
#' @export

find.fl <- function(ls, fn, ...) {
    if (is_empty(ls)) FALSE
    else if (fn(head(ls), ...)) head(ls)
    else find(tail(ls), fn)
}

#' @describeIn find Preserve original function
#' @export

find.default <- find

#' New Find Generic
#'
#' So that we can have both the original and new version in our package
#'
#' @param ... Arguments to different versions of \code{find}
#'
#' @seealso \code{\link[utils]{find}}
#'
#' @export

find <- function(...) {
    UseMethod('find')
}


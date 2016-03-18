#' Return the position of the first object in a list that fulfills a predicate
#'
#' A predicate function is one that takes a single argument returns either true or
#' false. \code{position} takes such a function, applies it to a functional list,
#' and returns the position of first object where the function returns \code{TRUE}.
#'
#' Remember, in this package we use cardinal numbers, counting from 0.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A predicate function, as described above
#' @param start A index of the list to begin the search (remember, cardinal numbers)
#'
#' @return Either the first object that fulfills the predicate or \code{FALSE}.
#'
#' @seealso  \code{\link[base]{funprog}}
#' @export

position <- function(ls, fn, start = 0) {
    if (start >= length(ls)) FALSE
    else if (fn(nth(ls, start))) start
    else position(ls, fn, start + 1)
}

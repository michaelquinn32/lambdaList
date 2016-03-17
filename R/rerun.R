#' Repeatedly execute a quoted R expression
#'
#' @param n An integer that provides the desired number of executions
#' @param expr A quoted R expression to be iteratively executed
#'
#' @return A functional list of length \code{n}, where each element
#' represents one execution of \code{expr}
#'
#' @examples
#' rerun(10, quote(rnorm(1)))
#'
#' @seealso \link[base]{replicate}
#'
#' @export

rerun <- function(n, expr) {
    if (n <= 0) empty_list
    else prepend(rerun(n - 1, expr), eval(expr))
}
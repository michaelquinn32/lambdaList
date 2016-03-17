#' Print the contents of a functional list
#'
#' @param x A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param ... Other arguments to print (for argument consistency)
#'
#' @method print fl
#' @export

print.fl <- function(x, ...) {
    if (is_empty(x)) {
        cat("")

    } else {
        cat(head(x), "")
        print(tail(x))
    }
}

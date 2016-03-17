#' List out of lambda
#'
#' The following scripts contains an R version of the classic article
#' \emph{List out of Lambda} by Steve Losh.
#' \href{http://stevelosh.com/blog/2013/03/list-out-of-lambda/}{The article can be found here},
#' and I strongly recommend reading the article before scanning these functions.
#'
#' In the following functions, we implement lists without creating an actual data
#' structure. Instead, we use pure functional programming. In the spirit of the
#' article, we also try to use a minimum number of elements in the R language.
#' The focus is in the implementation of the functions, recursion and interesting
#' applications of conditional logic.
#'
#' This package also contains a variety of functional programming tools
#' to work with these new functional lists. In that regard, it draws heavily on
#' Hadley Wickham's \href{https://github.com/hadley/purrr}{purrr package}.
#'
#' A bit of a caveat: This is just an illustration of functional programming. It
#' overwrites some base function in R, and it
#' abuses the S3 object oriented programming system, which probably wasn't designed
#' to add classes to functions. After all, R has actual objects, so there is
#' never a reason to do this in practice. Nonetheless, it illustrates some of the
#' more interesting aspects of R, which probably excuses the shortcuts it contains.
#'
#' More than a set of working functions, this exercise is meant to be another
#' realization of Steve's fantastic final recommendation. As he says, "Remember: the point
#' is not to create something that runs well on a physical computer. Instead of
#' thinking about how to make a particular combination of transistors and circuits
#' have the right voltages, think about 'computing' in the beautiful, perfect,
#' abstract sense."
#'
#' @name lambdaList
"_PACKAGE"

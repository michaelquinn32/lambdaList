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


# List essentials ---------------------------------------------------------

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


#' Access the tail component of a function list object
#'
#' This function is part of the \link[=lambdaList]{List out of Lambda} set of functions.
#' Like \code{\link{head}} and \code{\link{is_empty}}, this uses an anoymous function to
#' return a particular component of a functional list. Here, that's the tail argument.
#'
#' @param x A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param ... Additional arguments to \code{tail} (for argument consistency)
#'
#' @return The tail argument of a functional list
#'
#' @importFrom utils tail
#' @export

tail.fl <- function(x, ...) {
    x(function(h, t, e) t)
}


#' Check whether a list is empty
#'
#' This function is part of the \link[=lambdaList]{List out of Lambda} set of functions.
#' Like \code{\link{head}} and \code{\link{tail}}, this uses an anoymous function to
#' return a particular component of a functional list. Here, that's the empty status
#' of the functional list.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#'
#' @return The empty status of a functional list
#'
#' @export

is_empty <- function(ls) {
    ls(function(h, t, e) e)
}



# List Builders -----------------------------------------------------------

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


#' @describeIn append Preserve Original Function
#'
#' @export

append.default <- base::append


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
    if (is_empty(ls)) prepend(empty_list, el)
    else prepend(append(tail(ls), el), head(ls))
}


#' Combine two lists into one
#'
#' @param lsa A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param lsb A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#'
#' @return A closure with the \code{fl} class added. The closure can be used to access
#' different properties of the list: either the head, tail or its empty status.
#'
#' @seealso \code{\link[base]{c}}
#'
#' @export

concat <- function(lsa, lsb) {
    if (is_empty(lsa)) lsb
    else prepend(concat(tail(lsa), lsb), head(lsa))
}


# Helper Functions --------------------------------------------------------

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


#' Find the length of a list
#'
#' Here, the length equals the number of scalar data objects that it contations.
#'
#' @param x A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#'
#' @return A scalar equal to the number of scalars contained in the functional list.
#'
#' @method length fl
#' @export

length.fl <- function(x) {
    if (is_empty(x)) 0
    else length(tail(x)) + 1
}


#' Test if two functional lists are equal
#'
#' @param lsa A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param lsb A function, with \emph{class} \code{fl}, that operates as a functional
#' list object. This needs to be the same length as \code{lsa}
#'
#' @return A logical: TRUE or FALSE
#'
#' @export

is_equal <- function(lsa, lsb) {
    if (is_empty(lsa) && is_empty(lsb)) TRUE
    else if (length(lsa) != length(lsb)) FALSE
    else if (head(lsa) != head(lsb)) FALSE
    else is_equal(tail(lsa), tail(lsb))
}


# Subsetting --------------------------------------------------------------

#' Select an element of a functional list
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param n An integer referring the cardinal position of a scalar in the
#' functional list object
#'
#' @return A scalar contained in the functional list object.
#'
#' @export

nth <- function(ls, n) {
    if (n <= 0) head(ls)
    else nth(tail(ls), n - 1)
}


#' @describeIn drop Preserve original version
#' @export

drop.default <- base::drop


#' New Drop Generic
#'
#' So that we can have both the original and new version in our package
#'
#' @param ... Arguments to various versions of \code{drop}
#'
#' @seealso \code{\link[base]{drop}}
#' @export

drop <- function(...) {
    UseMethod('drop')
}

#' Drop all elements before nth element in a list
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0 and continues up to, but not
#' including the \emph{nth} number.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param n An integer referring the cardinal position of a scalar in the
#' functional list object
#' @param ... Used for S3 consistency
#'
#' @return A functional list containing all of the scalars including and after
#' the \emph{nth} position
#'
#' @export

drop.fl <- function(ls, n) {
    if (n <= 0) ls
    else drop(tail(ls), n - 1)
}


#' Keep all elements up to but not including n
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0 and continues up to, but not
#' including the \emph{nth} number.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param n An integer referring the cardinal position of a scalar in the
#' functional list object
#'
#' @return A functional list containing all of the scalars before the \emph{nth}
#' position
#'
#' @export

take <- function(ls, n) {
    if (n <= 0) empty_list
    else prepend(take(tail(ls), n - 1), head(ls))
}


#' Select all of the elements between two positions in a list
#'
#' Unlike base R, this subsetting function uses cardinal instead of ordinal
#' numbers. This means that counting starts from 0 and continues up to, but not
#' including the \emph{nth} number.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param start An integer referring the cardinal position of the first element in the slice
#' @param end An integer referring to the position after the final element in the slice
#'
#' @return A functional list containing all of the scalars from the \code{start}
#' position but before the \code{end} position
#'
#' @export

slice <- function(ls, start, end) {
    take(drop(ls, start), end - start)
}

# Functional Programming --------------------------------------------------

#' Apply a function to a list
#'
#' One of the most obvious extensions of a functional list is a tool to iterate
#' over that "object."
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A function be applied to each of the scalars contained in the arguments
#' of the functional list object
#' @param ... Additional arguments to \code{fn}
#'
#' @return A functional list of the same length as \code{ls}, where each new element
#' is equal to \code{fn(x)} of each scalar contained in \code{ls}
#'
#' @seealso \code{\link[base]{lapply}}, \code{\link[base]{Map}}
#'
#' @export

map <- function(ls, fn, ...) {
    if (is_empty(ls)) empty_list
    else prepend(map(tail(ls), fn, ...), fn(head(ls), ...))
}


#' Two-list map
#'
#' Apply a two-argument function to two lists. This can be used to implement
#' vectorized versions of common functions.
#'
#' @param lsa A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param lsb A function, with \emph{class} \code{fl}, that operates as a functional
#' list object. This needs to be the same length as \code{lsa}
#' @param fn A function be applied to each of the scalars contained in the arguments
#' of the functional list object
#' @param ... Additional arguments to \code{fn}
#'
#' @return A functional list of the same length as \code{lsa} and \code{lsb}, where each new element
#' is equal to \code{fn(x)} of each scalar contained in \code{ls}
#'
#' @export

map2 <- function(lsa, lsb, fn, ...) {
    if (is_empty(lsa)) empty_list
    else prepend(map2(tail(lsa), tail(lsb), fn, ...), fn(head(lsa), head(lsb), ...))
}


#' Reduce
#'
#' Recursively fold a function across a list, yielding a scalar.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A binary function, i.e. a function that takes two arguments
#' @param ... Additional arguments to \code{fn}
#'
#' @return A scalar
#'
#' @seealso \code{\link[base]{Reduce}}
#'
#' @export

reduce <- function(ls, fn, ...) {
    if (is_empty(tail(ls))) head(ls)
    else fn(head(ls), reduce(tail(ls), fn, ...), ...)
}

#' @describeIn filter Preserve original function
#' @importFrom stats filter
#' @export

filter.default <- filter

#' New generic filter to keep both versions
#'
#' @param ... Arguments to different versions of filter
#'
#' @seealso \code{\link[stats]{filter}}
#'
#' @export

filter <- function(...) {
    UseMethod('filter')
}


#' Return values that fulfill a predicate function
#'
#' A predicate function is one that takes a single argument returns either true or
#' false. \code{filter} takes such a function, applies it to a functional list,
#' and keeps those values that return true in the predicate.
#'
#' @param ls A function, with \emph{class} \code{fl}, that operates as a functional
#' list object
#' @param fn A predicate function, as described above
#' @param ... Additional arguments to \code{fn}
#'
#' @return A functional list whose data elements return \code{true} in the predicate
#' function.
#'
#' @seealso \code{\link{remove}}, \code{\link[base]{Filter}}
#'
#' @export

filter.fl <- function(ls, fn, ...) {
    if (is_empty(ls)) {
        empty_list

    } else if (fn(head(ls), ...)) {
        prepend(filter(tail(ls), fn, ...), head(ls))

    } else {
        filter(tail(ls), fn, ...)
    }
}

#' @describeIn remove Preserve original function
#' @export

remove.default <- base::remove


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
    if (is_empty(ls)) {
        empty_list

    } else if (fn(head(ls))) {
        remove(tail(ls), fn)

    } else {
        prepend(remove(tail(ls), fn), head(ls))
    }
}


# List Builders -----------------------------------------------------------

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


#' Create a sequence of integers
#'
#' This function generates a functional list that generates a sequence of
#' integers in a manner similar to \code{range} in \code{Python}.
#'
#' @param empty An empty functional list, used to take advance of S3
#' @param start An integer specifying the begin of the sequence
#' @param end An integer that is the end of the sequence (but not included)
#'
#' @return A functional list that contains the sequence of integers
#'
#' @export

seq.fl <- function(empty, start, end) {
    if (end <= start) empty
    else prepend(seq(empty, start + 1, end), start)
}

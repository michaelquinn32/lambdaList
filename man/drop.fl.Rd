% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop-fl.R
\name{drop.fl}
\alias{drop.fl}
\title{Drop all elements before nth element in a list}
\usage{
\method{drop}{fl}(ls, n)
}
\arguments{
\item{ls}{A function, with \emph{class} \code{fl}, that operates as a functional
list object}

\item{n}{An integer referring the cardinal position of a scalar in the
functional list object}

\item{...}{Used for S3 consistency}
}
\value{
A functional list containing all of the scalars including and after
the \emph{nth} position
}
\description{
Unlike base R, this subsetting function uses cardinal instead of ordinal
numbers. This means that counting starts from 0 and continues up to, but not
including the \emph{nth} number.
}


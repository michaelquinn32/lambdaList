# An R Translation of List Out of Lambda

This package translates Steve Losh's amazing article, [List Out of Lambda](http://stevelosh.com/blog/2013/03/list-out-of-lambda/) into R. Like the original article, it builds lists using only functions and then expands on these "objects" with a variety of functional programming tools. As far as the latter are concerned, the package draws considerably from Hadley Wickham's [`purrr` package](https://github.com/hadley/purrr). It can be mostly thought of as a thought experiment, since it does not meet performance or usability standards of traditional R objects.

Then again, that's not really the point either. To quote Steve,

> Remember: the point is not to create something that runs well on a physical computer. Instead of thinking about how to make a particular combination of transistors and circuits have the right voltages, think about “computing” in the beautiful, perfect, abstract sense.

An explanation of the functional lists and their surprising applications [can be found here](http://michaelquinn32.github.io/list-out-of-lambda-in-R/).

## Installation

Again, this package is only an exercise. But that doesn't mean that it isn't fun to play with. If you'd like to try it out, run:

```{r }
# install.packages("devtools")
devtools::install_github('michaelquinn32/lambdaList')
```
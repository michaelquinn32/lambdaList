# Tests for the lambdaList package

library(magrittr)

# Empty Lists -------------------------------------------------------------

context('Empty Lists')

test_that('Empty lists return the appropriate components', {
    expect_null(head(empty_list))
    expect_null(tail(empty_list))
    expect_true(is_empty(empty_list))
})

test_that('Empty lists contain the appropriate attributes', {
    expect_is(empty_list, 'function')
    expect_is(empty_list, 'fl')
})


# Constructed lists -------------------------------------------------------

context('Constructed Lists')

my_list <- prepend(empty_list, 1)

test_that('Prepended lists return the appropriate components', {
    expect_equal(head(my_list), 1)
    expect_true(is_empty(tail(my_list)))
    expect_false(is_empty(my_list))
})

another_list <- append(my_list, 2)

test_that('Appended lists return the appropriate components', {
    expect_equal(head(another_list), 1)
    expect_equal(head(tail(another_list)), 2)
    expect_false(is_empty(another_list))
})


# List Concatenation ------------------------------------------------------

context('Concatted Lists')

cat_list <- concat(another_list, my_list)

test_that('Appended lists return the appropriate components', {
    expect_equal(head(cat_list), 1)
    expect_false(is_empty(another_list))
})


# List equality -----------------------------------------------------------

context('Testing is_equal')

test_list <- empty_list %>% append(1) %>% append(2) %>% append(1)

test_that('is_equal covers all cases', {
    expect_true(is_equal(empty_list, empty_list))
    expect_true(is_equal(cat_list, test_list))
    expect_false(is_equal(cat_list, another_list))
    expect_false(is_equal(cat_list, 'a'))
})


# List Properties ---------------------------------------------------------

context('List Properties')

test_that('Constructed lists contain the appropriate attributes', {
    expect_is(my_list, 'function')
    expect_is(my_list, 'fl')
    expect_is(another_list, 'function')
    expect_is(another_list, 'fl')
    expect_is(cat_list, 'function')
    expect_is(cat_list, 'fl')
})


# List Length -------------------------------------------------------------

context('List length')

test_that('Lists have the appropriate length', {
    expect_equal(length(empty_list), 0)
    expect_equal(length(my_list), 1)
    expect_equal(length(another_list), 2)
    expect_equal(length(cat_list), 3)
})


# Subsetting --------------------------------------------------------------

context('List subsetting')

test_that('Subsets return the correct elements', {
    expect_null(nth(empty_list, 0))
    expect_null(nth(empty_list, 1))
    expect_equal(nth(my_list, 0), 1)
    expect_null(nth(my_list, 1))
    expect_true(is_equal(my_list, take(another_list, 1)))
    expect_false(is_equal(my_list, drop(another_list, 1)))
    expect_true(is_equal(another_list, take(cat_list, 2)))
    expect_true(is_equal(another_list, slice(cat_list, 0, 2)))
})


# Functional Programming --------------------------------------------------

context('Functional programming')

times_2 <- function(x) x * 2
is_odd <- function(x) x %% 2 != 0
answer_list <- empty_list %>% append(2) %>% append(4)

test_that('Functional programming operators return correct results', {
    expect_true(is_equal(map(another_list, times_2), answer_list))
    expect_equal(filter(another_list, is_odd) %>% head, 1)
    expect_equal(remove(another_list, is_odd) %>% head, 2)
    expect_true(is_equal(map2(another_list, another_list, `+`), answer_list))
    expect_equal(reduce(another_list, `+`), 3)
    expect_false(find(empty_list, is_odd))
    expect_false(find(answer_list, is_odd))
    expect_equal(find(test_list, is_odd), 1)
    expect_false(position(empty_list, is_odd))
    expect_false(position(answer_list, is_odd))
    expect_equal(position(test_list, is_odd), 0)
})


# List Builders -----------------------------------------------------------

context('List constructors')

my_sequence <- seq(empty_list, 1, 3)

test_that('List constructors work as expected', {
    expect_true(is_equal(my_sequence, another_list))
})

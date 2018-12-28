
# Tests --------

#' @name class-tests
#' @title Enhanced Class Tests
#' @description
#' These tests allow for mapped and enhanced tests regarding class.
#'
#' @inheritParams testthat::expect_is
#' @param object An object to test
#' @param lst A list of objects to test
#' @param class The class object is to be, or classes it is allowed to be.
#'
#' @family class
#' @example inst/examples/example-class-tests.R
NULL

#' @describeIn class-tests Check if all elements of a list are or inherit from the given class.
#' Uses [base::inherits()] to check inheritance.
all_inherit <- function(lst, class, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.character(class) || is.null(class) )
    if (all(. <- purrr::map_lgl(lst, inherits, what=class, which=FALSE)))
        return(TRUE)
    msg <- if (sum(!.) > 1L) {
        ._("%s has bad elements at %s which do not inherit from %s."
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(class), sep2 = ' or ', sep.last = ' or ')
          ) } else {
        bad.class <- purrr::map_chr(lst[!.], class0)
        ._("%s has bad element at %s which does not inherit from %s. It is a %s"
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(class), sep2 = ' or ', sep.last = ' or ')
          , dQuote(bad.class)
          )
          }
    return(structure(FALSE, msg=msg, bad.elements = which(!.)))
}
if(FALSE){#@testing
    l <- list( 'a', 'b', 'c'
             , 1, 2
             , function()"hello world"
             )
    expect_identical( validate_that(all_inherit(l, 'character'))
                    , "`l` has bad elements at 4, 5, and 6" %<<%
                      "which do not inherit from" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_inherit(l, c('character', 'function')))
                    , "`l` has bad elements at 4 and 5" %<<%
                      "which do not inherit from" %<<%
                      dQuote("character") %<<% 'or' %<<%
                      dQuote("function") %<<<% '.')
    expect_identical( validate_that(all_inherit(l, c('character', 'numeric')))
                    , "`l` has bad element at 6" %<<%
                      "which does not inherit from" %<<%
                      dQuote("character") %<<% 'or' %<<%
                      dQuote("numeric") %<<<% '.' %<<%
                      "It is a" %<<% dQuote("function"))

    expect_true( all_inherit(list(1L, 2L, 3L), 'integer'))
}

#' @describeIn class-tests [methods::is] mapped over a vector.
#'     Similar to `all_inherit` but uses [methods::is()] for test.
#'     This manifests in S4 Virtual classes such as the 'ANY' class
are <- function(lst, class){
    purrr::map_lgl(lst, is, class)
}
if(FALSE){#@testing
    lst <- list('a', 1L, TRUE)

    expect_true(all(are(lst, 'ANY')))
    expect_identical(are(lst, 'character'), c(T,F,F))
    expect_identical(are(lst, 'integer'), c(F,T,F))
    expect_identical(are(lst, 'numeric'), c(F,T,F))
}

#' @describeIn class-tests Test that an object is exactly a class; excludes inheritance.
is_exactly <- function(object, class){any(inherits(object, what=class, which=TRUE)==1)}
if(FALSE){
    x <- Rd_text("text")
    expect_true(is_exactly(x, 'Rd_TEXT'))
    expect_true(is_exactly(x, c('Rd_RCODE', 'Rd_TEXT')))
    expect_false(is_exactly(Rd(x), c('Rd_RCODE', 'Rd_TEXT')))

    docs <- function_documentation()

    expect_true(is_exactly(docs, 'function-Documentation'))
    expect_false(is_exactly(docs, 'Documentation'))
}

#' @describeIn class-tests Version of `is_exactly` for all elements of a list.
all_are_exactly <- function(lst, class, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.string(class) )
    if (all(. <- purrr::map_lgl(lst, is_exactly, class=class)))
        return(TRUE)
    bad.class <- purrr::map_chr(lst[!.], class0)
    msg <- if (sum(!.) > 1L){
        ._("%s has bad elements at positions %s which are not of class %s."
          , act$lab
          , comma_list(which(!.))
          , dQuote(class)
          )} else {
        ._("%s has bad element at position %s which is not of class %s."
          , act$lab
          , which(!.)
          , dQuote(class)
        )}
    return(structure(FALSE, msg=msg))
}
if(FALSE){#@testing
    l <- list( 'a', 'b', 'c'
             , 1, 2)
    expect_identical( validate_that(all_are_exactly(l, 'character'))
                    , "`l` has bad elements at positions 4 and 5" %<<%
                      "which are not of class" %<<%
                      dQuote("character") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1,2), 'integer', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("integer") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1L,2L), 'numeric', '...'))
                    , "... has bad elements at positions 1 and 2" %<<%
                      "which are not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_identical( validate_that(all_are_exactly(list(1, 2L), 'numeric', '...'))
                    , "... has bad element at position 2" %<<%
                      "which is not of class" %<<%
                      dQuote("numeric") %<<<% '.')
    expect_true(all_are_exactly(list(1L, 2L), 'integer'))
}


# Expectations ------

#' @name class-expectations
#' @title Class Expectations
#' @description
#' These extend the [testthat::expect_is] to have finer grain tests.
#'
#' @inheritParams testthat::expect_is
#' @param object the object in question.
#' @param class the expected class object is to be.
#'
#' @family class
#' @example inst/examples/example-class-expectations.R
NULL

#' @describeIn class-expectations test that an object does **not** inherit from a class.
expect_is_not <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <-
    exp_lab <- paste(class, collapse = "/")
    testthat::expect( Negate(is)(act$val, class)
                    , sprintf("%s is a %s; should not inherit from `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}
if(FALSE){#@testing
    expect_is_not('a', 'numeric')
}

#' @describeIn class-expectations test that an object is exactly a specific class
#'  and not a child class.
expect_is_exactly <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <- collapse(class(object), "/")
    exp_lab <- comma_list(class, sep2 = ' or ', sep.last = ', or a')
    testthat::expect( is_exactly(act$val, class)
                    , sprintf("%s is a %s; should be exactly a `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}
if(FALSE){#@testing
    x <- list(1:3)

    expect_identical(expect_is_exactly(x, 'list'), x)

    class(x) <- c('class', 'super1', 'super2')

    expect_is_exactly(x, 'class')

    expect_is(x, 'super1')
    expect_error( expect_is_exactly(x, 'super1')
                , "`x` is a class/super1/super2; should be exactly a `super1`."
                )
}

#' @describeIn class-expectations test that all elements of a list inherit a given class.
expect_all_inherit <- function (object, class, info = NULL, label = NULL) {
    act <- testthat::quasi_label(rlang::enquo(object), label)
    test <- all_inherit(object, class, label=act$lab)
    testthat::expect( isTRUE(test)
                    , attr(test, 'msg')
                    , info = info)
    invisible(test)
}
if(FALSE){#@testing
    expect_true( expect_all_inherit(1:3, 'integer'))
    l <- list( 'a', 'b', 'c'
             , 1, 2
             , function()"hello world"
             )
    expect_error( expect_all_inherit(l, 'character')
                , "`l` has bad elements at 4, 5, and 6" %<<%
                  "which do not inherit from" %<<%
                  dQuote("character") %<<<% '.')
    expect_error( expect_all_inherit(l, c('character', 'function'))
                , "`l` has bad elements at 4 and 5" %<<%
                  "which do not inherit from" %<<%
                  dQuote("character") %<<% 'or' %<<%
                  dQuote("function") %<<<% '.')
    expect_error( expect_all_inherit(l, c('character', 'numeric'))
                , "`l` has bad element at 6" %<<%
                  "which does not inherit from" %<<%
                  dQuote("character") %<<% 'or' %<<%
                  dQuote("numeric") %<<<% '.' %<<%
                  "It is a" %<<% dQuote("function"))


}


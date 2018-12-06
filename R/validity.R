# Validity Tests ------


#' @name validity-tests
#' @title Alternate check for validity
#' @description
#' These functions will test if an object is valid
#' returning a value appropriate to use in [assertthat::validate_that()],
#' [assertthat::assert_that()], or [assertthat::see_if()].
#'
#' @inheritParams class-tests
#' @inheritParams methods::validObject
#' @inheritParams base::sapply
#' @param object an S4 object to test for validity
#' @param lst a list of S4 objects to test for validity.
#' @family validity-tests
NULL

#' @describeIn validity-tests Check if an object is valid.
is_valid <- function(object, complete=FALSE){
    valid <- validObject(object, test=TRUE, complete=complete)
    if(isTRUE(valid)) return(TRUE)
                 else return(structure(FALSE, msg=valid))
}
#' @describeIn validity-tests Check if each element in a list is valid.
are_valid <-
function(lst, complete=FALSE){
    valid <- lapply(lst, is_valid, complete=complete)

    if (all(. <- sapply(valid, isTRUE))) return(.)
    messages <- sapply(valid, attr, 'msg')
    structure(., messages=messages)
}


# Expectations ----------------------


#' Expect an S4 object is valid
#'
#' Similar to [is_valid()] except designed to work in the
#' [testthat::test_that()] framework.
#'
#' @inheritParams validity-tests
#' @inheritParams testthat::expect_is
#' @family validity-tests
expect_valid <-
function (object, complete=FALSE, info=NULL, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(object), label)
    is.valid <- validObject(object, test=TRUE, complete=complete)
    testthat::expect(isTRUE(is.valid)
                    , ._("%s is not valid; %s", act$lab, dQuote(is.valid))
                    , info=info
                    )
}
if(FALSE){#@testing
    gen <- setClass('invalid', list(valid='logical'))
    setValidity('invalid', function(object) 'This class is always invalid')
    obj <- gen()

    expect_false(is_valid(obj))
    expect_identical( assertthat::validate_that(is_valid(obj))
                    , "This class is always invalid")

    expect_error( expect_valid(obj)
                , "`obj` is not valid;" %<<% dQuote("This class is always invalid")
                )

    cls <- setClass('test_class', contains='list')
    setValidity('test_class', function(object)TRUE)
    obj2 <- cls()

    expect_true(is_valid(obj2))
    expect_silent(expect_valid(obj2))

    lst <- list(obj, obj2)
    expect_identical( are_valid(lst)
                    , structure( c(FALSE, TRUE)
                               , messages = list("This class is always invalid", NULL)
                               )
                    )
    expect_identical( validate_that(all(are_valid(lst)))
                    , "Elements 1 of are_valid(lst) are not true"
                    )
    expect_identical(are_valid(list(obj2, obj2)), c(TRUE, TRUE))
}

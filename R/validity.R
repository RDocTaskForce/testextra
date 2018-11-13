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
function(lst, complete=FALSE, simplify=NA){
    valid <- lapply(lst, is_valid, complete=complete)
    if (isFALSE(simplify)) return(valid) else
    if (isTRUE(simplify) || all(valid)) return(simplify2array(valid))
    else return(valid)
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
}

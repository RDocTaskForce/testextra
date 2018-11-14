#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `validity.R`')
#line 55 "R/validity.R"
test_that('expect_valid', {#@testing
    gen <- setClass('invalid', list(valid='logical'))
    setValidity('invalid', function(object) 'This class is always invalid')
    obj <- gen()

    expect_false(is_valid(obj))
    expect_identical( assertthat::validate_that(is_valid(obj))
                    , "This class is always invalid")

    expect_error( expect_valid(obj)
                , "`obj` is not valid;" %<<% dQuote("This class is always invalid")
                )
})

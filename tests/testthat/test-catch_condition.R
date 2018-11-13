#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `catch_condition.R`')
#line 12 "R/catch_condition.R"
test_that('catch_condition', {#@testing
    val <- catch_condition(pkg_message("testing"))
    expect_is(val, 'condition')
    expect_is(val, 'message')
})

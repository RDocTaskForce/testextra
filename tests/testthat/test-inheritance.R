#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `inheritance.R`')
#line 70 "C:/Users/u0092104/Dropbox/rdtf/testextra/R/inheritance.R"
test_that('are', {#@testing
    lst <- list('a', 1L, TRUE)

    expect_true(all(are(lst, 'ANY')))
    expect_identical(are(lst, 'character'), c(T,F,F))
    expect_identical(are(lst, 'integer'), c(F,T,F))
    expect_identical(are(lst, 'numeric'), c(F,T,F))
})
#line 113 "C:/Users/u0092104/Dropbox/rdtf/testextra/R/inheritance.R"
test_that('all_are_exactly', {#@testing
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
})
#line 178 "C:/Users/u0092104/Dropbox/rdtf/testextra/R/inheritance.R"
test_that('expect_is_exactly', {#@testing
    x <- list(1:3)

    expect_identical(expect_is_exactly(x, 'list'), x)

    class(x) <- c('class', 'super1', 'super2')

    expect_is_exactly(x, 'class')

    expect_is(x, 'super1')
    expect_error( expect_is_exactly(x, 'super1')
                , "`x` is a class/super1/super2; should be exactly a `super1`."
                )
})

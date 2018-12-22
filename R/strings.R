#' @name string-tests
#' @title Tests for strings
#'
#' @param x a character vector/string.
NULL


.nonempty.string.msg <- "does not conform to a non-empty string" %<<%
                        "(a character vector of length 1 without" %<<%
                        "without missing or empty values)."

#' @describeIn string-tests Test that a character is both a string (character vector of length one)
#' and that it is non-empty, has at least one character and is not missing.
#'
#' @examples
#' # TRUE
#' is_nonempty_string("hello")
#'
#' # All FALSE
#' x <- c("hello", "world")
#' is_nonempty_string(x)
#' is_nonempty_string(NA_character_)
#' is_nonempty_string(character(0))
#' is_nonempty_string(NULL)
#' is_nonempty_string(12345)
is_nonempty_string <- structure(function(x){
    is.character(x) &&
    length(x) == 1L &&
    !is.na(x)       &&
    nchar(x) > 0L
}, fail = function(call, env){
    sQuote(deparse(call$x)) %<<% .nonempty.string.msg
})
if(FALSE){#@testing
    expect_true(is_nonempty_string("hello world"))
    expect_false(is_nonempty_string(c("hello", "world")))
    expect_false(is_nonempty_string(character(0)))
    expect_false(is_nonempty_string(NA_character_))
    expect_false(is_nonempty_string(''))

    expect_identical( validate_that(is_nonempty_string(character(0)))
                    , sQuote("character(0)") %<<% .nonempty.string.msg)

    bad <- NA
    expect_identical( validate_that(is_nonempty_string(bad))
                    , sQuote("bad") %<<%.nonempty.string.msg)
}

.optional.string.msg <- "does not conform to an optional string" %<<%
                        "(a character vector of length 0 or 1," %<<%
                        "without missing or empty values)."
#' @describeIn string-tests Check for an optional string: must be a character, not missing,
#' a vector of either length 0 or 1, and if provided must not be empty ("").
is_optional_string <- structure(function(x){
    is.character(x) &&
    length(x) <= 1L &&
    !any(is.na(x))  &&
    !any(nchar(x) == 0L)
}, fail = function(call, env){
    sQuote(deparse(call$x)) %<<% .optional.string.msg
})
if(FALSE){#@testing
    expect_true(is_optional_string("hello"))
    expect_true(is_optional_string(character(0)))
    expect_false(is_optional_string(NA_character_))
    expect_false(is_optional_string(''))
    expect_false(is_optional_string(letters))

    bad <- NA_character_
    expect_identical(validate_that(is_optional_string(bad))
                    , sQuote('bad') %<<% .optional.string.msg)
}

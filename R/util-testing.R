#' Extract class as a single string.
#'
#' @param x any object.
class0 <- function(x)collapse(class(x), '/')


#' Check if a regular expression is valid.
#'
#' @param pattern the regular expression pattern to test.
is_valid_regex <- function(pattern){
    tryCatch( grepl(pattern, '') || TRUE
            , error= function(e)structure(FALSE, msg=e$message))
}
if(FALSE){#@testing
    expect_true(is_valid_regex("^hello world$"))
    expect_false(is_valid_regex("^hello (world$"))
    expect_identical( validate_that(is_valid_regex("^hello (world$"))
                    , "invalid regular expression " %<<<%
                      "'^hello (world$', reason 'Missing ')''"
                    )
}




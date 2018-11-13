
#' Catch a condition for testing.
#'
#' This function captures a condition object such as a warning or
#' error, to allow for testing components and classes.
#'
#' @param code code to run that should assert a condition.
catch_condition <- function(code){
    val <- tryCatch(force(code), condition = function(cond)cond)
    if (is(val, 'condition')) return(val)
}
if(FALSE){#@testing
    val <- catch_condition(pkg_message("testing"))
    expect_is(val, 'condition')
    expect_is(val, 'message')
}

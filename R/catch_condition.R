
#' Catch a condition for testing.
#'
#' This function captures a condition object such as a warning or
#' error, to allow for testing components and classes.
#'
#' @param code code to run that should assert a condition.
#' @examples
#' (cond <- catch_condition(stop("catch me.")))
#' class(cond)
#'
#' my_fun <- function(){
#'     message("a message")
#'     warning("a warning")
#'     pkg_message("a package message", scope="test")
#'     pkg_warning("a package warning", scope="test")
#'     pkg_error("a package error", scope='test')
#' }
#' conditions <- catch_all_conditions(my_fun())
#' conditions$messages
#' conditions$warnings
#' conditions$error  # only one error can be caught at a time.
#'
catch_condition <- function(code){
    val <- tryCatch(force(code), condition = function(cond)cond)
    if (is(val, 'condition')) return(val)
}
if(FALSE){#@testing
    val <- catch_condition(pkg_message("testing"))
    expect_is(val, 'condition')
    expect_is(val, 'message')
}

#' @rdname catch_condition
catch_all_conditions <- function(code){
    conditions <- list()
    tryCatch(
        withCallingHandlers( code
                           , warning = function(cond){
                                   conditions$warnings <<- c(conditions$warnings, list(cond))
                                   invokeRestart("muffleWarning")
                           }
                           , message = function(cond){
                                   conditions$messages <<- c(conditions$messages, list(cond))
                                   invokeRestart("muffleMessage")
                           })
            , error     = function(cond) conditions$error <<- cond
            , condition = function(cond) conditions$other <<- cond
            )
    return(conditions)
}
if(FALSE){#@testing
    my_fun <- function(){
        message("a message")
        warning("a warning")
        pkg_message("a package message", scope="test")
        pkg_warning("a package warning", scope="test")
        pkg_error("a package error", scope='test')
    }
    conditions <- catch_all_conditions(my_fun())

    expect_length(conditions, 3)
    expect_is(conditions$error, 'test-error')
    expect_length(conditions$warnings, 2)
    expect_is(conditions$warnings[[1]], 'simpleWarning')
    expect_is(conditions$warnings[[2]], 'test-warning')
    expect_length(conditions$messages, 2)
    expect_is(conditions$messages[[1]], 'simpleMessage')
    expect_is(conditions$messages[[2]], 'test-message')
}

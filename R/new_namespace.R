
#' Create a new namespace environment
#'
#' @param name The name of the environment
#' @param path An optional path.
#' @param import Package to include in the imports.
#' @inheritDotParams new_namespace_env
#' @param register Should the package namespace be registered?
new_namespace_env <-
function( name
        , path = file.path(tempdir())
        , import = 'methods'
        ){
    assert_that(!isNamespaceLoaded(name))
    new_sub_env <- function(part, parent=baseenv()){
        structure( new.env(parent=parent, hash=TRUE)
         , name = part %<<<% ':' %<<<% name )
    }

    if (!dir.exists(path)) dir.create(path) # nocov
    path <- normalizePath(path, "/", TRUE)

    imports <- new_sub_env('imports')
    ns      <- new.env(TRUE, imports)
    ns$.__NAMESPACE__. <- new.env(parent = baseenv())
    ns$.__NAMESPACE__.$spec <- c(name = name, version = "0.0.0")
    setNamespaceInfo(ns, "exports"  , new_sub_env('exports'))
    setNamespaceInfo(ns, "lazydata" , new_sub_env('lazydata'))
    setNamespaceInfo(ns, "imports"  , list(base = TRUE))
    setNamespaceInfo(ns, "path"     , path)
    setNamespaceInfo(ns, "dynlibs"  , NULL)
    setNamespaceInfo(ns, "S3methods", matrix(NA_character_, 0L, 3L))
    ns$.__S3MethodsTable__. <- new.env(hash = TRUE, parent = baseenv())

    for (i in import) namespaceImport(ns, i)
    ns
}
if(FALSE){#@testing
    ns <- new_namespace_env("test namespace")
    expect_true(isNamespace(ns))
    expect_false(isNamespaceLoaded("test namespace"))
}

#' @rdname new_namespace_env
new_pkg_environment <-
function( name = "test package environment"
        , ...
        , register = FALSE
        ){
    env <- new_namespace_env(name, ...)
    setPackageName(name, env)
    if (register)
        register_namespace(env)
    return(env)
}
if(FALSE){#@testing
    ns <- new_pkg_environment()
    expect_true(isNamespace(ns))
    expect_equal(getPackageName(ns), "test package environment")
    expect_equal(environmentName(ns), "test package environment")
    expect_false(is_namespace_registered(ns))

    ns2 <- new_pkg_environment("pkg2", register=TRUE)
    expect_true(isNamespace(ns2))
    expect_equal(getPackageName(ns2), "pkg2")
    expect_equal(environmentName(ns2), "pkg2")
    expect_true(is_namespace_registered('pkg2'))
}

makeActiveBinding('.ns.registry', function(){
    (get(".Internal", envir = baseenv(), mode = "function"))(getNamespaceRegistry())
}, environment())

#' Register a namespace
#'
#' Only full packages should ever be registered.
#'
#' @param ns A namespace environment.
#'
register_namespace <- function(ns){
    assert_that( isNamespace(ns)
               , is_nonempty_string(name <- environmentName(ns))
               , !is_namespace_registered(name)
               )
    assign(name, ns, .ns.registry)
    invisible(ns)
}

#' Check if a namespace is registered
#'
#' @param ns a namespace environment or a character name of a namespace.
is_namespace_registered <-
function(ns){
    if (is.environment(ns) && assert_that(isNamespace(ns)))
        ns <- environmentName(ns)
    else assert_that(is.character(ns)
                    , msg = "ns must be a name string or a namespace environment" )
    ns %in% names(.ns.registry)
}

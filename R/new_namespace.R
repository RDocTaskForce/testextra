globalVariables(c('getNamespaceRegistry', 'unregisterNamespace'))

#' @name namespaces
#' @title Create namespace environments
#'
#' @description
#' Create and manipulate namespace and test package environments.
#'
#' @param name The name of the environment
#' @param path An optional path.
#' @param import Package to include in the imports.
#' @inheritDotParams new_namespace_env
#' @param register Should the package namespace be registered?
#' @param ns a namespace environment or a character name of a namespace.
#' @example inst/examples/example-namespace.R
NULL

#' @describeIn namespaces Create a new namespace environment
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

    imports <- new_sub_env('imports', .BaseNamespaceEnv)
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

    for (i in import) namespaceImport(self=ns, i, from=i)
    ns
}
if(FALSE){#@testing
    ns <- new_namespace_env("test namespace")
    expect_true(isNamespace(ns))
    expect_false(isNamespaceLoaded("test namespace"))
}

#' @describeIn namespaces Create a package environment.
#' All package environments are namespaces but not all
#' namespaces qualify as package environments.
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

    if (is_namespace_registered("pkg2"))
        unregister_namespace(asNamespace("pkg2"))

    ns2 <- new_pkg_environment("pkg2", register=TRUE)
    expect_true(isNamespace(ns2))
    expect_equal(getPackageName(ns2), "pkg2")
    expect_equal(environmentName(ns2), "pkg2")
    expect_true(is_namespace_registered('pkg2'))
}
if(FALSE){#@testing Can define classes, generics and methods.
    ns <- new_pkg_environment("class-test", register=TRUE)
    expect_true(isNamespace(ns))
    expect_equal(getPackageName(ns), "class-test")
    expect_equal(environmentName(ns), "class-test")
    expect_true(is_namespace_registered(ns))

    cls <- setClass("my-test-class", contains='list', where=ns)
    expect_is(cls, 'classGeneratorFunction')
    expect_true(exists(classMetaName(cls@className), ns))

    val <- setGeneric( "my_generic", function(object)stop('not implimented')
                     , where = ns )
    expect_identical(val, "my_generic")
    expect_true(exists('my_generic', ns))

    val <- setMethod('my_generic', 'my-test-class', function(object){
        "horray it works"
    }, where=ns)
    expect_identical(val, "my_generic")
    expect_true(exists(methodsPackageMetaName('T', "my_generic", getPackageName(ns)), ns))
    expect_true(exists('my-test-class'
                      , get( methodsPackageMetaName('T', "my_generic", getPackageName(ns))
                           , ns)))

    unregister_namespace(ns)
    expect_false(is_namespace_registered(ns))
    expect_false(unregister_namespace(ns))
}
if(FALSE){#@testing can specify imports
    pkg <- new_pkg_environment('test-import', import=c('methods', 'testextra'))
    expect_true(isNamespace(pkg))
    expect_true("testextra" %in% names(pkg$.__NAMESPACE__.$imports))
    expect_true(exists("new_pkg_environment", parent.env(pkg), inherits = TRUE))
}


.ns.registry <- function(){
    (get(".Internal", envir = baseenv(), mode = "function"))(getNamespaceRegistry())
}

#' @describeIn namespaces Register a namespace
register_namespace <- function(ns){
    assert_that( isNamespace(ns)
               , is_nonempty_string(name <- environmentName(ns))
               , !is_namespace_registered(name)
               )
    assign(name, ns, .ns.registry())
    invisible(ns)
}

#' @describeIn namespaces Remove a namespace from the registry
unregister_namespace <- function(ns){
    assert_that( isNamespace(ns)
               , is_nonempty_string(name <- environmentName(ns))
               )
    if (is_namespace_registered(name))
        (get(".Internal", envir = baseenv(), mode = "function"))(unregisterNamespace(name))
    else FALSE
}

#' @describeIn namespaces Check if a namespace is registered
is_namespace_registered <-
function(ns){
    if (is.environment(ns) && assert_that(isNamespace(ns)))
        ns <- environmentName(ns)
    else assert_that(is.character(ns)
                    , msg = "ns must be a name string or a namespace environment" )
    ns %in% names(.ns.registry())
}

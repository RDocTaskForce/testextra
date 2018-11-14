#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `new_namespace.R`')
#line 38 "C:/Users/u0092104/Dropbox/rdtf/testextra/R/new_namespace.R"
test_that('new_namespace_env', {#@testing
    ns <- new_namespace_env("test namespace")
    expect_true(isNamespace(ns))
    expect_false(isNamespaceLoaded("test namespace"))
})
#line 56 "C:/Users/u0092104/Dropbox/rdtf/testextra/R/new_namespace.R"
test_that('new_pkg_environment', {#@testing
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
})

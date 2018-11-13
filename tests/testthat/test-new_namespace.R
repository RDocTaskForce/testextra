#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `new_namespace.R`')
#line 36 "R/new_namespace.R"
test_that('new_namespace_env', {#@testing
    ns <- new_namespace_env("test namespace")
    expect_true(isNamespace(ns))
    expect_false(isNamespaceLoaded("test namespace"))
})
#line 54 "R/new_namespace.R"
test_that('new_pkg_environment', {#@testing
    ns <- new_pkg_environment()
    expect_true(isNamespace(ns))
    expect_equal(getPackageName(ns), "test package environment")
    expect_equal(environmentName(ns), "test package environment")
})

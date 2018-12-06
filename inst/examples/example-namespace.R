ns <- new_namespace_env('my namespace')
isNamespace(ns)
environmentName(ns)
packageName(ns) # not a package

pkg <- new_pkg_environment("myPackage")
isNamespace(pkg)
environmentName(pkg)
packageName(pkg)             # now a package
is_namespace_registered(pkg) # but not registered
\dontrun{
asNamespace("myPackage")     # so this WILL NOT work.
}

register_namespace(pkg)
is_namespace_registered(pkg) # now registered
asNamespace("myPackage")     # so this WILL work.

unregister_namespace(pkg)
is_namespace_registered(pkg) # now unregistered
isNamespace(pkg)             # but still a namespace


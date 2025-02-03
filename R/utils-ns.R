ns_env <- function (x = parent.frame()) {
  env <- switch(typeof(x),
                builtin = ,
                special = ns_env("base"),
                closure = topenv(environment(x) %||% asNamespace("base")),
                environment = topenv(x),
                character = if (is_string(x)) asNamespace(x))
  if (!isNamespace(env)) {
    stop("Input must be a package name or a function inheriting from a namespace")
  }
  env
}
ns_env_name <- function (x = caller_env()) {
  env <- switch(typeof(x),
                environment = ,
                builtin = ,
                special = ,
                closure = ns_env(x),
                stop("Input must be a package name or a function inheriting from a namespace"))
  unname(getNamespaceName(env))
}

ns_exports <- function(x) {
  if (is.call(x) || is_symbol(x)) {
    x <- ns_env(lang_get_fn(x))
  } else if (is.function(x)) {
    x <- ns_env(x)
  }
  getNamespaceExports(x)
}

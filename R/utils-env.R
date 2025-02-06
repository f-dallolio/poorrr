# environment ----
#' Get properties of the current or caller frame
#' @name utils-env
NULL
#' @rdname utils-env
#' @export
env_parent <- function (env = parent.frame(), n = 1) {
  stopifnot(is.environment(env))
  while (n > 0) {
    if (identical(env, emptyenv())) {
      stop("The empty environment has no parent.")
    }
    n <- n - 1
    env <- parent.env(env)
  }
  env
}
#' @rdname utils-env
#' @export
env_get_list <- function (env = caller_env(),
                          nms = names(env),
                          default = list(NULL),
                          inherit = FALSE,
                          last = NULL,
                          mode = "any") {
  if (!is.null(last)) {
    warning("Argument `last` is not implemented. Kept only for compatibility with `rlang`.")
  }
  out <- mget(x = nms,
              envir = env,
              mode = mode,
              ifnotfound = default,
              inherits = inherit)
  i <- vapply(out, Negate(identical), logical(1), default)
  out[i]
}
#' @rdname utils-env
#' @export
env_get <- function (env = caller_env(),
                     nm,
                     default = NULL,
                     inherit = FALSE,
                     last = NULL,
                     mode = "any") {
  if (missing(default) || identical(default, quote(expr = ))) {
    get(nm, envir = env, mode = mode, inherits = inherit)
  } else {
    get0(nm, envir = env, mode = mode, inherits = inherit, ifnotfound = default)
  }
}
#' @rdname utils-env
#' @export
env_names <- function(env, sort = FALSE) {
  stopifnot(is.environment(env))
  if (sort) {
    sort(names(env))
  } else {
    names(env)
  }
}
#' @rdname utils-env
#' @export
env_names_if <- function(env, .p = NULL, ..., sort = FALSE) {
  stopifnot(is.environment(env))
  if (is.null(.p)) {
    return(env_names(env, sort))
  }
  p <- as_predicate(.p, ...)
  out <- keep(env_get_list(env = env), .p, ...)
  if(sort) {
    out[sort(names(out))]
  } else {
    out
  }
}

env_info <- function (env, ..., sort = TRUE) {
  objs <- mget(names(env), env)
  if (sort) {
    objs <- objs[sort(names(objs))]
  }
  df <- data.frame(name = names(objs),
                   # export = ifelse(export, "export", "private"),
                   # export = export,
                   mode = vapply(objs, mode, ""),
                   type = vapply(objs, typeof, ""),
                   class = vapply(objs, function(x) class(x)[[1]], ""))
  row.names(df) <- NULL
  df
  # lifecycle <- find_fn_lifecycle(pkg)
  # names(lifecycle)[names(lifecycle) == "value"] <- "lifecycle"
  # out <- merge(df, lifecycle, all.x = TRUE)
  # out$lifecycle[is.na(out$lifecycle)] <- ""
  # out
}
find_fn_lifecycle <- function(pkg, nms = getNamespaceExports(pkg)) {
  if (length(nms) > 1) {
    out <- lapply(nms, sys.function(), pkg = pkg)
    out <- do.call(rbind, Filter(length, out))
    out <- sort_by(out, out$value)
    attr(out, "row.names") <- .set_row_names(nrow(out))
    return(out)
  }
  .find_lifecycle(nms, pkg)
}
.find_lifecycle <- function (nm, pkg) {
  fn <- get0(nm, envir = asNamespace(pkg), mode = "function")
  if (is.null(fn)) {
    return(NULL)
  }
  call <- body(fn)
  if (is_call(call, "{")) {
    call <- call[[2]]
  }
  if (!is.call(call)) {
    return(NULL)
  }
  if (is.null(call_ns(call)) || call_ns(call) != "lifecycle") {
    return(NULL)
  }
  name <- call_name(call)
  if (name == "signal_stage") {
    out <- call_args(call)[[1]]
  } else {
    out <- name
  }
  data.frame(name = nm, value = out)
}


# eamespace ----
#' Get the namespace of a package
#' @name utils-ns
NULL
#' @rdname utils-ns
#' @export
ns_env <- function (x = parent.frame()) {
  env <- switch(typeof(x),
                builtin = ,
                special = asNamespace("base"),
                closure = topenv(environment(x) %||% asNamespace("base")),
                environment = topenv(x),
                character = if (length(x) == 1) asNamespace(x))
  if (!isNamespace(env)) {
    stop("Input must be a package name or a function inheriting from a namespace")
  }
  env
}
#' @rdname utils-ns
#' @export
ns_imports <- function (ns, sort = TRUE) {
  if (is.function(ns)) {
    ns <- ns_env_name(ns)
  }
  out <- getNamespaceImports(ns)
  if (sort) {
    return(sort(out))
  }
  out
}
#' @rdname utils-ns
#' @export
ns_imports_env <- function (x = parent.frame()) {
  env_parent(ns_env(x))
}
#' @rdname utils-ns
#' @export
ns_import_from <- function (ns, names, env = parent.frame()) {
  objs <- mget(names, ns_env(ns), ifnotfound = list(NULL))
  objs <- Filter(Negate(is.null), objs)
  for (i in seq_along(objs)) {
    assign(names(objs)[[i]], objs[[i]], envir = env)
  }
}
#' @rdname utils-ns
#' @export
ns_env_name <- function (x = parent.frame()) {
  env <- switch(typeof(x),
                environment = ,
                builtin = ,
                special = ,
                closure = ns_env(x),
                stop("Input must be a package name or a function inheriting from a namespace"))
  unname(getNamespaceName(env))
}
#' @rdname utils-ns
#' @export
ns_exports <- function (ns, sort = TRUE) {
  if (is.function(ns)) {
    ns <- ns_env_name(ns)
  }
  out <- getNamespaceExports(ns)
  if (sort) {
    sort(out)
  } else {
    out
  }
}
#' @rdname utils-ns
#' @export
ns_privates <- function (ns, sort = TRUE) {
  out <- setdiff(names(asNamespace(ns)), getNamespaceExports(ns))
  if (sort) {
    sort(out)
  } else {
    out
  }
}
#' @rdname utils-ns
#' @export
ns_names <- function(ns, sort = TRUE) {
  out <- names(asNamespace(ns))
  if (sort) {
    sort(out)
  } else {
    out
  }
}
#' @rdname utils-ns
#' @export
ns_exports_has <- function (ns, name) {
  if (is_string(ns)) {
    ns <- rlang:::ns_env(ns)
  }
  if (identical(ns, ns_env("base"))) {
    exports <- baseenv()
  } else {
    exports <- ns$.__NAMESPACE__.$exports
  }
  !is.null(exports) && exists(name, envir = exports, inherits = FALSE)
}

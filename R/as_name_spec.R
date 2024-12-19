#' @export
as_name_spec <- function(name_spec) {
  UseMethod("as_name_spec")
}
#' @export
as_name_spec.default <- function(name_spec, ...) {
  stop("Argument `name_spec` must be a glue-like string, a formula, or a function.")
}
#' @export
as_name_spec.character <- function(name_spec, ...) {
  stopifnot(length(name_spec) == 1)
  matches <- c(set_names(gregexpr("[{]outer[}]", name_spec), "outer"),
               set_names(gregexpr("[{]inner[}]", name_spec), "inner"))
  matches <- sort(unlist(matches))
  matches <- matches[matches != -1]
  nms <- names(matches)
  nms[grep("outer", nms)] <- "outer"
  nms[grep("inner", nms)] <- "inner"

  fmt <- gsub("[{]outer[}]|[{]inner[}]", "%s", name_spec)
  body_call <- make_call("sprintf", fmt = fmt, .args = lapply(nms, str2lang))

  out <- make_function(alist(outer = , inner = ),
                       call("{", body_call),
                       env = caller_env())

  structure(out, class = c("name_spec_function", "function"))
}
#' @export
as_name_spec.formula <- function(name_spec) {
  out <- make_function(
    alist(outer = , inner = , ... = , .x = outer, .y = inner),
    call("{", name_spec[[2]]),
    env = caller_env()
  )
  structure(out, class = c("name_spec_function", "function"))
}
#' @export
as_name_spec.function <- function(name_spec, ...) {
  body_call <- bquote({
    fn <- .(name_spec)
    fn(outer, inner)
  })
  out <- make_function(
    alist(outer = , inner =),
    body_call,
    env = caller_env()
  )
  structure(out, class = c("name_spec_function", "function"))
}

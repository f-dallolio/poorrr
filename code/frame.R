frame_get <- function (frame, accessor) {
  if (identical(frame, globalenv())) return(NULL)
  frames <- eval(call("sys.frames"), frame)
  for (i in seq_along(frames)) {
    if (identical(frames[[i]], frame)) {
      return(accessor(i))
    }
  }
  NULL
}
frame_call <- function (frame = caller_env()) {
  stopifnot(is.environment(frame))
  frame_get(frame, sys.call)
}
frame_fn <- function (frame = caller_env()) {
  stopifnot(is.environment(frame))
  frame_get(frame, sys.function)
}

caller_arg <- function (arg) {
  arg <- substitute(arg)
  stopifnot(is.symbol(arg))
  expr <- do.call(substitute, list(arg), envir = parent.frame())
  deparse(expr)
}
caller_env <- function (n = 1) {
  parent.frame(n + 1)
}
caller_call <- function (n = 1){
  stopifnot(n %/% 1 == 0)
  frame_call(caller_env(n + 1))
}
caller_fn <- function (n = 1){
  stopifnot(n %/% 1 == 0)
  frame_fn(caller_env(n + 1))
}

current_env <- function () {
  caller_env()
}
current_call <- function() {
  caller_call()
}
current_fn <- function() {
  caller_fn()
}

x <- grep("^is_", getNamespaceExports(asNamespace("rlang")), value = T) |>
  setdiff(grep(pattern = "^ffi_", getNamespaceExports(asNamespace("rlang")), value = T)) |>
  setdiff(ls()) |>
  gsub(pattern = "is_", replacement = "is.") |>
  intersect(grep("^is[.]", getNamespaceExports(asNamespace("base")), value = T)) |>
  gsub(pattern = "^is[.]", replacement = "^is_?.*_") |> paste(collapse = "|") |>
  grep(getNamespaceExports(asNamespace("rlang")), value = T) |>
  mget(asNamespace("rlang")) |> sapply(Negate(is.primitive)) |> which() |> names() |>
  mget(asNamespace("rlang"))
Map(\(x, nms) paste(nms,
                    " <- ",
                    call("function", formals(x), body(x)) |> deparse() |> paste(collapse = "\n")
                    ),
    x,
    names(x)) |>
  unlist() |>
  cat(sep = "\n\n",
      file = "R/is.R")

new_function <- function (args, body, env = caller_env()) {
  as.function(c(args, body), envir = env)
}
as_function <- function (x, env = global_env()) {
  if (is.function(x)) return(x)
  if (is.call(x) && identical(x[[1]], quote(`~`))) {
    stopifnot("If a formula, `x` must be one-sided (rhs)." = length(x) < 3)
    env <- attr(x, ".Environment")
    stopifnot(is.environment(env))
    args <- list(... = quote(expr = ),
                 .x = quote(..1),
                 .y = quote(..2),
                 . = quote(..1))
    fn <- structure(.Data = as.function(c(args, x[[2]]), envir = env),
                    class = c("rlang_lambda_function", "function"))
    return(fn)
  }
  if (is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }
  stop("Can't convert `x` to a function.")
}
as_closure <- function (x, env = caller_env()) {
  x <- as_function(x, env = env)
  stopifnot(is_function(x))
  if (typeof(x) == "closure") return(x)
  fn_name <- .primitive_name(x)
  # fn <- op_as_closure(fn_name)
  if (!is_null(fn)) return(fn)
  fmls <- formals(args(fn_name))
  prim_call <- do.call(call, c(x, .primitive_args(fmls)))
  new_function(fmls, prim_call, global_env())
}

.primitive_name <- function (prim) {
  stopifnot(is.primitive(prim))
  name <- format(prim)
  name <- sub("^\\.(Primitive|Native)\\(\"", "", name)
  name <- sub("\"\\)$", "", name)
  name
}
.primitive_sym <- function(x) {
  as.symbol(.primitive_name(x))
}
.primitive_body <- function(x) {
  if(!is.function(x)) return(NULL)
  if(!is.primitive(x) ) return(body(x))
  str <- gsub("^.*[.]Primitive", ".Primitive", capture.output(x))
  str2lang(str)
}
.primitive_fmls <- function(x) {
  if(!is.function(x)) return(NULL)
  if(!is.primitive(x) ) return(formals(x))
  formals(args(.primitive_name(x)))
}
.primitive_env <- function(x) {
  if(!is.function(x)) return(NULL)
  if(!is.primitive(x) ) return(formals(x))
  asNamespace("base")
}
.primitive_args <- function (fmls) {
  args <- names(fmls)
  dots_i <- match("...", args)
  if (!is_na(dots_i)) {
    idx <- seq2(dots_i + 1L, length(args))
    names2(args)[idx] <- args[idx]
  }
  lapply(args, as.symbol)
}







# op_as_closure <- function (prim_nm) {
#   switch(prim_nm,
#          `<-` = ,
#          `<<-` = ,
#          `=` = function(.x, .y) {
#            op <- sym(prim_nm)
#            expr <- expr((!!op)(!!enexpr(.x), !!enexpr(.y)))
#            eval(expr, caller_env())},
#          `@` = ,
#          `$` = function(.x, .i) {
#            op <- sym(prim_nm)
#            expr <- expr((!!op)(.x, !!quo_squash(enexpr(.i), warn = TRUE)))
#            eval_bare(expr)},
#          `[[<-` = function(.x, .i, .value) {
#            expr <- expr((!!enexpr(.x))[[!!enexpr(.i)]] <- !!enexpr(.value))
#            eval_bare(expr, caller_env())},
#          `[<-` = function(.x, ...) {
#            args <- exprs(...)
#            n <- length(args)
#            if (n < 2L) {
#              abort("Must supply operands to `[<-`.")
#            }
#            expr <- expr((!!enexpr(.x))[!!!args[-n]] <- !!args[[n]])
#            eval_bare(expr, caller_env())
#          },
#          `@<-` = function(.x, .i, .value) {
#            expr <- expr(`@`(!!enexpr(.x), !!enexpr(.i)) <- !!enexpr(.value))
#            eval_bare(expr, caller_env())
#          },
#          `$<-` = function(.x, .i, .value) {
#            expr <- expr(`$`(!!enexpr(.x), !!enexpr(.i)) <- !!enexpr(.value))
#            eval_bare(expr, caller_env())
#          },
#          `(` = function(.x) .x, `[` = function(.x, ...) .x[...],
#          `[[` = function(.x, ...) .x[[...]],
#          `{` = function(...) {
#            values <- list(...)
#            values[[length(values)]]
#          },
#          `&` = new_binary_closure(function(.x, .y) .x & .y),
#          `|` = new_binary_closure(function(.x, .y) .x | .y),
#          `&&` = new_binary_closure(function(.x, .y) .x && .y),
#          `||` = new_binary_closure(function(.x, .y) .x || .y, shortcircuiting = TRUE),
#          `!` = function(.x) !.x,
#          `+` = new_binary_closure(function(.x, .y) if (missing(.y)) .x else .x + .y, versatile = TRUE),
#          `-` = new_binary_closure(function(.x, .y) if (missing(.y)) -.x else .x - .y, versatile = TRUE),
#          `*` = new_binary_closure(function(.x, .y) .x * .y),
#          `/` = new_binary_closure(function(.x, .y) .x/.y),
#          `^` = new_binary_closure(function(.x,.y) .x^.y),
#          `%%` = new_binary_closure(function(.x, .y) .x%%.y),
#          `<` = new_binary_closure(function(.x, .y) .x < .y),
#          `<=` = new_binary_closure(function(.x, .y) .x <= .y),
#          `>` = new_binary_closure(function(.x, .y) .x > .y),
#          `>=` = new_binary_closure(function(.x, .y) .x >= .y),
#          `==` = new_binary_closure(function(.x,.y) .x == .y),
#          `!=` = new_binary_closure(function(.x, .y) .x != .y),
#          `:` = new_binary_closure(function(.x, .y) .x:.y),
#          `~` = function(.x, .y) {
#            if (is_missing(substitute(.y))) {
#              new_formula(NULL, substitute(.x), caller_env())
#            } else {
#              new_formula(substitute(.x), substitute(.y), caller_env())
#            }},
#          c = function(...) c(...), seq.int = function(from = 1L, to = from, ...) seq.int(from, to, ...))
# }

# rlang:::new_binary_closure

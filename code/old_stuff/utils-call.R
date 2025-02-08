# #' @rdname XXX
#' @export
call_head <- function(call) {
  stopifnot(is.call(call))
  return(call[[1]])
}

# #' @rdname XXX
#' @export
call_name <- function(call) {
  stopifnot(is.call(call))
  head <- call[[1]]
  if (is.call(head) && deparse(head[[1]]) %in% c("::", ":::")) {
    deparse(head[[3]])
  } else if (is.symbol(head)) {
    deparse(head)
  } else {
    NULL
  }
}
# #' @rdname XXX
#' @export
call_ns <- function(call) {
  stopifnot(is.call(call))
  head <- call[[1]]
  if (is.call(head) && deparse(head[[1]]) %in% c("::", ":::")) {
    deparse(head[[2]])
  } else {
    NULL
  }
}
# #' @rdname XXX
#' @export
call_args <- function(call) {
  stopifnot(is.call(call))
  as.list(call)[-1]
}

# #' @rdname XXX
#' @export
call_type <- function(x) {
  stopifnot(typeof(x) == "language")
  type <- typeof(x[[1]])
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(x[[1]])) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    stop("corrupt language object")
  }
}
# #' @rdname XXX
#' @export
call_type2 <- function(x) {
  stopifnot(typeof(x) == "language")
  head <- x[[1]]
  if (identical(head, quote(`~`))) {
    return("formula")
  }
  type <- typeof(head)
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(x[[1]])) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    stop("corrupt language object")
  }
}

#' @export
as_call <- function(x, ...) {
  UseMethod("as_call")
}
#' @export
as_call.default <- function(x, ...) {
  as.call(x)
}
#' @export
as_call.function <- function(x, ...) {
  stopifnot(typeof(x) == "closure")
  body <- body(x)
  if (!(is.call(body) && deparse(body[[1]]) == "{")) {
    body <- call("{", body)
  }
  call("function", as.pairlist(formals(x)), body)
}
#' @export
as_call.name <- function(x, ...) {
  as.call(list(x))
}
#' @export
as_call.character <- function(x, ...) {
  if (length(x) == 1) {
    return(call(x))
  }
  stop("Input must be a string.")
}
#' @export
as_call.list <- function(x, ..., .head = NULL) {
  if (is.null(.head)) {
    .head <- x[[1]]
    x <- as.list(x)[-1]
  }
  if (is_string(.head)) {
    if (grepl(":{2,3}", .head)) {
      .head <- str2lang(.head)
    } else {
      .head <- as.symbol(.head)
    }
  }
  as.call(c(.head, x))
}
#' @export
as_call.pairlist <- function(x) {
  as_call.list(as.list(x))
}

new_call <- function(car, cdr = NULL) {
  if (is_string(car)) {
    if (grepl(":{2,3}", car)) {
      car <- str2lang(car)
    } else {
      car <- as.symbol(car)
    }
  }
  stopifnot(is.function(car) || is.symbol(car) || is.call(car))
  out <- pairlist(car)
  if (is.null(cdr)) {
    return(as.call(out))
  }
  as.call(c(out, as.pairlist(cdr)))
}

new_formula <- function(..., lhs, rhs) {
                          # call_prep <- function(x) {
                          #   if (is.character(x)) {
                          #     stopifnot(length(x) == 1)
                          #     x <- tryCatch(str2lang(x), error = function(e) as.symbol(x))
                          #   }
                          #   if (is.symbol(x) || is_namespaced_symbol(x)) {
                          #     x <- as.call(list(x))
                          #   }
                          #   head <- x[[1]]
                          #   args <- as.pairlist(as.list(x)[-1])
                          #   out <- structure(x,
                          #                    fn = head,
                          #                    args = args,
                          #                    ns = NULL,
                          #                    class = c("call_prep"))
                          #   type <- call_type(x)
                          #   if (type == "named" || type == "inlined") {
                          #     return(out)
                          #   }
                          #   if (type == "namespaced") {
                          #     attr(out, "fn") <- deparse(head[[3]])
                          #     attr(out, "ns") <- deparse(head[[2]])
                          #     attr(out, "private") <- deparse(head[[1]]) == ":::"
                          #     return(out)
                          #   }
                          #   if (type == "recursive") {
                          #     attr(out, "fn") <- call_prep(head)
                          #     return(out)
                          #   }
                          #   stop("corrupt language object")
                          # }
                          #
                          # call_head <- function(x) {
                          #   if (is.character(x)) {
                          #     stopifnot(length(x) == 1)
                          #     x <- tryCatch(str2lang(x), error = function(e) as.symbol(x))
                          #   }
                          #   if (is.symbol(x) || is_namespaced_symbol(x)) {
                          #     x <- as.call(list(x))
                          #   }
                          #   type <- call_type(x)
                          #   if (type == "inlined") (
                          #     head <- as.symbol(fn_name(x[[1]]))
                          #   ) else {
                          #     head <- x[[1]]
                          #   }
                          #
                          #
                          #
                          #   out <- list(ns = "", op = "", fn = "")
                          #   if (type == "named") {
                          #     out$fn <- deparse(head)
                          #   } else if (type == "namespaced") {
                          #     out$op <- deparse(head[[1]])
                          #     out$ns <- deparse(head[[2]])
                          #     out$fn <- deparse(head[[3]])
                          #   } else if (type == "recursive") {
                          #     out$fn <- call_head(head)
                          #    } else {
                          #       stop("corrupt language object")
                          #     }
                          #   }
                          #   out
                          # }
                          # x <- str2lang("base::mean()")
                          # if (type) {
                          #   x <- setNames(as.list(x), c("op", "ns", "fn"))
                          #   vapply(x, deparse, "")[c("ns", "op", "fn")]
                          # } else if (
                          # )
                          #
                          # x <-
  # x <-
  call_add_namespace <- function(call, fn = NULL) {
    if (is_string(call)) {
      call <- tryCatch(as.symbol(call), error = function(e) as.symbol(call))
    }
    if (is_namespaced_symbol(call)) {
      call <- as.call(list(call))
    }
    if (!is.call(call) && !is.symbol(call[[1]])) {
      return(call)
    }
    if (is.null(fn)) {
      fn <- fn_match(call)
    }
    if (is_namespaced_call(call)) {
      sym <- call[[1]][[3]]
    } else {
      sym <- call[[1]]
    }
    nm <- deparse(sym)
    if (nm %in% c("::", ":::")) {
      return(call)
    }
    env <- environment(fn)
    top <- topenv(env)
    if (identical(env, globalenv())) {
      prefix <- "global"
      op <- "::"
    } else if (isNamespace(top)) {
      prefix <- ns_env_name(top)
      if (nm %in% getNamespaceExports(top)) {
        op <- "::"
      } else {
        op <- ":::"
      }
    } else {
      return(call)
    }
    namespaced_sym <- call(op, as.symbol(prefix), sym)
    call[[1]] <- namespaced_sym
    call
  }
}

# is_df <- TRUE
# x <- as.list(mtcars)
# nms <- names(mtcars)
# out <- setNames(rep(list(call("[[", quote(x))), length(x)), names(x))
# map(1:10, function(x, nm = "mtcars", n = length(x)) {
#   mtcars
# })

# call_append <- function (x, values, ..., after = NULL, bound = c("last","first")) {
#   x <- as.list(x)
#   lengx <- length(x)
#   if (is.null(after)) {
#      if (match.arg(bound) == "last") {
#        after <-  length(x)
#      } else {
#        after <- 0
#      }
#   }
#   if (after >= -n && after < 0) {
#     after <- n + 1 + after
#   }
#   if (!after) {
#     out <- c(values, x)
#   } else if (after >= lengx) {
#     out <- c(x, values)
#   }else {
#     out <- c(x[1L:after], values, x[(after + 1L):lengx])
#   }
#   as.call(out)
# }
# call_prepend <- function (x, values, before = NULL) {
#   if (is.null(before)) {
#     before == 1L
#   }
#   call_append(x, values, after = before - before - 1)
# }
#
#
# ff <- function(x) {
#   if (is.atomic(x)){
#     out <- as.call(c(list(as.symbol("c")), as.list(x)))
#     return(out)
#   }
#
#   ix <- seq_along(x)
#   names(ix) <- names(x)
#   args <- lapply(ix, function(i) call("[[", quote(.data), i))
#   as.call(c(alist(head), args))
#
#   if (is.data.frame(x)) {
#     head <- "data.frame"
#   } else if (is.expression(x)) {
#     head <- "expression"
#   } else if (is.pairlist(x)) {
#     head <- "pairlist"
#   } else if (is.list(x)) {
#     head <- "list"
#   } else {
#     stop()
#   }
#
#   xi <- seq_along(as.list(x))
#   names(xi) <- names(x)
#   out <- lapply(xi, function(i) call("[[", quote(.data), i))
#   as.call(c(list(as.symbol(head)), out))
# }
# ff(as.list(mtcars))
#
#
# function (i, n, names = NULL) {
#   out <- seq_len(n)
#   names(out) <- names
#   if (is_logical(i, n = 1) && !length(out)) {
#     return(out)
#   }
#   unname(out[i])
# }
# function(i, nm = "x") {
#   if(length(i) > 1) {
#     return(lapply(i, sys.function(), nm))
#   }
#   stopifnot(is.chara)
#   out <- call("[[", as.symbol(nm), i)
# }
#
#

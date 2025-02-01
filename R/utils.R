#' Utility functions

.as_function <- function(x, env = globalenv(), ...) {
    if (nargs()) {
        stopifnot(...length() == 0)
    }
    if (is.function(x)) {
        return(x)
    }
    if (inherits(x, "quosure")) {
        stop("Cannot handle `quosures`. Install and use `rlang::as_function()`.")
    }
    if (is.call(x) && deparse(x[[1]]) == "~") {
        if (length(x) > 2) {
            stop("Cannot coerce a two-sided formula to a function.")
        }
        env <- attr(x, ".Environment")
        if (!is.environment(env)) {
            stop("Formula must carry an environment.")
        }
        args <- alist(... = , .x = ..1, .y = ..2, . = ..1)
        fn <- eval(call("function", as.pairlist(args), x[[2]]))
        environment(fn) <- env
        fn <- structure(fn, class = c("rlang_lambda_function", "function"))
        return(fn)
    }
    if (is.character(x) && length(x) == 1) {
        return(get(x, envir = env, mode = "function"))
    }
    stop("Cannot coeerce `x` into a function")
}

.set_names <- function(x, nm = x, ...) {
    n <- length(x)
    stopifnot(length(nm) %in% c(1, n))
    if (n == 1) {
        nm <- rep(nm, n)
    }
    names(x) <- nm
    x
}
paste_line0 <- function (x, .trailing = TRUE) {
    if (.trailing) {
        paste0(x, "\n", collapse = "")
    } else {
        paste(x, collapse = "\n")
    }
}

paste_line <- function (..., .trailing = TRUE) {
    text <- as.character(list(...))
    if (.trailing) {
        paste0(text, "\n", collapse = "")
    } else {
        paste(text, collapse = "\n")
    }
}

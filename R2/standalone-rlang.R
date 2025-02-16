.rlang_compat <- function(fn, try_rlang = TRUE) {
  out <- switch(fn,
    is_installed = return(function(pkg) {
      requireNamespace(pkg,
        quietly = TRUE
      )
    })
  )
  if (try_rlang && requireNamespace("rlang", quietly = TRUE) &&
    environmentIsLocked(asNamespace("rlang"))) {
    switch(fn,
      is_interactive = return(rlang::is_interactive)
    )
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }
  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }
  format_msg <- function(x) paste(x, collapse = "\n")
  switch(fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) {
      warning(format_msg(msg),
        call. = FALSE
      )
    }),
    inform = return(function(msg) message(format_msg(msg)))
  )
  stop(sprintf(
    "Internal error in rlang shims: Unknown function `%s()`.",
    fn
  ))
}
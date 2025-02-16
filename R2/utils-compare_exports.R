compare_exports <- function(pkg, wait = 2) {
  if (is.null(current)) {
    wd <- getwd()
    stopifnot(current_is_package(path = wd))
    current <- basename(wd)
  }
  cat("\nDocumenting and saving current package.\n")
  sleep <- tryCatch(capture.output(devtools::document(wd)),
    error = function(e) e, warning = function(w) w
  )
  message("installing current package.")
  if (inherits(sleep, c("error", "warning"))) {
    cat(
      "\nWarning or Error occurred. Retying in", wait,
      "seconds.\n"
    )
    Sys.sleep(wait)
    capture.output(devtools::document(wd))
  }
  cat("\nInstalling current package.\n")
  sleep <- tryCatch(capture.output(devtools::install(wd)),
    error = function(e) e, warning = function(w) w
  )
  if (inherits(sleep, c("error", "warning"))) {
    cat(
      "\nWarning or Error occurred. Retying in", wait,
      "seconds.\n"
    )
    Sys.sleep(wait)
    capture.output(devtools::install(wd))
  }
  cat("\nRe-loading current package.\n")
  sleep <- tryCatch(capture.output(devtools::load_all(wd)),
    error = function(e) e, warning = function(w) w
  )
  if (inherits(sleep, c("error", "warning"))) {
    cat(
      "\nWarning or Error occurred. Retying in", wait,
      "seconds.\n"
    )
    Sys.sleep(wait)
    capture.output(devtools::load_all(wd))
  }
  ns <- asNamespace(pkg)
  xprts <- Filter(Negate(is.null), mget(getNamespaceExports(ns = ns),
    envir = ns, mode = "function", ifnotfound = list(NULL),
    inherits = FALSE
  ))
  xprts <- Filter(Negate(is_lifecycle), xprts)
  xprts <- xprts[sort(names(xprts))]
  out <- list2env(xprts[setdiff(names(xprts), names(asNamespace(basename(current))))])
  structure(sort(names(out)), .Environment = ns)
}

current_is_package <- function(path = getwd()) {
  testfile <- file.path(path, "DESCRIPTION")
  if (!file.exists(testfile)) {
    return(FALSE)
  }
  if (dir.exists(testfile)) {
    return(FALSE)
  }
  TRUE
}
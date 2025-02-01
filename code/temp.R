files <- gh::gh("/repos/r-lib/rlang/contents/tests/testthat")
files <- files[grep("-fn", sapply(files, getElement, "name"))]
files <- sapply(files, getElement, "download_url")


f <- function(x) {
  if(is.list(x)) {
    unlist(lapply(x, sys.function()))
  } else {
    if(is.call(x) && deparse(x[[1]]) == "test_that") {
      x <- as.list(x)[-1]
      if(is.call(x) && deparse(x[[1]]) == "{") {
        as.list(x)[-1]
      }
    } else {
      NULL
    }
  }
}
nms <- names(call_args(call_args(body(rlang:::is_call_infix))[[1]]))
nms <- nms[nms != ""]
lapply(setNames(nms, nms), as.symbol) |> lapply(\(x) tryCatch(eval(x), error = \(e) NULL)) |>
  unlist()

test_call <- Filter(is.language, as.list(parse(files[[1]])[[1]])[-1])
map_if(test_call, \(x) is_call(x, "{"), )

function (test_call) {
  stopifnot(is_call(test_call, "test_that"))
  x <- call_args(test_call)
  x <- x[vapply(x, is.language, logical(1))]

  i <- vapply(x, is_call, logical(1), "{")

}

is_call_curly <- function (call) {
  is_call(call, "{")
}

is_symbolic

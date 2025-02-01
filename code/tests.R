library(tidyverse)

xprt_purrr <- getNamespaceExports("purrr")
xprt_poorrr <- getNamespaceExports("poorrr")

depr <- lifecycle::pkg_lifecycle_statuses("purrr")[["fun"]]
x <- sort(setdiff(xprt_purrr, depr) |> setdiff(xprt_poorrr))

fns <- mget(grep("is_", x, value = TRUE), asNamespace("rlang"), ifnotfound = list(NULL))
fn_nms <- names(fns)
xx <- strsplit(gsub("^is_", "", fn_nms), "_")
is_x <- unique(vapply(xx[lengths(xx) > 1], `[[`,"",  1))
is_x_nms <- grep(paste0("_", is_x, "_", collapse = "|"), fn_nms, value = TRUE)
is_x_nms <- split(is_x_nms, gsub("is_(.+)_.+$", "\\1", is_x_nms))

rlang_fns <- mget(is_x_nms[[1]], asNamespace("rlang"))
lapply(rlang_fns, body)

x1 <- mget(paste0("is_",xx[lengths(xx) == 1]),
     asNamespace("rlang"),
     ifnotfound = list(NULL))
x1[lengths(x1) == 0] <- mget(names(x1)[lengths(x1) == 0],
                             asNamespace("purrr"),
                             ifnotfound = list(NULL))

heads <- paste0("is_", c("logical", "integer", "double", "complex", "character", "raw")) |>
  mget(asNamespace("rlang")) |>
  map(args)


rlang_is_nms <- sort(grep("^is_", getNamespaceExports("rlang"), value = TRUE))

depr <- function(x) {
  if(is.character(x)) {
    x <- mget(x, asNamespace("rlang"), mode = "function", ifnotfound = list(NULL)) |>
      compact()
  }
  if(is.list(x)) {
    return(map_lgl(x, sys.function()))
  }
  if(typeof(x) != "closure"){
    return(FALSE)
  }
  any(all.names(body(x)) %in% paste0("deprecate_", c("soft", "stop", "warn")))
}
rlang_is_nms <- which(!depr(rlang_is_nms)) |> names()


# |>
#   setdiff(
#     lifecycle::pkg_lifecycle_statuses("rlang") |>
#       as_tibble() |>
#       filter(lifecycle %in%  |>
#       pull(fun)
#   )


names(rlang_is_nms) <- rlang_is_nms
i_bare <- grepl("_bare_", rlang_is_nms)
i_sclr <- grepl("_scalar", rlang_is_nms)

is_std <- list(bare = rlang_is_nms[i_bare],
     scalar = rlang_is_nms[i_bare]) |>
  enframe() |>
  unnest(value) |>
  mutate(value2 = gsub("_bare|_scalar", "", value)) |>
  pull(value2) |>
  unique() |>
  sort() |>
  mget(asNamespace("rlang"), ifnotfound = list(NULL)) |>
  compact() |>
  map(args)


h <- help.search("^is_", fields = "alias", package = "rlang")[["matches"]] |>
  as_tibble() |>
  rename_with(tolower) |>
  select(topic, title, entry)


fn <- function(x) {
  f <- get0(x, asNamespace("rlang"))
  stopifnot(!is.null(f))
  if (x == "is_vector") {
    x <- quote(!is.atomic(x) && !is.list(x))
  } else if (x == "is_atomic") {
    x <- quote(!is.atomic(x) || is.null(x))
  } else {
    x <- call("!", call(gsub("_", ".", x), quote(x)))
  }


  head <- args(f)
  fmls <- formals(head)
  if ("n" %in% names(fmls)) {
    if (length(fmls) == 2) {
      body(head) <- bquote({
        if (.(x)) {
          return(FALSE)
        }
        if (is.null(n)) {
          return(TRUE)
        }
        length(x) %in% n
      })
    } else if (length(fmls) == 3 && "finite" %in% names(fmls)) {
      body(head) <- bquote({
        if (.(x)) {
          return(FALSE)
        }
        if (is.null(n) && is.null(finite)) {
          return(TRUE)
        }
        if (is.null(finite)) {
          return(length(x) %in% n)
        }
        all(is.finite(x)) == finite
      })
    } else {
      body(head) <- call("{", NULL)
    }
  } else {
    body(head) <- call("{", NULL)
  }
  head
}

.fn_as_string <- function(fn, nm, export = TRUE, rdname = NULL) {
  stopifnot(typeof(fn) == "closure")
  body <- body(fn)
  if (is.call(body) && deparse(body[[1]]) == "{") {
    fn_call <- call("function", formals(fn), body)
  } else {
    fn_call <- call("function", formals(fn), call("{", body))
  }
  # call <- call("<-", as.symbol(nm), fn_call)
  out <- paste(deparse(fn_call), collapse = "\n")
  cbind(name = nm,
        rdname = rdname %||% "",
        export = if (export) "#' @export" else "",
        string = paste(nm, out, sep = " <- "))
  # if (export) {
  #   out <- paste0("#' @export\n", out)
  #   if (!is.null(rdname)) {
  #     out <- paste0("# @rdname ", rdname, "\n", out)
  #   }
  # }
  # paste0(out, "\n")
}

string_fns <- function (fns, nms = names(fns), export = TRUE, rdname = NULL) {
  if(is.function(fns)) {
    fns <- list(fns)
  }
  out <- mapply(.fn_as_string, fns, as.list(nms), MoreArgs = list(export = export, rdname = rdname), SIMPLIFY = FALSE)
  do.call("rbind", out)
}

collect_fn_strings <- function(x) {
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  rdname[rdname != ""] <- paste0(rdname, "")
  with(x, paste(rdname[rdname != ""],
                export,
                string,
                sep = "\n"))
}

.fn_name <- function(fn){
  stopifnot(typeof(fn) == "closure")
  ns <- environment(fn)
  nms <- names(ns)
  for (nm in nms) {
    f <- get0(nm, ns, mode = "function") %||% next
    if (identical(fn, f)) {
      return(nm)
    }
  }
}
.fn_call <- function (fn) {
  stopifnot(typeof(fn) == "closure")
  body <- body(fn)
  if (is.call(body) && deparse(body[[1]]) == "{") {
    call("function", formals(fn), body)
  } else {
    call("function", formals(fn), call("{", body))
  }
}

.fn_call_assn <- function (fn, nm = NULL) {
  if (is.null(nm)) {
    nm <- .fn_name(fn)
  }
  call("<-", as.symbol(nm), .fn_call(fn))
}

.is_fn_call <- function (x) {
  is.call(x) && deparse(x[[1]]) == "function"
}
.is_fn_call_assn <- function (x, nm = NULL) {
  if(is.call(x) && deparse(x[[1]]) == "<-") {
    if (!.is_fn_call(x[[3]])) {
      return(FALSE)
    }
    if (is.null(nm)) {
      return(TRUE)
    }
    deparse(x[[2]]) %in% nm
  } else {
    FALSE
  }
}

new_fn_string <- function (fn) {

}

named_fn <- function(fn, nm = NULL) {
  stopifnot(typeof(fn) == "closure")
  if (is.null(nm)) {
    nm <- .fn_name(fn)
  }
  if(is.null(nm) || is.na(nm) || nm == "") {
    stop("incorrect name")
  }
  structure(fn, name = nm)
}



new_sfn <- function(fn, nm = NULL) {

  structure(nm %||% .fn_name(fn),
            rdname = rdname,
            export = export,
            fun = fn,
            class = "sfn")
}
is_sfn <- function(x) {
  inherits(x, "sfn")
}


new_sfns <- function(fns, nms = NULL,  export = TRUE, rdname = NULL) {
  if (is.null(nms)) {
    nms <- names(fns)
    if (is.null(nms) || any(is.na(nms) || nms == "")) {
      nms <- NULL
    }
  }

  nm <- attribures(x)
  data.frame(name = x)

}

print.sfn <- function (x) {
  nm <- x
  attributes(nm) <- NULL
  call <- call("<-", as.symbol(nm), .fn_call(attr(x, "fun")))
  out <- paste0(deparse(call), collapse = "\n")

  xprt <- rdnm <- NULL
  if(attr(x, "export")) {
    xprt <- "#' @export\n"
  }
  if (!is.null(attr(x, "rdname"))) {
    rdnm <- paste0("#' @rdname ", rdnm, "\n")
  }
  cat(paste0(rdnm, xprt, out, "\n"), sep = "")
  invisible(x)
}




new_fn_string(is_atomic)

.fn_string <- function(fn, .string = TRUE) {
  if (is.call(fn)) {
    out <- deparse(fn)
  } else {
    out <- deparse(.fn_call(fn))
  }
  if (!.string) {
    return(out)
  }
  paste(out, collapse = "\n")
}



new_fn_string <- function(fn, nm = NULL, export = TRUE, rdname = NULL) {
  if (is.null(fn)) {
    nm <- .fn_name(fn)
  }

}

filter(h, entry %in% names(is_std) & grepl("^type-predicate", topic)) |>
  arrange(entry) |>
  pull(entry) |>
  set_names() |>
  map(fn) |>
  string_fns() |>
  as_tibble() |>
  nest(.by = name) |>
  deframe() |>
  map(as.list)
|>
  collect_fn_strings()
|>
  cat(sep = "\n")
|>
  apply(1, function())


  as_tibble()

  cat(sep = "\n")
  map_chr(fn_as_string) |>
  cat(sep = "\n")


filter(h, entry %in% names(is_std) & grepl("^type-predicate", topic)) |>
  arrange(entry) |>
  pull(entry) |>
  mget(asNamespace("rlang")) |>
  map(args) |>
  map(~ call("function", formals(.x), call("{", body(.x)))) |>
  map(deparse) |>
  map_chr(paste, collapse = "\n") |>
  imap_chr(~ paste0("#' @export\n", .y, " <- ", .x)) |>
  enframe(name = "entry") |>
  left_join(h) |>
  mutate(value = paste0("#' @rdname ", topic, "\n", value))|>
  summarise(value = paste(value, collapse = "\n"),
            .by = title) |>
  pull(value) |> cat()
|>
  mutate(title = paste(value, collapse = "\n"))
  mutate()



h <- help.search("^is_", fields = "alias", package = "rlang")[["matches"]] |>
  as_tibble() |>
  rename_with(tolower) |>
  select(topic, title, entry) |>
  filter(entry %in% rlang_is_nms) |>
  mutate(
    obj = mget(entry, asNamespace("rlang"), ifnotfound = list(NULL)),
    fmls = map_if(obj, ~ typeof(.x) == "closure", formals),
    fml_names = map(fmls, names),
    body = map_if(obj, ~ typeof(.x) == "closure", body),
    entry = setNames(entry, topic),
    title = setNames(title, topic),
    topic = structure(topic, title = title)) |>
  select(-title)



h |>
  mutate(fml_nms = map_vec(fml_names, paste, collapse = ", ")) |>
  summarise(
    entry = list(entry),
    fml_names = list(fml_names),
    .by = fml_nms
  ) |>
  mutate(
    entry_nms = map_chr(entry, paste, collapse = ", ")
  )


rlang_is_bare <- rlang_is_nms[i_bare]
h |> filter(entry %in% rlang_is_bare & grepl("predicate", topic))
heads_is_bare <- map(mget(rlang_is_bare, asNamespace("rlang")), args)

paste0("is_", c("logical", "integer", "double", "complex", "character", "raw")) |>
  mget(asNamespace("rlang")) |>
  map(args) |>
  map( ~ names(formals(.x))) |>
  imap(~ if (length(.x) == 2 && "n" %in% .x) {fn(gsub("_", ".", .y))} else {.x})



rlang_is_sclr <- rlang_is_nms[i_sclr]
rlang_is_atom <- rlang_is_nms[paste0("is_", c("atomic", "logical", "integer", "double", "complex", "character", "raw"))]




nn <- lengths(strsplit(rlang_is_nms, "_")) |> setNames(rlang_is_nms)

rlang_is_fns <- mget(rlang_is_nms, asNamespace("rlang"))



p <- map(gsub("_", ".",names(heads)), ~ call(.x, quote(x)))
map(p, ~ call("&&", quote(length(x) %in% n), .x))

x <- paste0("is_", c("logical", "integer", "double", "character", "raw", "complex")) |>
  poorrr:::.set_names() |> map(fn)
x




fn2 <- function(x) {
  x <- as.symbol(x)
  bquote({
    if (!.(x)(x)) {
      return(FALSE)
    }
    if (is.null(n) && is.null(finite)) {
      return(TRUE)
    }
    if (is.null(finite)) {
      return(length(x) %in% n)
    }
      all(is.finite(x)) == finite
  })
}
fn2("is.double")

is_atomic <- function(x, n = NULL) {
  if (!is.atomic(x) || is.null(x)) {
    return(FALSE)
  }
  if (is.null(n)) {
    return(TRUE)
  }
  length(x) %in% n
}


names(fns)
purrr:::simplify_impl

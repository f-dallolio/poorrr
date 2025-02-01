
#' @export
new_call <- function(car, cdr = NULL) {
  stopifnot(is_callable(car))
  as.call(c(car, cdr))
}

#' @export
new_call2 <- function(f, ..., .args = NULL) {
  if (is_string(f)) {
    f <- as.symbol(f)
  }
  as.call(c(f, ..., .args))
}

#' @export
new_ns_symbol <- function(.fn, .ns = NULL, .private = FALSE) {
  if (.private) {
    op <- ":::"
  } else {
    op <- "::"
  }
  if(is_string(.fn)) {
    .fn <- .call_name_chr(.fn)
  }
  if(is_call(.fn, c("::", ":::"))) {
    .fn <- .fn[[3]]
  }
  if(is_string(.ns)) {
    .ns <- as.symbol(.ns)
  }
  call(op, .ns, .fn)
}

#' @export
call2 <- function (.fn, ..., .args = NULL, .ns = NULL, .private = FALSE) {
  if(is_string(.fn)) {
    .fn <- .call_name_chr(.fn)
  }
  if(is_call(.fn, c("::", ":::"))) {
    if(is.null(.ns) ) {
      if(identical(.fn[[1]], quote(`:::`))) {
        .private = TRUE
        .ns <- .fn[[2]]
      }
    }
    .fn <- .fn[[3]]
  }

  if(!is.null(.ns)) {
    if(is_string(.ns)) {
      .ns <- as.symbol(.ns)
    }
    .fn <- new_ns_symbol(.fn, .ns, .private)
  }
  as.call(c(.fn, c(list(...), .args)))
}

#' @export
call_name <- function(call) {
  if (is.expression(call)) {
    call <- as.list(call)
  }
  if(is.list(call)) {
    out <- lapply(call, sys.function())
    return(out)
  }
  out <- call[[1]]
  if(is.symbol(out)) {
    deparse(out)
  } else {
    deparse(out[[3]])
  }
}
#' @export
call_name_set <- function(call, name, ns = NULL, private = FALSE) {
  if(is_string(name)) {
    name <- .call_name_chr(name)
  }
  if(is_call(name, c("::", ":::"))) {
    if(identical(name[[1]], `:::`)) {
      private <- TRUE
    }
    ns <- name[[2]]
    name <- name[[3]]
  }
  out <- new_call(name, as.pairlist(call_args(call)))
  if (is.null(ns)) {
    return(out)
  }
  call_ns_set(out, ns, private)
}

.call_name_chr <- function(x) {
  stopifnot(is_string(x))
  if (grepl("^.+[:]{2,3}.+$", x)) {
    str2lang(x)
  } else {
    as.symbol(x)
  }
}


#' @export
call_args <- function (call, ...) {
  out <- as.list(call)[-1]
  if (is.null(names(out))) {
    names(out) <- rep("", length(out))
  }
  i <- list(...)
  if(length(i)) {
    lapply(i, `[[`, out)
  } else {
    out
  }
}
#' @export
call_args_len <- function(call, ...) {
  length(as.list(call)) - 1
}
#' @export
call_args_names <- function (call, ...) {
  names(call_args(call))
}
#' @export
call_args_syms <-  function (call, ...) {
  nms <- call_args_names(call)
  out <- lapply(nms, as.symbol)
  names(out) <- rep("", length(out))
  i <- nms != "..."
  names(out)[i] <- nms[i]
  out
}

#' @export
call_ns <- function (call) {
  out <- call[[1]]
  if(is.call(out) && deparse(out[[1]]) %in% c("::", ":::")) {
    deparse(out[[2]])
  } else {
    NULL
  }
}
#' @export
call_ns_set <-  function(call, ns, private = FALSE) {
  if(is_string(ns)) {
    ns <- as.symbol(ns)
  }
  stopifnot(is_symbolic(ns))
  if(private) {
    op <- ":::"
  } else {
    op <- "::"
  }
  name <- call("::", ns, as.symbol(call_name(call)))
  new_call(name, as.pairlist(call_args(call)))
}


#' @export
is_call_assn <- function (x, .call_name = NULL) {
  if(!is_call(x, "<-")) {
    return(NULL)
  }
  if(is.null(.call_name)) {
    return(TRUE)
  }
  is_call(call_assn_get_value(x), name = .call_name)
}


#' @export
is_call_function <- function(call) {
  is_call(call, name = "function")
}
#' @export
is_call_tilde <- function(call) {
  is_call(call, name = "~")
}


#' @export
call_assn_get_value <- function(x) {
  if(is_call_assn(x)) {
    if(!is_call_assn(x[[3]])) {
      return(x)
    }
    return(call_assn_get_value(x))
  }
  NULL
}
#' @export
call_assn_get_name <- function(x) {
  if(!is_call_assn(x)) {
    return(NULL)
  }
  name <- deparse(x[[2]])
  x <- x[[3]]
  if(!is_simple_assn(x)) {
    return(name)
  }
  c(name, call_assn_get_name(x))
}





#
# split_call_assign(call)
#
#
# split_call_assign <- function(call) {
#   e <- new.env(hash = FALSE)
#   eval(call, e)
#   out <- mget(names(e), e)
#   return(out)
#   # if(length(out) == 1) {
#   #   structure(out, names = names(out)[[1]], alias = names(out)[-1])
#   # }
#   # if(length(out) > 1 && )
#   # if(length(unique(out)) == 1) {
#   #   out[1]
#   # }
#
# }
# attributes(quote(~mean))
# rlang::is_bare_formula()
# #   args <- call_args(call)
# #   out <- deparse(args[[1]])
# #   call <- args[[length(args)]]
# #   if(is_call_assign(call)) {
# #     c(out, split_call_assign(call)
# #   } else {
# #     return(list(out, call))
# #   }
# #   list(name = unlist(out[-length(out)]),
# #        call = out[[length(out)]])
# # }
#
# call <- call("<-", quote(x), call("<-", quote(y), quote(function(x) base::mean(x, na.rm = T))))
# unique(split_call_assign(call))
#
#
# repo_urls <- function(repo, dir = "R", pattern = "download_url") {
#   endpoint0 <- sprintf("/repos/%s/contents/%s", repo, dir)
#   out <- unlist(lapply(endpoint0, gh::gh))
#   id <- grep(pattern, names(out))
#   unname(out[id])
# }
#
# repo_exports <- function(repo) {
#   url <- repo_urls(repo, "NAMESPACE")
#   lines <- readLines(url)
#   x <- grep("^export\\(.+\\)", lines, value = TRUE)
#   x <- as.list(str2expression(x))
#   as.character(lapply(x, .subset2, 2))
# }
#
# parse_to_list <- function(...) {
#   tmp <- new.env()
#   eval(parse(...), tmp)
#   mget(names(tmp), envir = tmp)
# }
#
# # repo_objs <- function (repo) {
# #
# #
# # }
# #
# # # repo_standalone <- function (url) {
# #   if(length(url) > 1) {
# #     out <- lapply(url, sys.function())
# #     names(out) <- gsub("[.].+$", "", basename(url))
# #     structure(out, url = url)
# #   }
# #   print(url)
# #   temp_env <- new.env()
# #   eval(parse(url, keep.source = FALSE), temp_env)
# #   temp_env
# # }
#
# # x <- list(letters[1:5], LETTERS[1:4])
# # lapply(seq_len(max(lengths(x))), rep, list(), length(x))
# # rep(list(vector("list", length(x))), 5)
# # repo <- "r-lib/rlang"
# # dir <- "NAMESPACE"
# # dir <- "R"
# # pattern = "download_url"
# #
# #
# # repo <- "r-lib/rlang"
# # repo_urls(repo)
# # url <- repo_get(repo)[[5]]
# #
# #
# # endpoint0 <- sprintf("/repos/%s/contents/", repo)
# # url <- gh::gh(file.path(endpoint0, "NAMESPACE"))[["download_url"]]
# # repo_exports(url)
# # ?environment



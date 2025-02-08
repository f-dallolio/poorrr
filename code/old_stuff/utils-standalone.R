#' Tools for managing standalone files from Github.
#' @name utils-standalone
NULL

#' @rdname utils-standalone
#' @export
standalone_urls <- function(repo, sort = TRUE) {
  json <- gh::gh("/repos/{repo_spec}/contents/{path}", repo_spec = repo, path = "R/")
  urls <- vapply(json, getElement, "", "download_url")
  names(urls) <- basename(urls)
  urls <- urls[grepl("^standalone-", names(urls))]
  names(urls) <- gsub("^standalone-|.[Rr]$", "", names(urls))
  if (sort) {
    sort(urls)
  } else {
    urls
  }
}
#' @rdname utils-standalone
#' @export
standalone_choices <- function(repo, sprt = TRUE) {
  names(standalone_urls(repo))
}
#' @rdname utils-standalone
#' @export
standalone_select <- function(nm, repo, use_grep = TRUE, sort = TRUE, simplify = TRUE) {
  choices <- standalone_choices(repo, sort)
  if (!use_grep) {
    if (sort) {
      nm <- sort(nm)
    }
    return(choices[nm])
  }
  names(nm) <- nm
  out <- lapply(nm, grep, choices, value = TRUE)
  n <- lengths(out)
  if (simplify) {
    return(unlist(out))
  }
  out
}
#' @rdname utils-standalone
#' @export
standalone_get <- function(repo,
                           nm = ".",
                           dest_dir = "code/standalone",
                           ...,
                           use_grep = TRUE, sort = TRUE, simplify = TRUE) {
  sel <- standalone_select(nm, repo, use_grep, sort, simplify)
  urls <- standalone_urls(repo)[sel]
  choices <- names(urls)
  choices2 <- format(dQuote(choices))
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir)
  }
  dest_dir <- normalizePath(gsub("^[/]*", "", dest_dir))
  paths <- file.path(dest_dir, basename(urls))
  paths2 <- format(dQuote(paths, q = FALSE))
  for (i in seq_along(urls)) {
    url <- urls[[i]]
    path <- paths[[i]]
    name <- choices[[i]]
    msg <- sprintf("Downloading   %s   to:\n", dQuote(name))
    cat(msg)
    download.file(url, path, quiet = TRUE)
    cat(dQuote(path, q = FALSE), "\n\n")
  }
  invisible(urls)
}
#' @rdname utils-standalone
#' @export
choose_standalones <- function(repo,
                               dest_dir = "R",
                               multiple = TRUE,
                               ...,
                               use_grep = TRUE, sort = TRUE, simplify = TRUE) {
  urls <- standalone_urls(repo = repo)
  urls_nms <- names(urls)
  title <- sprintf("Pick a standalone file from repo:   %s. ", dQuote(repo, q = FALSE))
  nm <- select.list(c(urls_nms, "ALL"), title = title, multiple = multiple)
  if ("ALL" %in% nm) {
    nm <- names(urls)
  }
  standalone_get(repo, nm, dest_dir,
    use_grep = use_grep,
    sort = sort, simplify = simplify
  )
}
#' @rdname utils-standalone
#' @export
local_standalones <- function(dir,
                              pattern = "standalone-",
                              full_path = FALSE) {
  files <- list.files(dir, full.names = TRUE)
  paths <- grep(pattern, files, value = TRUE)
  if (full_path) {
    paths <- normalizePath(paths)
  }
  names(paths) <- gsub("[.].+$", "", basename(paths))
  paths
}
#' @rdname utils-standalone
#' @export
standalone_info <- function(repo, nm = NULL, use_grep = TRUE, sort = FALSE, simplify = TRUE) {
  urls <- standalone_urls(repo)
  if (!is.null(nm)) {
    urls <- urls[standalone_select(nm, sort = FALSE)]
  }
  out <- standalone_get_info(urls)
  structure(out, repo = repo, url = urls, class = c("standalone_info", "tbl", "data.frame"))
}
standalone_get_info <- function(con, text = NULL) {
  if (length(con) > 1) {
    nms <- gsub("^standalone-|.[Rr]$", "", basename(con))
    out <- lapply(con, sys.function())
    df <- data.frame(standalone = rep(nms, sapply(out, nrow)))
    df <- cbind(df, do.call("rbind", out))
    row.names(df) <- NULL
    return(df)
  }
  x <- eval_assn_call(parse(con))
  env_info(list2env(x), sort = FALSE)
}

#' @rdname utils-standalone
#' @export
print.standalone_info <- function(x) {
  arg <- caller_arg(x)
  repo <- attr(x, "repo")
  urls <- attr(x, "url")

  nms <- format(unique(x$standalone))
  ids <- format(seq_along(nms))
  # ids <- gsub(" ", "0", format(seq_along(nms)))
  nms <- sprintf("%s: %s", ids, nms)
  class(x) <- class(x)[-1]

  cat("<Standalone Info Table>\n")
  print(x)
  cat(sprintf("\nRepository: \"%s\".\n\n", repo))
  cat(nms, fill = TRUE)
  cat(sprintf("\nUse `attr(%s, \"url\")` to retrive download urls.\n\n", arg))
  invisible(x)
}

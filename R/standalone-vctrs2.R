# nocov start
# Coercion ----
vec_can_cast <- function(x, to) {
  out <- tryCatch(vec_cast(x, to), error = function(e) NULL)
  !is.null(out)
}

# Names ----
vec_names2 <- function(x, ..., repair = c("minimal", "unique", "check_unique", "universal")) {
  nms <- vec_names(x)
  if (is.null(nms)) {
    nms <- rep("", vec_size(x))
  }
  vec_as_names(nms, repair = repair)
}
vec_names <- function(x) {
  if (is.data.frame(x)) {
    nms <- attr(x, "row.names")
    if (is.numeric(nms)) {
      return(NULL)
    }
    return(nms)
  }
  if (is.array(x)) {
    return(dimnames(x)[[1]])
  }
  names(x)
}
vec_set_names <- function(x, names = NULL) {
  if (is.data.frame(x)) {
    row.names(x) <- names
  } else if (is.array(x)) {
    dimnames(x)[[1]] <- names
  } else {
    names(x) <- names
  }
  x
}
vectbl_set_names <- function(x, names = NULL) {
  if (inherits(x, "vctrs_rcrd")) {
    return(x)
  }
  vec_set_names(x, names)
}


vec_as_names <- function(names, ..., repair = c("minimal", "unique", "check_unique", "universal")) {
  .vctrs_make_syntactic <- function(names) {
    names[is.na(names)] <- ""
    names[names == ""] <- "."
    names[names == "..."] <- "...."
    names <- sub("^_", "._", names)

    new_names <- make.names(names)

    X_prefix <- grepl("^X", new_names) & !grepl("^X", names)
    new_names[X_prefix] <- sub("^X", "", new_names[X_prefix])
    dot_suffix <- which(new_names == paste0(names, "."))
    new_names[dot_suffix] <- sub("^(.*)[.]$", ".\\1", new_names[dot_suffix])

    regex <- paste0(
      "^(?<leading_dots>[.]{0,2})",
      "(?<numbers>[0-9]*)",
      "(?<leftovers>[^0-9]?.*$)"
    )
    re <- .vctrs_re_match(new_names, pattern = regex)

    needs_dots <- which(re$numbers != "")
    needs_third_dot <- re$leftovers[needs_dots] == ""
    re$leading_dots[needs_dots] <- ifelse(needs_third_dot, "...", "..")

    new_names <- paste0(re$leading_dots, re$numbers, re$leftovers)
    new_names
  }
  .vctrs_make_unique <- function(x) {
    x <- gsub("^[.]+[0-9]*$", "", x)
    x[is.na(x) | .name_is_dots(x)] <- ""
    i <- .name_is_duplicate(x)
    if (all(!i)) {
      return(x)
    }
    x[i] <- paste0(x[i], "...", seq_along(x)[i])
    x
  }
  .vctrs_make_minimal <- function(x) {
    x[is.na(x)] <- ""
    x
  }
  switch(match.arg(repair),
         minimal = .vctrs_make_minimal(names),
         unique = .vctrs_make_unique(names),
         check_unique = stopifnot(identical(names, .vctrs_make_unique(names))),
         universal = .vctrs_make_unique(.vctrs_make_syntactic(names))
  )
}

.vctrs_re_match <- function(text, pattern, perl = TRUE, ...) {
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)
  match <- regexpr(pattern, text, perl = T)

  start <- as.vector(match)
  length <- attr(match, "match.length")
  end <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[start == -1] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE, .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {
    gstart <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend <- gstart + glength - 1L
    groupstr <- substring(text, gstart, gend)
    groupstr[gstart == -1] <- NA_character_
    dim(groupstr) <- dim(gstart)
    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}
.name_is_void <- function(x) {
  x == "" | is.na(x)
}
.name_is_dots <- function(x) {
  grepl("^[.]{2,3}[0-9]*$", x)
}
.name_is_duplicate <- function(x) {
  nn <- table(x)
  x %in% names(nn)[nn > 1]
}


# Predicates ----
obj_is_list <- function(x) {
  is_bare_list(x) || inherits(x, "list")
}

obj_is_vector <- function(x) {
  if (exists("vec_proxy", envir = parent.frame(), mode = "function")) {
    typeof(vec_proxy(x)) == "list" || is_atomic(x)
  } else {
    is_atomic(x) || obj_is_list(x) || is.data.frame(x)
  }
}

list_all_vectors <- function(x) {
  obj_check_list(x)
  all(vapply(x, obj_is_vector, logical(1)))
}

list_all_size <- function(x, size) {
  obj_check_list(x)
  all(vapply(x, vec_size, numeric(1)) == 1)
}
# nocov end

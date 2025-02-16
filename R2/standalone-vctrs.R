data_frame <- function(...) {
  new_data_frame(df_list(...), .class = "tbl")
}

new_tbl <- function(.x = list(),
                    ...,
                    .size = NULL,
                    .class = NULL) {
  new_data_frame(.x, .size = .size, .class = c(.class, "tbl"))
}

new_data_frame <- function(.x = list(),
                           ...,
                           .size = NULL,
                           .class = NULL) {
  n_cols <- length(.x)
  if (n_cols != 0 && is.null(names(.x))) {
    stop("Columns must be named.", call. = FALSE)
  }

  if (is.null(.size)) {
    if (n_cols == 0) {
      .size <- 0
    } else {
      .size <- vec_size(.x[[1]])
    }
  }

  structure(
    .x,
    class = c(.class, "data.frame"),
    row.names = .set_row_names(.size),
    ...
  )
}

df_list <- function(..., .size = NULL) {
  vec_recycle_common(list(...), size = .size)
}


vec_rbind <- function(...) {
  xs <- vec_cast_common(list(...))
  do.call(base::rbind, xs)
}

vec_cbind <- function(...) {
  xs <- list(...)
  ptype <- vec_ptype_common(lapply(xs, `[`, 0))
  class <- setdiff(class(ptype), "data.frame")
  xs <- vec_recycle_common(xs)
  out <- do.call(base::cbind, xs)
  new_data_frame(out, .class = class)
}

vec_size <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
}

vec_rep <- function(x, times) {
  i <- rep.int(seq_len(vec_size(x)), times)
  vec_slice(x, i)
}

vec_recycle_common <- function(xs) {
  sizes <- vapply(xs, vec_size, integer(1))
  n <- unique(sizes)
  if (length(n) == 1 && is.null(size)) {
    return(xs)
  }
  n <- setdiff(n, 1L)
  ns <- length(n)
  if (ns == 0) {
    if (is.null(size)) {
      return(xs)
    }
  } else if (ns == 1) {
    if (is.null(size)) {
      size <- n
    } else if (n != size) {
      stop("Inputs can't be recycled to `size`.", call. = FALSE)
    }
  } else {
    stop("Inputs can't be recycled to a common size.", call. = FALSE)
  }
  to_recycle <- sizes == 1L
  xs[to_recycle] <- lapply(xs[to_recycle], vec_rep, size)
  xs
}

vec_slice <- function(x, i) {
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.numeric(i) || is.character(i))
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    out <- x[i, 0, drop = FALSE]
    out[seq_along(x)] <- lapply(x, vec_slice, i)
    if (is.numeric(attr(x, "row.names"))) {
      row_names <- .set_row_names(nrow(out))
    } else {
      row_names <- attr(out, "row.names")
    }
    mtd <- .rlang_vctrs_s3_method("[", class(x))
    if (is_null(mtd) || identical(environment(mtd), asNamespace("base"))) {
      attrib <- attributes(x)
      attrib$row.names <- row_names
      attributes(out) <- attrib
    }
    return(out)
  }
  d <- vec_dims(x)
  if (d == 1) {
    if (is.object(x)) {
      out <- x[i]
    } else {
      out <- x[i, drop = FALSE]
    }
  } else if (d == 2) {
    out <- x[i, , drop = FALSE]
  } else {
    j <- rep(list(quote(expr = )), d - 1)
    out <- eval(as.call(list(quote(`[`), quote(x), quote(i),
      j,
      drop = FALSE
    )))
  }
  mtd <- .rlang_vctrs_s3_method("[", class(x))
  if (is_null(mtd) || identical(environment(mtd), asNamespace("base"))) {
    attrib <- attributes(x)
    attrib$names <- attr(out, "names")
    attrib$dim <- attr(out, "dim")
    attrib$dim.names <- attr(out, "dim.names")
    attributes(out) <- attrib
  }
  out
}

vec_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}

vec_as_location <- function(i, n) {
  out <- seq_len(n)
  names(out) <- names
  if (is_logical(i, n = 1) && !length(out)) {
    return(out)
  }
  unname(out[i])
}

vec_init <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}

vec_assign <- function(x, i, value) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.numeric(i) || is.character(i))
  value <- vec_recycle(value, vec_size(i))
  value <- vec_cast(value, to = x)
  d <- vec_dims(x)
  if (d == 1) {
    x[i] <- value
  } else if (d == 2) {
    x[i, ] <- value
  } else {
    stop("Can't slice-assign arrays.", call. = FALSE)
  }
  x
}

vec_recycle <- function(x, size) {
  if (is.null(x) || is.null(size)) {
    return(NULL)
  }
  n_x <- vec_size(x)
  if (n_x == size) {
    x
  } else if (size == 0L) {
    vec_slice(x, 0L)
  } else if (n_x == 1L) {
    vec_slice(x, rep(1L, size))
  } else {
    stop("Incompatible lengths: ", n_x, ", ", size, call. = FALSE)
  }
}

vec_cast_common <- function(xs) {
  ptype <- vec_ptype_common(xs, ptype = to)
  lapply(xs, vec_cast, to = ptype)
}

vec_cast <- function(x, to) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(to)) {
    return(x)
  }
  if (vec_is_unspecified(x)) {
    return(vec_init(to, vec_size(x)))
  }
  stop_incompatible_cast <- function(x, to) {
    stop(sprintf(
      "Can't convert <%s> to <%s>.", .rlang_vctrs_typeof(x),
      .rlang_vctrs_typeof(to)
    ), call. = FALSE)
  }
  lgl_cast <- function(x, to) {
    lgl_cast_from_num <- function(x) {
      if (any(!x %in% c(0L, 1L))) {
        stop_incompatible_cast(x, to)
      }
      as.logical(x)
    }
    switch(.rlang_vctrs_typeof(x),
      logical = x,
      integer = ,
      double = lgl_cast_from_num(x),
      stop_incompatible_cast(
        x,
        to
      )
    )
  }
  int_cast <- function(x, to) {
    int_cast_from_dbl <- function(x) {
      out <- suppressWarnings(as.integer(x))
      if (any((out != x) | xor(is.na(x), is.na(out)))) {
        stop_incompatible_cast(x, to)
      } else {
        out
      }
    }
    switch(.rlang_vctrs_typeof(x),
      logical = as.integer(x),
      integer = x,
      double = int_cast_from_dbl(x),
      stop_incompatible_cast(
        x,
        to
      )
    )
  }
  dbl_cast <- function(x, to) {
    switch(.rlang_vctrs_typeof(x),
      logical = ,
      integer = as.double(x),
      double = x,
      stop_incompatible_cast(x, to)
    )
  }
  chr_cast <- function(x, to) {
    switch(.rlang_vctrs_typeof(x),
      character = x,
      stop_incompatible_cast(
        x,
        to
      )
    )
  }
  list_cast <- function(x, to) {
    switch(.rlang_vctrs_typeof(x),
      list = x,
      stop_incompatible_cast(
        x,
        to
      )
    )
  }
  df_cast <- function(x, to) {
    if (length(setdiff(names(x), names(to))) > 0) {
      stop("Can't convert data frame because of missing columns.",
        call. = FALSE
      )
    }
    out <- as.list(x)
    common <- intersect(names(x), names(to))
    out[common] <- Map(vec_cast, out[common], to[common])
    from_type <- setdiff(names(to), names(x))
    out[from_type] <- lapply(to[from_type], vec_init, n = vec_size(x))
    out <- out[names(to)]
    new_data_frame(out)
  }
  rlib_df_cast <- function(x, to) {
    new_data_frame(df_cast(x, to), .class = "tbl")
  }
  tib_cast <- function(x, to) {
    new_data_frame(df_cast(x, to), .class = c("tbl_df", "tbl"))
  }
  switch(.rlang_vctrs_typeof(to),
    logical = lgl_cast(x, to),
    integer = int_cast(x, to),
    double = dbl_cast(x, to),
    character = chr_cast(x, to),
    list = list_cast(x, to),
    base_data_frame = df_cast(x, to),
    rlib_data_frame = rlib_df_cast(
      x,
      to
    ),
    tibble = tib_cast(x, to),
    stop_incompatible_cast(
      x,
      to
    )
  )
}

vec_can_cast <- function(x, to) {
  out <- tryCatch(vec_cast(x, to), error = function(e) NULL)
  !is.null(out)
}

vec_ptype_common <- function(xs) {
  if (!is.null(ptype)) {
    return(vec_ptype(ptype))
  }
  xs <- Filter(function(x) !is.null(x), xs)
  if (length(xs) == 0) {
    return(NULL)
  }
  if (length(xs) == 1) {
    out <- vec_ptype(xs[[1]])
  } else {
    xs <- map(xs, vec_ptype)
    out <- Reduce(vec_ptype2, xs)
  }
  vec_ptype_finalise(out)
}

vec_ptype_finalise <- function(x) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, vec_ptype_finalise)
    return(x)
  }
  if (inherits(x, "rlang_unspecified")) {
    logical()
  } else {
    x
  }
}

vec_ptype <- function(x) {
  if (vec_is_unspecified(x)) {
    return(.rlang_vctrs_unspecified())
  }
  if (is.data.frame(x)) {
    out <- new_data_frame(lapply(x, vec_ptype))
    attrib <- attributes(x)
    attrib$row.names <- attr(out, "row.names")
    attributes(out) <- attrib
    return(out)
  }
  vec_slice(x, 0)
}

vec_ptype2 <- function(x, y) {
  stop_incompatible_type <- function(x, y) {
    stop(sprintf(
      "Can't combine types <%s> and <%s>.", .rlang_vctrs_typeof(x),
      .rlang_vctrs_typeof(y)
    ), call. = FALSE)
  }
  x_type <- .rlang_vctrs_typeof(x)
  y_type <- .rlang_vctrs_typeof(y)
  if (x_type == "unspecified" && y_type == "unspecified") {
    return(.rlang_vctrs_unspecified())
  }
  if (x_type == "unspecified") {
    return(y)
  }
  if (y_type == "unspecified") {
    return(x)
  }
  df_ptype2 <- function(x, y) {
    set_partition <- function(x, y) {
      list(both = intersect(x, y), only_x = setdiff(
        x,
        y
      ), only_y = setdiff(y, x))
    }
    x <- as.list(vec_slice(x, 0))
    y <- as.list(vec_slice(y, 0))
    names <- set_partition(names(x), names(y))
    if (length(names$both) > 0) {
      common_types <- Map(vec_ptype2, x[names$both], y[names$both])
    } else {
      common_types <- list()
    }
    only_x_types <- x[names$only_x]
    only_y_types <- y[names$only_y]
    out <- c(common_types, only_x_types, only_y_types)
    out <- out[c(names(x), names$only_y)]
    new_data_frame(out)
  }
  rlib_df_ptype2 <- function(x, y) {
    new_data_frame(df_ptype2(x, y), .class = "tbl")
  }
  tib_ptype2 <- function(x, y) {
    new_data_frame(df_ptype2(x, y), .class = c(
      "tbl_df",
      "tbl"
    ))
  }
  ptype <- switch(x_type,
    logical = switch(y_type,
      logical = x,
      integer = y,
      double = y,
      stop_incompatible_type(x, y)
    ),
    integer = switch(.rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = y,
      stop_incompatible_type(
        x,
        y
      )
    ),
    double = switch(.rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = x,
      stop_incompatible_type(
        x,
        y
      )
    ),
    character = switch(.rlang_vctrs_typeof(y),
      character = x,
      stop_incompatible_type(x, y)
    ),
    list = switch(.rlang_vctrs_typeof(y),
      list = x,
      stop_incompatible_type(x, y)
    ),
    base_data_frame = switch(.rlang_vctrs_typeof(y),
      base_data_frame = ,
      s3_data_frame = df_ptype2(
        x,
        y
      ),
      rlib_data_frame = rlib_df_ptype2(x, y),
      tibble = tib_ptype2(
        x,
        y
      ),
      stop_incompatible_type(x, y)
    ),
    rlib_data_frame = switch(.rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      s3_data_frame = rlib_df_ptype2(
        x,
        y
      ),
      tibble = tib_ptype2(x, y),
      stop_incompatible_type(
        x,
        y
      )
    ),
    tibble = switch(.rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      tibble = ,
      s3_data_frame = tib_ptype2(x, y),
      stop_incompatible_type(
        x,
        y
      )
    ),
    stop_incompatible_type(x, y)
  )
  vec_slice(ptype, 0)
}

.rlang_vctrs_typeof <- function(x) {
  if (is.object(x)) {
    class <- class(x)
    if (identical(class, "rlang_unspecified")) {
      return("unspecified")
    }
    if (identical(class, "data.frame")) {
      return("base_data_frame")
    }
    if (identical(class, c("tbl", "data.frame"))) {
      return("rlib_data_frame")
    }
    if (identical(class, c("tbl_df", "tbl", "data.frame"))) {
      return("tibble")
    }
    if (inherits(x, "data.frame")) {
      return("s3_data_frame")
    }
    class <- paste0(class, collapse = "/")
    stop(sprintf("Unimplemented class <%s>.", class), call. = FALSE)
  }
  type <- typeof(x)
  switch(type,
    `NULL` = return("null"),
    logical = if (vec_is_unspecified(x)) {
      return("unspecified")
    } else {
      return(type)
    },
    integer = ,
    double = ,
    character = ,
    raw = ,
    list = return(type)
  )
  stop(sprintf("Unimplemented type <%s>.", type), call. = FALSE)
}

vec_is_unspecified <- function(x) {
  !is.object(x) && typeof(x) == "logical" && length(x) && all(vapply(
    x,
    identical, logical(1), NA
  ))
}

.rlang_vctrs_unspecified <- function() {
  structure(rep(NA, length(x)), class = "rlang_unspecified")
}

.rlang_vctrs_s3_method <- function(generic, class, env = parent.frame()) {
  fn <- get(generic, envir = env)
  ns <- asNamespace(topenv(fn))
  tbl <- ns$.__S3MethodsTable__.
  for (c in class) {
    name <- paste0(generic, ".", c)
    if (exists(name, envir = tbl, inherits = FALSE)) {
      return(get(name, envir = tbl))
    }
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      return(get(name, envir = globalenv()))
    }
  }
  NULL
}

vec_names2 <- function(x, ..., repair = c(
           "minimal", "unique", "check_unique",
           "universal"
         )) {
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

vec_as_names <- function(names, ..., repair = c(
           "minimal", "unique", "check_unique",
           "universal"
         )) {
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
      "^(?<leading_dots>[.]{0,2})", "(?<numbers>[0-9]*)",
      "(?<leftovers>[^0-9]?.*$)"
    )
    re <- .vctrs_re_match(new_names, pattern = regex)
    needs_dots <- which(re$numbers != "")
    needs_third_dot <- re$leftovers[needs_dots] == ""
    re$leading_dots[needs_dots] <- ifelse(needs_third_dot,
      "...", ".."
    )
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
    check_unique = stopifnot(identical(
      names,
      .vctrs_make_unique(names)
    )),
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

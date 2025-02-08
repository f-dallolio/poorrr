vec_cast_common <- function(xs, to = NULL) {
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
  switch(.rlang_vctrs_typeof(to),
    logical = .vctrs__lgl_cast(x, to),
    integer = .vctrs__int_cast(x, to),
    double = .vctrs__dbl_cast(x, to),
    character = .vctrs__chr_cast(x, to),
    list = .vctrs__list_cast(x, to),
    base_data_frame = .vctrs__df_cast(x, to),
    rlib_data_frame = .vctrs__rlib_df_cast(x,to),
    tibble = .vctrs__tib_cast(x, to),
    .vctrs__stop_incompatible_cast(x, to))
}

.vctrs__stop_incompatible_cast <- function(x, to) {
  stop(sprintf(
    "Can't convert <%s> to <%s>.", .rlang_vctrs_typeof(x),
    .rlang_vctrs_typeof(to)
  ), call. = FALSE)
}
.vctrs__lgl_cast <- function(x, to) {
  switch(.rlang_vctrs_typeof(x),
    logical = x,
    integer = ,
    double = .vctrs__lgl_cast_from_num(x),
    .vctrs__stop_incompatible_cast(x, to))
}
.vctrs__lgl_cast_from_num <- function(x) {
    if (any(!x %in% c(0L, 1L))) {
        .vctrs__stop_incompatible_cast(x, to)
    }
    as.logical(x)
}

.vctrs__int_cast <- function(x, to) {
  switch(.rlang_vctrs_typeof(x),
    logical = as.integer(x),
    integer = x,
    double = .vctrs__int_cast_from_dbl(x),
    .vctrs__stop_incompatible_cast(x, to))
}
.vctrs__int_cast_from_dbl <- function(x) {
    out <- suppressWarnings(as.integer(x))
    if (any((out != x) | xor(is.na(x), is.na(out)))) {
        .vctrs__stop_incompatible_cast(x, to)
    } else {
        out
    }
}
.vctrs__dbl_cast <- function(x, to) {
  switch(.rlang_vctrs_typeof(x),
    logical = ,
    integer = as.double(x),
    double = x,
    .vctrs__stop_incompatible_cast(x, to)
  )
}
.vctrs__chr_cast <- function(x, to) {
  switch(.rlang_vctrs_typeof(x),
    character = x,
    .vctrs__stop_incompatible_cast(x, to))
}
.vctrs__list_cast <- function(x, to) {
  switch(.rlang_vctrs_typeof(x),
    list = x,
    .vctrs__stop_incompatible_cast(x, to))
}
.vctrs__df_cast <- function(x, to) {
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
.vctrs__rlib_df_cast <- function(x, to) {
  new_data_frame(df_cast(x, to), .class = "tbl")
}
.vctrs__tib_cast <- function(x, to) {
  new_data_frame(df_cast(x, to), .class = c("tbl_df", "tbl"))
}

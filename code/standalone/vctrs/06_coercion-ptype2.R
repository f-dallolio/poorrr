vec_ptype2 <- function(x, y) {
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
  ptype <- switch(x_type,
    logical = switch(y_type,
      logical = x,
      integer = y,
      double = y,
      .vctrs__stop_incompatible_type(x, y)
    ),
    integer = switch(.rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = y,
      .vctrs__stop_incompatible_type(x, y)
    ),
    double = switch(.rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = x,
      .vctrs__stop_incompatible_type(x, y)
    ),
    character = switch(.rlang_vctrs_typeof(y),
      character = x,
      .vctrs__stop_incompatible_type(x, y)
    ),
    list = switch(.rlang_vctrs_typeof(y),
      list = x,
      .vctrs__stop_incompatible_type(x, y)
    ),
    base_data_frame = switch(.rlang_vctrs_typeof(y),
      base_data_frame = ,
      s3_data_frame = .vctrs__df_ptype2(x, y),
      rlib_data_frame = .vctrs__rlib_df_ptype2(x, y),
      tibble = .vctrs__tib_ptype2(x, y),
      .vctrs__stop_incompatible_type(x, y)
    ),
    rlib_data_frame = switch(.rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      s3_data_frame = .vctrs__rlib_df_ptype2(x, y),
      tibble = .vctrs__tib_ptype2(x, y),
      .vctrs__stop_incompatible_type(x, y)
    ),
    tibble = switch(.rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      tibble = ,
      s3_data_frame = .vctrs__tib_ptype2(x, y),
      .vctrs__stop_incompatible_type(x, y)
    ),
    .vctrs__stop_incompatible_type(x, y)
  )
  vec_slice(ptype, 0)
}

.vctrs__stop_incompatible_type <- function(x, y) {
    stop(sprintf(
        "Can't combine types <%s> and <%s>.", .rlang_vctrs_typeof(x),
        .rlang_vctrs_typeof(y)
    ), call. = FALSE)
}
.vctrs__df_ptype2 <- function(x, y) {
    x <- as.list(vec_slice(x, 0))
    y <- as.list(vec_slice(y, 0))
    names <- .vctrs__set_partition(names(x), names(y))
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
.vctrs__set_partition <- function(x, y) {
    list(both = intersect(x, y), only_x = setdiff(
        x,
        y
    ), only_y = setdiff(y, x))
}
.vctrs__rlib_df_ptype2 <- function(x, y) {
    new_data_frame(.vctrs__df_ptype2(x, y), .class = "tbl")
}
.vctrs__tib_ptype2 <- function(x, y) {
    new_data_frame(.vctrs__df_ptype2(x, y), .class = c(
        "tbl_df",
        "tbl"
    ))
}

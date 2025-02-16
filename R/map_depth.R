#' Map/modify elements at given depth
#' @name map_depth
NULL

#' @rdname map_depth
#' @export
map_depth <- function (.x, .depth, .f, ..., .ragged = .depth < 0, .is_node = NULL) {
  force(.ragged)
  .depth <- check_depth(.depth, pluck_depth(.x, .is_node))
  .f <- as_mapper(.f, ...)
  .is_node <- as_is_node(.is_node)
  map_depth_rec(map, .x, .depth, .f, ..., .ragged = .ragged, .is_node = .is_node)
}
#' @rdname map_depth
#' @export
modify_depth <- function (.x, .depth, .f, ..., .ragged = .depth < 0, .is_node = NULL) {
  force(.ragged)
  .depth <- check_depth(.depth, pluck_depth(.x, .is_node))
  .f <- as_mapper(.f, ...)
  .is_node <- as_is_node(.is_node)
  map_depth_rec(modify, .x, .depth, .f, ..., .ragged = .ragged,
                .is_node = .is_node)
}


check_depth <- function (depth, max_depth, error_call = caller_env()) {
  if (depth %% 1 != 0) {
    msg <- sprintf("`depth` must be a whole number, not %s", as.character(depth))
    abort(msg, call = error_call)
  }

  if (depth < 0) {
    if (-depth > max_depth) {
      msg <- sprintf("If negative, `depth` must be greater than %s.", as.character(-max_depth))
      abort(msg, call = error_call)
    }
    depth <- max_depth + depth
  }
  depth
}


map_depth_rec <- function (.fmap, .x, .depth, .f, ..., .ragged, .is_node, .purrr_error_call = caller_env()) {
  if (.depth == 0) {
    if (identical(.fmap, map)) {
      return(.f(.x, ...))
    } else {
      .x[] <- .f(.x, ...)
      return(.x)
    }
  }
  if (!.is_node(.x)) {
    if (.ragged) {
      return(.fmap(.x, .f, ...))
    } else {
      abort("List not deep enough", call = .purrr_error_call)
    }
  }
  if (.depth == 1) {
    .fmap(.x, .f, ...)
  }
  else {
    .fmap(.x, function(x) {
      map_depth_rec(.fmap = .fmap, .x = x, .depth = .depth -
                      1, .f = .f, ..., .ragged = .ragged, .is_node = .is_node,
                    .purrr_error_call = .purrr_error_call)
    })
  }
}

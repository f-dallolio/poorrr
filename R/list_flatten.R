#' Flatten a list
#'
#' Flattening a list removes a single layer of internal hierarchy,
#' i.e. it inlines elements that are lists leaving non-lists alone.
#'
#' @param x A list.
#' @param name_spec If both inner and outer names are present, control
#'   how they are combined. Should be a glue specification that uses
#'   variables `inner` and `outer`.
#' @param name_repair One of `"minimal"`, `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vctrs::vec_as_names()] for the meaning of these
#'   options.
#' @param \dots	These dots are for future extensions and must be empty.
#' @return A list of the same type as `x`. The list might be shorter
#'   if `x` contains empty lists, the same length if it contains lists
#'   of length 1 or no sub-lists, or longer if it contains lists of
#'   length > 1.
#' @examples
#' x <- list(1, list(2, 3), list(4, list(5)))
#' x |> list_flatten() |> str()
#' x |> list_flatten() |> list_flatten() |> str()
#'
#' # Flat lists are left as is
#' list(1, 2, 3, 4, 5) |> list_flatten() |> str()
#'
#' # Empty lists will disappear
#' list(1, list(), 2, list(3)) |> list_flatten() |> str()
#'
#' # Another way to see this is that it reduces the depth of the list
#' x <- list(
#'   list(),
#'   list(list())
#' )
#' x |> pluck_depth()
#' x |> list_flatten() |> pluck_depth()
#'
#' # Use name_spec to control how inner and outer names are combined
#' x <- list(x = list(a = 1, b = 2), y = list(c = 1, d = 2))
#' x |> list_flatten() |> names()
#' x |> list_flatten(name_spec = "{outer}") |> names()
#' x |> list_flatten(name_spec = "{inner}") |> names()
#' @export
list_flatten <- function (x, ..., name_spec = "{outer}_{inner}", name_repair = c("minimal", "unique", "check_unique", "universal")) {
  i <- vapply(x, vec_is_list, logical(1))
  x[i] <- lapply(x[i], unclass)
  x[!i] <- lapply(x[!i], list)
  out <- unname(do.call(c, x))

  nms <- .list_flatten_names(name_spec = name_spec,
                             outer = names(x),
                             inner = lapply(x, names),
                             n = lengths(x))

  names(out) <- vec_as_names(nms, repair = match.arg(name_repair))
  out
}

.list_flatten_names <- function(name_spec, outer, inner, n) {
  if (length(outer) > 1 || is.list(inner) || length(n) > 1) {
    l <- .args_recycle(list(outer = outer, inner = inner, n = n))
    out <- mapply(.list_flatten_names, l$outer, l$inner, l$n, MoreArgs = list(name_spec = name_spec))
    return(unlist(out, use.names = FALSE))
  }
  if (is.null(outer) || identical(outer, "")) return(inner)
  if (is.null(inner) || all(inner == "")) {
    if (n > 1) {
      inner <- seq_len(n)
    } else {
      return(outer)
    }
  }
  as_name_spec(name_spec)(outer, inner)
}

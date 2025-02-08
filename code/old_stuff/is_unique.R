#' Does an object contains a unique value?
#' @export
is_unique <- function(x) {
  length(unique(x)) == 1
}

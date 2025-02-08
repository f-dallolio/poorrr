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

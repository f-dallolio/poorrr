list_rbind <- function(x) {
  obj_check_list(x)
  do.call("rbind", x)
}

list_cbind <- function(x) {
  obj_check_list(x)
  do.call("cbind", x)
}

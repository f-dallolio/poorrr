#
# ns_nms <- function (pkg, export = NULL, sort = TRUE) {
#   nms <- ns_names(pkg, sort)
#   id <- nms %in% ns_exports(pkg)
#   names(nms)[id] <- rep("<e>", sum(id))
#   names(nms)[!id] <- rep("<p>", sum(!id))
#   if (is.null(export)) {
#     out <- structure(nms, name = pkg, class = c("ns_nms", class(nms)))
#     return(out)
#   }
#   if (export) {
#     nms <- nms[id]
#   } else if (!export) {
#     nms <- nms[!id]
#   }
#   structure(nms, name = pkg, export = export, class = c("ns_nms", class(nms)))
# }
#
# print.ns_nms <- function(x) {
#   name <- attr(x, "name")
#   export <- attr(x, "export")
#   nms <- names(x)
#   out <- unname(x)
#   attributes(out) <- NULL
#
#   if(is.null(export)) {
#     title <- sprintf("<Namespace: \"%s\">", name)
#     nms[nms == "<p>"] <- rep("   ", sum(nms == "<p>"))
#     out <- paste(nms, out)
#   } else {
#     if (export) {
#       title <- sprintf("<Namespace: \"%s\" -- export --", name)
#     } else {
#       title <- sprintf("<Namespace: \"%s\" -- private --", name)
#     }
#   }
#
#   cat(title, "\n")
#   print(noquote(out))
#   cat(title, "\n")
#   invisible(x)
# }
#
# `[.ns_nms` <- function(x, i) {
#   attrs <- attributes(x)
#   attrs$names <- attrs$names[i]
#
#   out <- unclass(x)[i]
#   attributes(out) <- attrs
#   out
# }






ns_info <- function (pkg, ..., sort = TRUE) {
  objs <- mget(ns_names(pkg), ns_env(pkg))
  if (sort) {
    objs <- objs[sort(names(objs))]
  }
  export = names(objs) %in% getNamespaceExports(pkg)
  df <- data.frame(name = names(objs),
                   # export = ifelse(export, "export", "private"),
                   export = export,
                   mode = vapply(objs, mode, ""),
                   type = vapply(objs, typeof, ""),
                   class = vapply(objs, function(x) class(x)[[1]], ""))
  row.names(df) <- NULL

  lifecycle <- find_fn_lifecycle(pkg)
  names(lifecycle)[names(lifecycle) == "value"] <- "lifecycle"
  out <- merge(df, lifecycle, all.x = TRUE)
  out$lifecycle[is.na(out$lifecycle)] <- ""
  out
}



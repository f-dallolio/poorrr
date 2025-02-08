vec_ptype_common <- function (xs, ptype = NULL) {
    if (!is.null(ptype)) {
        return(vec_ptype(ptype))
    }
    xs <- Filter(function(x) !is.null(x), xs)
    if (length(xs) == 0) {
        return(NULL)
    }
    if (length(xs) == 1) {
        out <- vec_ptype(xs[[1]])
    }
    else {
        xs <- map(xs, vec_ptype)
        out <- Reduce(vec_ptype2, xs)
    }
    vec_ptype_finalise(out)
}

vec_ptype_finalise <- function (x) {
    if (is.data.frame(x)) {
        x[] <- lapply(x, vec_ptype_finalise)
        return(x)
    }
    if (inherits(x, "rlang_unspecified")) {
        logical()
    }
    else {
        x
    }
}

vec_ptype <- function (x) {
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

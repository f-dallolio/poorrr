.rlang_vctrs_typeof <- function (x) 
{
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
    switch(type, `NULL` = return("null"), logical = if (vec_is_unspecified(x)) {
        return("unspecified")
    } else {
        return(type)
    }, integer = , double = , character = , raw = , list = return(type))
    stop(sprintf("Unimplemented type <%s>.", type), call. = FALSE)
}

vec_is_unspecified <- function (x) 
{
    !is.object(x) && typeof(x) == "logical" && length(x) && all(vapply(x, 
        identical, logical(1), NA))
}

.rlang_vctrs_unspecified <- function (x = NULL) 
{
    structure(rep(NA, length(x)), class = "rlang_unspecified")
}

.rlang_vctrs_s3_method <- function (generic, class, env = parent.frame()) 
{
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
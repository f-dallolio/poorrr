is_raw  <-  function(x, n = NULL) {
    .Call(ffi_is_raw, x, n)
}

is_logical  <-  function(x, n = NULL) {
    .Call(ffi_is_logical, x, n)
}

is_call_simple  <-  function(x, ns = NULL) {
    check_required(x)
    if (is_quosure(x) || is_formula(x)) {
        x <- get_expr(x)
    }
    if (!is_call(maybe_missing(x))) {
        return(FALSE)
    }
    if (is_call(x, c("::", ":::"))) {
        return(FALSE)
    }
    head <- x[[1]]
    namespaced <- is_call(head, c("::", ":::"))
    if (!is_null(ns) && !identical(namespaced, ns)) {
        return(FALSE)
    }
    namespaced || is_symbol(head)
}

is_scalar_integer  <-  function(x) {
    .Call(ffi_is_integer, x, 1L)
}

is_lgl_na  <-  function(x) {
    identical(x, na_lgl)
}

is_bare_vector  <-  function(x, n = NULL) {
    is_bare_atomic(x) || is_bare_list(x, n)
}

is_scalar_double  <-  function(x) {
    .Call(ffi_is_double, x, 1L, NULL)
}

is_call  <-  function(x, name = NULL, n = NULL, ns = NULL) {
    .Call(ffi_is_call, x, name, n, ns)
}

is_character  <-  function(x, n = NULL) {
    .Call(ffi_is_character, x, n, NULL, NULL)
}
is_string <- function(x) {
    is.character(x) && length(x) == 1
}

is_bare_double  <-  function(x, n = NULL) {
    !is.object(x) && is_double(x, n)
}

is_primitive_lazy  <-  function(x) {
    .Call(ffi_is_primitive_lazy, x)
}

is_scalar_character  <-  function(x) {
    is_character(x, n = 1L)
}

is_named2  <-  function(x) {
    nms <- names(x)
    if (is_null(nms)) {
        return(!length(x))
    }
    if (any(detect_void_name(nms))) {
        return(FALSE)
    }
    TRUE
}

is_integer  <-  function(x, n = NULL) {
    .Call(ffi_is_integer, x, n)
}

is_atomic  <-  function(x, n = NULL) {
    .Call(ffi_is_atomic, x, n)
}

is_namespace  <-  function(x) {
    isNamespace(x)
}

is_bare_character  <-  function(x, n = NULL) {
    !is.object(x) && is_character(x, n)
}

is_scalar_logical  <-  function(x) {
    .Call(ffi_is_logical, x, 1L)
}

is_scalar_atomic  <-  function(x) {
    .Call(ffi_is_atomic, x, 1L)
}

is_complex  <-  function(x, n = NULL, finite = NULL) {
    .Call(ffi_is_complex, x, n, finite)
}

is_vector  <-  function(x, n = NULL) {
    .Call(ffi_is_vector, x, n)
}

is_integerish  <-  function(x, n = NULL, finite = NULL) {
    .Call(ffi_is_integerish, x, n, finite)
}

is_scalar_list  <-  function(x) {
    .Call(ffi_is_list, x, 1L)
}

is_pairlist  <-  function(x) {
    typeof(x) == "pairlist"
}

is_dbl_na  <-  function(x) {
    identical(x, na_dbl)
}

is_bare_integer  <-  function(x, n = NULL) {
    !is.object(x) && is_integer(x, n)
}

is_double  <-  function(x, n = NULL, finite = NULL) {
    .Call(ffi_is_double, x, n, finite)
}

is_primitive_eager  <-  function(x) {
    .Call(ffi_is_primitive_eager, x)
}

is_scalar_complex  <-  function(x) {
    .Call(ffi_is_complex, x, 1L, NULL)
}

is_int_na  <-  function(x) {
    identical(x, na_int)
}

is_list  <-  function(x, n = NULL) {
    .Call(ffi_is_list, x, n)
}

is_bare_atomic  <-  function(x, n = NULL) {
    !is.object(x) && is_atomic(x, n)
}

is_bare_integerish  <-  function(x, n = NULL, finite = NULL) {
    !is.object(x) && is_integerish(x, n, finite)
}

is_chr_na  <-  function(x) {
    identical(x, na_chr)
}

is_na  <-  function(x) {
    is_scalar_vector(x) && is.na(x)
}

is_bare_logical  <-  function(x, n = NULL) {
    !is.object(x) && is_logical(x, n)
}

is_scalar_vector  <-  function(x) {
    .Call(ffi_is_vector, x, 1L)
}

is_named  <-  function(x) {
    nms <- names(x)
    if (is_null(nms)) {
        return(FALSE)
    }
    if (any(detect_void_name(nms))) {
        return(FALSE)
    }
    TRUE
}

is_bare_complex  <-  function(x, n = NULL) {
    !is.object(x) && is_complex(x, n)
}

is_cpl_na  <-  function(x) {
    identical(x, na_cpl)
}

is_node_list  <-  function(x) {
    typeof(x) %in% c("pairlist", "NULL")
}

is_function  <-  function(x) {
    .Call(ffi_is_function, x)
}

is_primitive  <-  function(x) {
    .Call(ffi_is_primitive, x)
}

is_callable  <-  function(x) {
    is_symbolic(x) || is_function(x)
}

is_scalar_integerish  <-  function(x, finite = NULL) {
    .Call(ffi_is_integerish, x, 1L, finite)
}

is_environment  <-  function(x) {
    typeof(x) == "environment"
}

is_symbolic  <-  function(x) {
    typeof(x) %in% c("language", "symbol")
}

is_symbol  <-  function(x, name = NULL) {
    if (typeof(x) != "symbol") {
        return(FALSE)
    }
    if (is_null(name)) {
        return(TRUE)
    }
    as_string(x) %in% name
}

is_scalar_raw  <-  function(x) {
    .Call(ffi_is_raw, x, 1L)
}

is_expression  <-  function(x) {
    stack <- new_stack()
    stack$push(zap_srcref(x))
    while (!is_exhausted(elt <- stack$pop())) {
        if (is_missing(elt)) {
            return(FALSE)
        }
        if (!is_null(attributes(elt))) {
            return(FALSE)
        }
        switch(typeof(elt), language = stack$push(!!!as.list(elt)),
            if (!is_symbol(elt) && !is_syntactic_literal(elt)) {
                return(FALSE)
            })
    }
    TRUE
}

is_bare_raw  <-  function(x, n = NULL) {
    !is.object(x) && is_raw(x, n)
}

is_bare_list  <-  function(x, n = NULL) {
    !is.object(x) && is_list(x, n)
}

is_bare_environment  <-  function(x) {
    !is.object(x) && typeof(x) == "environment"
}

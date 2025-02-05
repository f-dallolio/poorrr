as_sym <- function(..., car = NULL, cdr = NULL) {
  stopifnot(...length() == 0)
  c(car, cdr)
}
as_sym(cdr = 1)

f <- function (...) {
  mget("...")
}
names(f(letters))
eval(str2lang("...length()"), (f(letters, mean)))

is_spliced_call <- function(x) {
  all(all.names(x)[1:3] == "!")
}

subs <- pryr::subs

as.expression(list(!!!mtcars, quote(letters)))

subs(c(x, ), list(x = mtcars))
as.expression(as.list((letters)))

xx <- alist(!!!mtcars, letters)
x <- quote(!!!xx)
switch(typeof(x),
       symbol = eval(call("list", x)),
       langage = if (is_spliced_call(x)) eval(x[[c(2,2,2)]]) else eval(call("list", x)),
       list(x))



x <- quolettersx <- quote(mtcars)
obj <- eval(x)
if (is_list(obj)) {
  exprs <- lapply(seq_along(obj), \(i) call("[[", quote(x), i))
  names(exprs) <- names(obj)
  (substitute(eval(substitute(exprs)), list(x = x)))
}
rlang::new_call(as.symbol("list"), as.pairlist(exprs))


(function(...) {
  mget("...", environment(), inherits = FALSE)
  # dots <- eval(substitute(alist(...)))
  # e <- new.env()
  # delayedAssign("...", as.pairlist(dots), assign.env = e)
  # e
})(1,2,3)$... |> list() |> lapply(\(x) eval(substitute(alist(...))))

substitute(~ x, list2env(list(x = c(mtcars))))
x <- alist(!!!mtcars, letters)
is_spliced_dots <- function(...) {
  vapply(eval(substitute(alist(...))), )
}

f <- \(x) {
  lazy_eval()
  if (is_spliced_call(x)) {
    x <- x[[c(2,2,2)]]
  } else {
    x <- (x)
  }
  return(x)
  if (all(!vapply(x, is_spliced_call, logical(1)))) {
    do.call(c, lapply(x, sys.function()))
  } else {
    do.call(c, x)
  }
}
f(!!!x)
is_spliced_call(x)



dots_is_spliced <- function(dots) {
  any(vapply(dots, is_spliced_call, logical(1)))
}
has_spliced_call <- function(dots, negate = FALSE) {
  spliced <- dots_is_spliced(dots)
  if (negate) {
    all(!spliced)
  } else {
    any(spliced)
  }
}
splice_dot <- function(x) {
  if (is_spliced_call(x)) {
    eval(x[[c(2,2,2)]])
  } else {
    list(eval(x))
  }
}

substitute2 <- function (x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}
splice_endots <- function(dots) {
  if (dots_is_spliced(dots)) {
    do.call(c, lapply(dots, splice_dot))
  } else {
    lapply(dots, eval)
  }
}

  splice_endots(alist(!!!mtcars, letters))

  i <- vapply(dots, is_spliced_call, logical(1))
  if(all(i)) {
    lapply(dots, eval)
  } else {

  }
  # x <- list(...)
  # i <- dots_is_spliced(dots)
  # x[i] <- lapply(x[i], splice)
  # do.call(c, x)
}

x <- alist(!!!mtcars, letters)

fn <- function(...) {
  dots <- match.call(expand.dots = FALSE)[["..."]]
  if (!dots_is_spliced(dots)) {
    return(list(...))
  }
  i <- 1
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    if (is_spliced_call(dots[[i]])) {
      dots[[i]] <- eval.parent(dot[[c(2,2,2)]])
    } else {
      dots[[i]] <- list(eval.parent(dot))
    }
  }
  do.call("c", lapply(dots, eval))
}
 fn(!!!x)

eval_spliced <- function(x) {
  if (is_spliced_call(x)) {
      out <- x[[c(2,2,2)]]
  } else if (is.call(x) || is.symbol(x)){
      out <- list(eval(x))
    }
  } else {
    if ()
  }


}

function(x) {
  if (is_spliced_call(x)) {
    eval(x)
  }
}


dots <- alist(!!!mtcars, letters)

fn(mtcars, letters)

flatten_dots <- function(.dots) {
  vapply(.dots, is_spliced_call)
}
flatten_spliced <- function(...) {

}

list(..1 = 1)

f <- function(...) {
  dots <- eval(substitute(alist(...)))
  i <- vapply(dots, is_spliced_call, logical(1))
  if (any(i)) {
    dots[i] <- lapply(dots[i], `[[`, c(2,2,2))
    dots[!i] <- lapply(dots[!i], \(x) call("list", x))
    # names(dots) <- paste0("..", seq_along(dots))
  }
  eval(as.call(append(quote(c), dots)))
  # match.call(call = call("c"), envir = list2env(list("..." = dots))) |>
  #   eval()
  # dots
  # e <- new.env(hash = FALSE)
  # assign("...", , envir = e)
  # match.call(envir = e)
}
f(mtcars, letters)
dots <- alist(!!!mtcars, letters)

evalq(call("c", quote(`...`)), list2env(list(... = alist(mean, var))))


??obj_is_vector(mtcars)


is.recursive(setNames(letters, LETTERS))

as.call(mtcars)


substitute(rlang:::node_cdr(quote(mean)))
quote(char) |> pryr::sexp_type()


rlang:::expr_type_of
expr_type2 <- function (x) {
  if (missing(x) || identical(x, quote(expr = ))) {
    return("missing")
  }
  if (is.call(x)) {
    if (deparse(x[[1]]) %in% c("::", ":::")) {
      return("symbol")
    }
    if deparse(x[[1]] == "function") {
      fn <- eval(x)
    }
  }
  type <- typeof(x)
  if (type %in% c("symbol", "language", "pairlist", "NULL")) {
    type
  } else {
    "literal"
  }
}

switch_expr2 <- function(.x, ...) {
  switch(expr_type2(.x,...))
}

x <- quote(function(x, env = rlang::caller_env()){rlang::eval_tidy(x, env)})

swe <- function(x) {

  if (is.call(x) && deparse(x[[1]]) =="function") {
    x <- eval(x)
    out <- c(formals(x), body(x))
  } else {
    out <- switch(expr_type2(x),
                  "literal" = ,
                  "symbol" = list(x),
                  "pairlist" = ,
                  "language" = as.list(x),
                  NULL)
  }
  unlist(lapply(out, swe))
}
swe(x)

is_node2 <- function(x) {
  if (is.call(x)) {
    if (deparse(x[[1]]) %in% c("::", ":::")) {
      return(FALSE)
    }
    return(TRUE)
  }
  is.pairlist(x)
}

swe <- function(x) {
  xx <- unique(do.call(c, map_if(as.list(x), is_node2, as.list, .else = list)))
  xx <- discard(xx, identical, quote(expr = ))
  xx <- discard(xx, identical, x)
  if (none(xx, is_node2)) {
    return(xx)
  }
  swe(xx)
}
swe(x)

x <- quote(
  a <- function(x, env = rlang::caller_env()){rlang::eval_tidy(x, env)}
)

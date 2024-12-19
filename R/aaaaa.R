library(purrr)

x <- alist(
  x,
  y,
  z
)
bquote(
  {..(x)},
  splice = TRUE
)

list_sizes(as.list(mtcars))

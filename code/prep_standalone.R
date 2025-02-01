library("tidyverse")

h <- as_tibble(help.search(".", fields = "alias", package = "purrr")[["matches"]])
h <- transmute(h, name = Entry, topic = Topic, title = Title)

x <- as.list(parse("standalone-purrr.R"))
x <- names(unlist(lapply(x, \(x) within(list(), eval(x)))))
x <- tibble(name = x)

tbl <- left_join(x, select(h, -title)) |>
  mutate(topic = ifelse(name %in% c(".as_function", ".set_names"), "utils", topic)) |>
  print(n = Inf)

topic <- tbl$topic

name <- ""
for (i in seq_along(topic)) {
  nm <- topic[[i]]
  if (!is.na(nm)) {
    name <- nm
  }
  topic[i] <- name
}
tbl$topic <- topic

tbl2 <- summarise(tbl, name = list(name), .by = topic) |>
  left_join(select(h, topic, title) |> distinct()) |>
  unnest(cols = everything())

tbl_new <- filter(tbl2, is.na(title))
tbl2 <-  filter(tbl2, !is.na(title))

if (!dir.exists("poorrr")) {
  dir.create("poorrr/R", recursive = TRUE)
}

tbl_tot <- tbl2 |>
  mutate(path = file.path("poorrr/R", paste0(topic, ".r")))


e <- new.env()
eval(parse("standalone-purrr.R"), e)
f <- function(fn, nm) {
  stopifnot(typeof(fn) == "closure")
  x <- call("function", formals(fn), body(fn))
  x <- call("<-" , as.symbol(nm), x)
  paste0(deparse(x), collapse = "\n")
}


tbl_tot <- tbl_tot |>
  mutate(export = !grepl("^[.]", name)) |>
  mutate(obj = map(name, get, envir = e),
         txt = map2_chr(obj, name, f),
         txt = if_else(export, paste0("#' @rdname ", topic, "\n#' @export\n", txt), txt))


summarise(tbl_tot,
          txt = paste0(txt, collapse = "\n"),
          .by = c(topic, title, path)) |>
  mutate(title2 = paste0(paste("#'", title), "\n", paste0("#' @name ", topic, "\nNULL\n")),
         txt2 = paste(title2, txt, sep = "\n"))|>
  select(path, txt2) |>
  mutate(path = file.path("r_packages", path)) |>
  deframe() |>
  iwalk(~ cat(.x, file = .y))


tbl_new |>
  mutate(title = "Utility functions",
         path = file.path("r_packages/poorrr/R", paste0(topic, ".r")),
         obj = map(name, get, e),
         txt = map2_chr(obj, name, f)) |>
  summarise(txt = paste0(txt, collapse = "\n\n"),
         .by = c(title, path)) |>
  mutate(txt = paste0("#' ", title, "\n\n", txt)) |>
  select(-title) |>
  deframe() |>
  iwalk(~ cat(.x, file = .y))

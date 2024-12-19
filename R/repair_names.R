vec_as_names <- function(names, ..., repair = c("minimal", "unique", "universal", "check_unique")) {
  stopifnot(...length() == 0)
  switch(match.arg(repair),
         "universal" = repair_names_universal(names),
         "minimal" = repair_names_minimal(names),
         "unique" = repair_names_unique(names),
         "check_unique" = repair_names_check_unique(names))
}


repair_names_minimal <- function(x) {
  x[is.na(x)] <- ""
  x
}

repair_names_unique <- function (x) {
  if (length(x) == 0) return(character())
  x[is.na(x)] <- ""
  x <- gsub("^[.]+[0-9]*$", "", x)
  names(x) <- x
  unique_names <- names(which(table(x) == 1))
  if(length(x) == length(unique_names)) return(x)
  out <- paste0("...", seq_along(x))
  ifelse(x %in% unique_names, x, paste0(x, out))
}

repair_names_check_unique <- function (x) {
  if (all_names_unique(x)) return(x)
  nms <- names(which(table(x) > 1))
  nms <- vapply(setNames(nms, nms),
                function(names) oxford_comma_and(which(names == x)),
                character(1))
  print_bullets <- sprintf("- %s at position %s.\n", dQuote(names(nms)), nms)
  stop("Names must be unique.\n", print_bullets)
}

repair_names_universal <- function (x) {
  x <- repair_names_unique(x)
  x <- gsub("[^._[:alnum:]]", ".", )
  universal <- detect_names_universal(x)
  while(!all(universal)) {
    x[!universal] <- paste0(".", x[!universal])
    universal <- detect_names_universal(x)
  }
  x
}





detect_names_unique <- function(x, negate = FALSE) {
  names(x) <- x
  x[is.na(x)] <- ""
  x <- gsub("^[.]+[0-9]*$", "", x)
  nms <- names(which(table(x) == 1))
  out <- x %in% nms
  names(out) <- names(x)
  if(negate) !out else out
}

all_names_unique <- function(x, negate = FALSE) {
  out <- all(detect_names_unique(x))
  if(negate) !out else out
}



detect_names_universal <- function(x, negate = FALSE) {
  id_unique <- detect_names_unique(x)
  id_not_reserved <- !.str_detect_reserved(x)
  id_good_char <- !.str_detect_bad_char(x)
  id_no_backtick <- !.str_detect_backtick(x)
  id_no_elt <- !.str_detect_elt(x)
  out <- id_unique & id_not_reserved & id_good_char & id_no_backtick & id_no_elt
  if(negate) return(!out)
  out
}
all_names_universal <- function(x, negate = FALSE) {
  out <- all(detect_names_universal(x))
  if(negate) !out else out
}

.str_detect_reserved <- function(x) {
  .reserved_names <- c("if", "else", "repeat", "while",
                       "function", "for", "in", "next", "break",
                       "TRUE", "FALSE", "NULL", "Inf",
                       "NaN",  "NA",
                       "NA_integer_", "NA_real_",
                       "NA_complex_", "NA_character_")
  x %in% .reserved_names
}
.str_detect_backtick <- function (x) {
  grepl("`", deparse(as.symbol(x), backtick = TRUE))
}
.str_detect_elt <- function (x) {
  grepl("^[.]{3}$|^[.]{2}[0-9]+$", x)
}
.str_detect_bad_char <- function(x) {
  grepl(paste0("[^._[:alnum:]]", collapse = "|"), x)
}

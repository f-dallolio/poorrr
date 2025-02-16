str_flatten_comma <- function(string, last = "and", na.rm = FALSE, oxford = TRUE) {
  collapse <- ", "
  if (oxford) {
    last <- paste0(", ", gsub("^\\s*[,]", "", last))
    if (!grepl("\\s$", last)) {
      last <- paste0(last, " ")
    }
  }
  str_flatten(string, collapse, last, na.rm)
}

str_flatten <- function(string, collapse = "", na.rm = FALSE) {
  if (na.rm) {
    string <- string[!is.na(string)]
  }
  n <- length(string)
  if (n == 1) {
    return(string)
  }
  if (is.null(last)) {
    last <- collapse
  }
  if (n == 2) {
    paste0(string, collapse = last)
  } else {
    string_1 <- paste0(string[-n], collapse = collapse)
    paste(string_1, string[n], sep = last)
  }
}

str_detect <- function(
    string, pattern, negate = FALSE, ..., ignore.case = FALSE,
    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  out <- grepl(pattern, string, ignore.case, perl, fixed, useBytes)
  if (negate) {
    return(!out)
  }
  out
}

str_which <- function(
    string, pattern, negate = FALSE, ..., ignore.case = FALSE,
    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  grep(pattern, string,
    invert = negate, ignore.case, perl,
    value = FALSE, fixed, useBytes
  )
}

str_subset <- function(
    string, pattern, negate = FALSE, ..., ignore.case = FALSE,
    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  grep(pattern, string,
    invert = negate, ignore.case, perl,
    value = TRUE, fixed, useBytes
  )
}

str_replace <- function(
    string, pattern, replacement, ..., ignore.case = FALSE,
    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  sub(pattern, replacement,
    x = string, ignore.case, perl,
    fixed, useBytes
  )
}

str_replace_all <- function(
    string, pattern, replacement, ..., ignore.case = FALSE,
    perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  gsub(pattern, replacement,
    x = string, ignore.case, perl,
    fixed, useBytes
  )
}

str_remove <- function(
    string, pattern, ..., ignore.case = FALSE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE) {
  sub(pattern, "", x = string, ignore.case, perl, fixed, useBytes)
}

str_remove_all <- function(
    string, pattern, ..., ignore.case = FALSE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE) {
  gsub(pattern, "", x = string, ignore.case, perl, fixed, useBytes)
}

str_pad <- function(
    string, side = c("left", "right", "both"), pad = " ",
    use_width = TRUE) {
  if (is.null(width)) {
    if (use_width) {
      type <- "width"
    } else {
      type <- "char"
    }
    width <- max(nchar(string, type = type))
  }
  justify <- c(left = "right", right = "left", centre = "both")
  out <- format(string, width = width, justify = justify[match.arg(side)])
  if (pad == " ") {
    return(out)
  }
  gsub(" ", pad, out)
}

str_numpad <- function(x) {
  gsub(" ", "0", format(as.integer(x)))
}

str_enum <- function(x, side = c("right", "left"), sep = "") {
  enum <- str_numpad(x = seq_along(x))
  side <- match.arg(side)
  if (side == "left") {
    paste(enum, x, sep = sep)
  } else {
    paste(x, enum, sep = sep)
  }
}

str_enum_left <- function(x, sep = "") {
  str_enum(x, side = "left", sep = sep)
}

str_enum_right <- function(x, sep = "") {
  str_enum(x, side = "right", sep = sep)
}
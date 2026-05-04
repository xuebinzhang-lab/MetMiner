# Shared utility operators and predicates — loaded first so all downstream
# fct_ / mod_ files can use them without repeating definitions.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

has_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(rep(FALSE, length(x)))
  x <- as.character(x)
  !is.na(x) & nzchar(x)
}

coerce_text <- function(x, fallback = "") {
  if (is.null(x) || length(x) == 0) return(fallback)
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- fallback
  x
}

# Just add this to base R already
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

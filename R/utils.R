# Just add this to base R already
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Taken from ggplot2
message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

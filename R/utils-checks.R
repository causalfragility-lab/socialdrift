#' Check that an object is a social_graph_series
#' @param x Object to check.
#' @keywords internal
check_graph_series <- function(x) {
  if (!inherits(x, "social_graph_series")) {
    stop("Input must be a `social_graph_series` produced by `build_graph_series()`.",
         call. = FALSE)
  }
  if (length(x) == 0) {
    stop("Graph series is empty.", call. = FALSE)
  }
  invisible(x)
}

#' Check that an object is an igraph
#' @param x Object to check.
#' @keywords internal
check_igraph <- function(x) {
  if (!igraph::is_igraph(x)) {
    stop("Input must be an igraph object.", call. = FALSE)
  }
  invisible(x)
}

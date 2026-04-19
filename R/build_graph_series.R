#' Build a time series of graph snapshots
#'
#' Splits event data into non-overlapping time windows and builds one graph
#' per window. Returns a named list of igraph objects.
#'
#' @param data A standardized social event tibble (output of [as_social_events()]).
#' @param window Aggregation window: one of `"day"`, `"week"`, `"month"`,
#'   `"quarter"`, or `"year"`. Default `"month"`.
#' @param directed Logical; if `TRUE` (default) graphs are directed.
#' @param weighted Logical; if `TRUE` (default) edges carry aggregated weights.
#' @param remove_self_loops Logical; if `TRUE` (default) self-loops are removed.
#'
#' @return A named list of igraph objects of class `social_graph_series`.
#'   Names are ISO date strings representing the start of each period.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' length(gs)
#' names(gs)
#'
#' @export
build_graph_series <- function(data,
                               window           = c("day", "week", "month", "quarter", "year"),
                               directed         = TRUE,
                               weighted         = TRUE,
                               remove_self_loops = TRUE) {
  data   <- validate_social_events(data)
  window <- match.arg(window)

  data <- dplyr::mutate(data,
    .period = floor_time_window(.data$timestamp, window = window)
  )

  split_data <- split(data, data$.period)

  out <- purrr::imap(split_data, function(chunk, nm) {
    build_graph_snapshot(
      dplyr::select(chunk, -".period"),
      directed          = directed,
      weighted          = weighted,
      remove_self_loops = remove_self_loops
    )
  })

  class(out) <- c("social_graph_series", class(out))
  out
}

#' @export
print.social_graph_series <- function(x, ...) {
  cat("<social_graph_series>\n")
  cat("  Periods   :", length(x), "\n")
  cat("  Window    :", names(x)[1], "to", names(x)[length(x)], "\n")
  cat("  Directed  :", igraph::is_directed(x[[1]]), "\n")
  cat("  Avg nodes :", round(mean(sapply(x, igraph::vcount))), "\n")
  cat("  Avg edges :", round(mean(sapply(x, igraph::ecount))), "\n")
  invisible(x)
}

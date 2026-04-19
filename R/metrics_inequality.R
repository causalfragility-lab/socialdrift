#' Compute degree inequality over time
#'
#' Uses the Gini coefficient of node degree as a measure of how unequally
#' connections are distributed across the network. A Gini of 0 means perfect
#' equality; 1 means one node holds all connections.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#' @param mode Degree mode: `"all"` (default), `"in"`, or `"out"`.
#'
#' @return A tibble with columns `period`, `degree_gini`, and `degree_mean`.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' degree_inequality_ts(gs)
#'
#' @export
degree_inequality_ts <- function(graph_series, mode = c("all", "in", "out")) {
  check_graph_series(graph_series)
  mode <- match.arg(mode)
  tibble::tibble(
    period       = names(graph_series),
    degree_gini  = purrr::map_dbl(graph_series, ~ {
      deg <- igraph::degree(.x, mode = mode)
      gini_coeff(deg)
    }),
    degree_mean  = purrr::map_dbl(graph_series, ~ {
      mean(igraph::degree(.x, mode = mode))
    })
  )
}

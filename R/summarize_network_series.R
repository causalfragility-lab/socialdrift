#' Summarize structural metrics across all periods in a graph series
#'
#' A convenience wrapper that computes density, reciprocity, clustering,
#' and degree inequality for every period and returns a single wide tibble.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with one row per period and columns for all key metrics.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' summarize_network_series(gs)
#'
#' @export
summarize_network_series <- function(graph_series) {
  check_graph_series(graph_series)

  dens  <- network_density_ts(graph_series)
  recip <- reciprocity_ts(graph_series)
  clust <- clustering_ts(graph_series)
  ineq  <- degree_inequality_ts(graph_series)

  dens |>
    dplyr::left_join(recip, by = "period") |>
    dplyr::left_join(clust, by = "period") |>
    dplyr::left_join(ineq,  by = "period")
}

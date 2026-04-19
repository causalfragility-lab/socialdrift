#' Compute global clustering coefficient over time
#'
#' The global clustering coefficient (transitivity) measures the tendency
#' of nodes to form tightly connected triangles. High values indicate
#' clique-like structure; low values indicate sparse or tree-like networks.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with columns `period` and `clustering`.
#'   Returns `NA` for periods with fewer than 3 nodes.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' clustering_ts(gs)
#'
#' @export
clustering_ts <- function(graph_series) {
  check_graph_series(graph_series)
  tibble::tibble(
    period     = names(graph_series),
    clustering = purrr::map_dbl(graph_series, ~ {
      if (igraph::vcount(.x) < 3) NA_real_
      else igraph::transitivity(.x, type = "global")
    })
  )
}

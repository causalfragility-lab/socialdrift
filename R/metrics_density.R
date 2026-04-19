#' Compute network density over time
#'
#' Network density is the proportion of possible edges that are present.
#' High density indicates a highly connected network; low density indicates
#' sparse interaction.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{period}{Time period (character, ISO date).}
#'     \item{n_nodes}{Number of active nodes.}
#'     \item{n_edges}{Number of edges.}
#'     \item{density}{Network density (0--1).}
#'   }
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' network_density_ts(gs)
#'
#' @export
network_density_ts <- function(graph_series) {
  check_graph_series(graph_series)
  tibble::tibble(
    period  = names(graph_series),
    n_nodes = purrr::map_dbl(graph_series, igraph::vcount),
    n_edges = purrr::map_dbl(graph_series, igraph::ecount),
    density = purrr::map_dbl(graph_series, ~ {
      if (igraph::vcount(.x) < 2) NA_real_ else igraph::edge_density(.x)
    })
  )
}

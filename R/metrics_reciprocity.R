#' Compute reciprocity over time
#'
#' Reciprocity is the proportion of edges that have a mutual counterpart.
#' Applies to directed graphs only. High reciprocity suggests mutual
#' engagement; low reciprocity suggests broadcast-like interaction.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with columns `period` and `reciprocity`.
#'   Returns `NA` for undirected graphs or periods with no edges.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' reciprocity_ts(gs)
#'
#' @export
reciprocity_ts <- function(graph_series) {
  check_graph_series(graph_series)
  tibble::tibble(
    period      = names(graph_series),
    reciprocity = purrr::map_dbl(graph_series, ~ {
      if (!igraph::is_directed(.x) || igraph::ecount(.x) == 0) {
        NA_real_
      } else {
        igraph::reciprocity(.x)
      }
    })
  )
}

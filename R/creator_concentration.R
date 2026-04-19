#' Measure concentration of incoming attention over time
#'
#' Computes how concentrated incoming interactions (in-degree) are across
#' the network. High concentration indicates a few nodes receive most attention.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#' @param p Proportion of top actors to include in concentration measures.
#'   Default `0.10` (top 10%).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{period}{Time period.}
#'     \item{indegree_gini}{Gini coefficient of in-degree (0 = equal, 1 = monopoly).}
#'     \item{top_p_share}{Share of total in-degree held by top-p actors.}
#'   }
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' creator_concentration(gs)
#'
#' @export
creator_concentration <- function(graph_series, p = 0.10) {
  check_graph_series(graph_series)
  tibble::tibble(
    period       = names(graph_series),
    indegree_gini = purrr::map_dbl(graph_series, ~ {
      gini_coeff(igraph::degree(.x, mode = "in"))
    }),
    top_p_share  = purrr::map_dbl(graph_series, ~ {
      top_p_share(igraph::degree(.x, mode = "in"), p = p)
    })
  )
}


#' Visibility Concentration Index (VCI)
#'
#' A composite index measuring whether visibility (incoming attention) is
#' concentrated in a small fraction of users. Combines the Gini coefficient
#' and top-share into a single interpretable score.
#'
#' @details
#' \deqn{VCI = 0.6 \cdot Gini + 0.4 \cdot top\text{-}10\%\text{-}share}
#'
#' Values near 1 indicate extreme concentration; values near 0 indicate
#' roughly equal attention distribution.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with columns `period` and `vci`.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' visibility_concentration_index(gs)
#'
#' @export
visibility_concentration_index <- function(graph_series) {
  check_graph_series(graph_series)
  cc <- creator_concentration(graph_series, p = 0.10)
  cc |>
    dplyr::mutate(
      vci = 0.6 * .data$indegree_gini + 0.4 * .data$top_p_share
    ) |>
    dplyr::select("period", "vci")
}

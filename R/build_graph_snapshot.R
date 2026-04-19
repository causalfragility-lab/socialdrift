#' Build a weighted graph snapshot from event data
#'
#' Aggregates event-level interactions into a directed or undirected igraph
#' object for a specified time window.
#'
#' @param data A standardized social event tibble (output of [as_social_events()]).
#' @param start Optional start date/time (inclusive). Events before this are excluded.
#' @param end Optional end date/time (exclusive). Events from this point are excluded.
#' @param directed Logical; if `TRUE` (default) the graph is directed.
#' @param weighted Logical; if `TRUE` (default) edge weights reflect total
#'   event count between each pair.
#' @param remove_self_loops Logical; if `TRUE` (default) self-loops are removed.
#'
#' @return An igraph object. Returns an empty graph if no events fall in the
#'   specified window.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' g  <- build_graph_snapshot(ev)
#' igraph::vcount(g)
#' igraph::ecount(g)
#'
#' @export
build_graph_snapshot <- function(data,
                                 start            = NULL,
                                 end              = NULL,
                                 directed         = TRUE,
                                 weighted         = TRUE,
                                 remove_self_loops = TRUE) {
  data <- validate_social_events(data)

  if (!is.null(start)) data <- dplyr::filter(data, .data$timestamp >= start)
  if (!is.null(end))   data <- dplyr::filter(data, .data$timestamp <  end)
  if (remove_self_loops) {
    data <- dplyr::filter(data, .data$actor_id != .data$target_id)
  }

  if (nrow(data) == 0) {
    return(igraph::make_empty_graph(n = 0, directed = directed))
  }

  edges <- data |>
    dplyr::group_by(.data$actor_id, .data$target_id) |>
    dplyr::summarise(weight = sum(.data$weight, na.rm = TRUE), .groups = "drop")

  g <- igraph::graph_from_data_frame(
    d        = edges,
    directed = directed,
    vertices = NULL
  )

  if (weighted) igraph::E(g)$weight <- edges$weight

  g
}

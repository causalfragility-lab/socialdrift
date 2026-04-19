#' Detect communities over time using the Louvain method
#'
#' Applies the Louvain community detection algorithm to each snapshot in a
#' graph series. Directed graphs are coerced to undirected for community
#' detection (standard practice for modularity-based methods).
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{period}{Time period (character).}
#'     \item{n_communities}{Number of communities detected.}
#'     \item{modularity}{Modularity score (higher = more modular community structure).}
#'     \item{largest_community_size}{Size of the largest community.}
#'     \item{singleton_count}{Number of isolated single-node communities.}
#'   }
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' detect_communities_ts(gs)
#'
#' @export
detect_communities_ts <- function(graph_series) {
  check_graph_series(graph_series)

  purrr::imap_dfr(graph_series, function(g, nm) {
    if (igraph::vcount(g) < 2 || igraph::ecount(g) < 1) {
      return(tibble::tibble(
        period                 = nm,
        n_communities          = NA_integer_,
        modularity             = NA_real_,
        largest_community_size = NA_integer_,
        singleton_count        = NA_integer_
      ))
    }

    g2   <- if (igraph::is_directed(g)) igraph::as.undirected(g, mode = "collapse") else g
    comm <- igraph::cluster_louvain(g2)
    sizes <- lengths(igraph::communities(comm))

    tibble::tibble(
      period                 = nm,
      n_communities          = length(sizes),
      modularity             = igraph::modularity(comm),
      largest_community_size = max(sizes),
      singleton_count        = sum(sizes == 1L)
    )
  })
}

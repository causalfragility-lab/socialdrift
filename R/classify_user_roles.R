#' Classify structural roles of users in a graph
#'
#' Assigns each node a structural role based on its in-degree, out-degree,
#' and betweenness centrality relative to the rest of the network.
#'
#' @details
#' Role classification rules (applied in order):
#' \enumerate{
#'   \item **isolated** --- degree = 0 in both directions.
#'   \item **bridge** --- betweenness in top quartile.
#'   \item **core** --- both in- and out-degree in top quartile.
#'   \item **popular** --- in-degree in top quartile, out-degree below.
#'   \item **broadcaster** --- out-degree in top quartile, in-degree below.
#'   \item **peripheral** --- all other nodes.
#' }
#'
#' @param graph An igraph object.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{node}{Node name.}
#'     \item{indegree}{In-degree.}
#'     \item{outdegree}{Out-degree.}
#'     \item{betweenness}{Betweenness centrality.}
#'     \item{role}{Assigned structural role.}
#'   }
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' g  <- build_graph_snapshot(ev)
#' classify_user_roles(g)
#'
#' @export
classify_user_roles <- function(graph) {
  check_igraph(graph)

  if (igraph::vcount(graph) == 0) {
    return(tibble::tibble(
      node        = character(),
      indegree    = numeric(),
      outdegree   = numeric(),
      betweenness = numeric(),
      role        = character()
    ))
  }

  indeg <- igraph::degree(graph, mode = "in")
  outdeg <- igraph::degree(graph, mode = "out")
  btw   <- igraph::betweenness(graph,
                                directed = igraph::is_directed(graph),
                                normalized = TRUE)

  # Quantile cutoffs (only for non-zero values)
  q75 <- function(x) stats::quantile(x, 0.75, na.rm = TRUE)
  indeg_cut  <- q75(indeg)
  outdeg_cut <- q75(outdeg)
  btw_cut    <- q75(btw)

  role <- purrr::pmap_chr(
    list(indeg, outdeg, btw),
    function(i, o, b) {
      if ((i + o) == 0)                                    return("isolated")
      if (b >= btw_cut && btw_cut > 0)                    return("bridge")
      if (i >= indeg_cut && o >= outdeg_cut)              return("core")
      if (i >= indeg_cut && o <  outdeg_cut)              return("popular")
      if (o >= outdeg_cut && i <  indeg_cut)              return("broadcaster")
      "peripheral"
    }
  )

  tibble::tibble(
    node        = igraph::V(graph)$name,
    indegree    = indeg,
    outdegree   = outdeg,
    betweenness = btw,
    role        = role
  )
}

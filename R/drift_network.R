#' Compute the Network Drift Index (NDI) over time
#'
#' The Network Drift Index quantifies how much a network's structure changes
#' between consecutive time periods. It combines changes in edge turnover,
#' degree distribution, community structure, and centralization into a single
#' composite score.
#'
#' @details
#' \deqn{NDI_t = w_1 \Delta_{edges} + w_2 \Delta_{degree} + w_3 \Delta_{community} + w_4 \Delta_{centrality}}
#'
#' Default weights: `w1 = 0.30, w2 = 0.25, w3 = 0.25, w4 = 0.20`.
#'
#' Component definitions:
#' \describe{
#'   \item{edge_turnover}{Jaccard distance between edge sets of adjacent snapshots.}
#'   \item{degree_shift}{Jensen-Shannon-like divergence of degree distributions.}
#'   \item{modularity_change}{Absolute change in community modularity.}
#'   \item{centralization_change}{Absolute change in degree centralization.}
#' }
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#' @param w1 Weight for edge turnover. Default `0.30`.
#' @param w2 Weight for degree distribution shift. Default `0.25`.
#' @param w3 Weight for community structure change. Default `0.25`.
#' @param w4 Weight for centralization change. Default `0.20`.
#'
#' @return A tibble with columns `period`, `edge_turnover`,
#'   `degree_shift`, `modularity_change`, `centralization_change`, and `ndi`.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' network_drift(gs)
#'
#' @export
network_drift <- function(graph_series,
                          w1 = 0.30, w2 = 0.25, w3 = 0.25, w4 = 0.20) {
  check_graph_series(graph_series)
  if (abs(w1 + w2 + w3 + w4 - 1) > 1e-9) {
    stop("Weights w1, w2, w3, w4 must sum to 1.", call. = FALSE)
  }

  periods <- names(graph_series)
  n       <- length(periods)

  if (n < 2) {
    message("Only one period in series; NDI requires at least two periods.")
    return(tibble::tibble(
      period               = periods,
      edge_turnover        = NA_real_,
      degree_shift         = NA_real_,
      modularity_change    = NA_real_,
      centralization_change = NA_real_,
      ndi                  = NA_real_
    ))
  }

  # Helper: edge set as character "u1->u2"
  edge_set <- function(g) {
    if (igraph::ecount(g) == 0) return(character(0))
    el <- igraph::as_edgelist(g)
    paste(el[, 1], el[, 2], sep = "->")
  }

  # Helper: modularity of undirected version
  graph_modularity <- function(g) {
    if (igraph::vcount(g) < 2 || igraph::ecount(g) < 1) return(NA_real_)
    g2 <- if (igraph::is_directed(g)) igraph::as.undirected(g, mode = "collapse") else g
    igraph::modularity(igraph::cluster_louvain(g2))
  }

  # Helper: degree centralization
  graph_centralization <- function(g) {
    deg <- igraph::degree(g, mode = "all")
    n   <- length(deg)
    if (n < 2) return(NA_real_)
    max_possible <- (n - 1L) * (n - 2L)
    if (max_possible == 0) return(0)
    sum(max(deg) - deg) / max_possible
  }

  # Helper: distribution divergence (TV distance)
  dist_divergence <- function(x, y) {
    all_vals <- union(x, y)
    px <- table(factor(x, levels = all_vals)) / length(x)
    py <- table(factor(y, levels = all_vals)) / length(y)
    0.5 * sum(abs(as.numeric(px) - as.numeric(py)))
  }

  rows <- vector("list", n)
  rows[[1]] <- tibble::tibble(
    period                = periods[1],
    edge_turnover         = NA_real_,
    degree_shift          = NA_real_,
    modularity_change     = NA_real_,
    centralization_change = NA_real_,
    ndi                   = NA_real_
  )

  for (i in seq(2, n)) {
    g_prev <- graph_series[[i - 1L]]
    g_curr <- graph_series[[i]]

    # Edge Jaccard distance
    e_prev <- edge_set(g_prev)
    e_curr <- edge_set(g_curr)
    union_e <- union(e_prev, e_curr)
    inter_e <- intersect(e_prev, e_curr)
    et <- if (length(union_e) == 0) 0
          else 1 - length(inter_e) / length(union_e)

    # Degree distribution shift
    d_prev <- igraph::degree(g_prev, mode = "all")
    d_curr <- igraph::degree(g_curr, mode = "all")
    ds <- if (length(d_prev) == 0 || length(d_curr) == 0) NA_real_
          else dist_divergence(d_prev, d_curr)

    # Modularity change
    m_prev <- graph_modularity(g_prev)
    m_curr <- graph_modularity(g_curr)
    mc <- if (is.na(m_prev) || is.na(m_curr)) NA_real_
          else abs(m_curr - m_prev)

    # Centralization change
    c_prev <- graph_centralization(g_prev)
    c_curr <- graph_centralization(g_curr)
    cc <- if (is.na(c_prev) || is.na(c_curr)) NA_real_
          else abs(c_curr - c_prev)

    # NDI composite
    components <- c(et, ds, mc, cc)
    weights    <- c(w1, w2, w3, w4)
    ndi <- if (all(!is.na(components))) sum(weights * components)
           else sum(weights[!is.na(components)] * components[!is.na(components)]) /
                sum(weights[!is.na(components)])

    rows[[i]] <- tibble::tibble(
      period                = periods[i],
      edge_turnover         = et,
      degree_shift          = ds,
      modularity_change     = mc,
      centralization_change = cc,
      ndi                   = ndi
    )
  }

  do.call(rbind, rows)
}


#' Compute the Network Drift Index (alias for network_drift)
#'
#' @inheritParams network_drift
#' @return See [network_drift()].
#' @export
network_drift_index <- network_drift

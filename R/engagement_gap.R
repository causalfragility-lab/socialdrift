#' Compute engagement gap between user groups over time
#'
#' Compares average in-degree (received attention) and out-degree (initiated
#' interactions) between two user groups defined in the original event data.
#'
#' @param data A standardized social event tibble with `actor_group` and
#'   `target_group` columns (set via [as_social_events()]).
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#' @param group_var Column in `data` identifying actor groups. Default `"actor_group"`.
#' @param window Aggregation window matching the one used in `graph_series`.
#'   Default `"month"`.
#'
#' @return A tibble with columns `period`, `group`, `mean_indegree`,
#'   `mean_outdegree`, and `engagement_ratio` (indegree / outdegree).
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(
#'   sim_social_events,
#'   actor_group  = "actor_group",
#'   target_group = "target_group"
#' )
#' gs  <- build_graph_series(ev, window = "month")
#' engagement_gap(ev, gs, group_var = "actor_group", window = "month")
#'
#' @export
engagement_gap <- function(data,
                           graph_series,
                           group_var = "actor_group",
                           window    = c("month", "week", "day", "quarter", "year")) {
  window <- match.arg(window)
  check_graph_series(graph_series)

  if (!group_var %in% names(data)) {
    stop("`", group_var, "` not found in data. ",
         "Did you pass `actor_group` to `as_social_events()`?", call. = FALSE)
  }

  data <- dplyr::mutate(data,
    .period = floor_time_window(.data$timestamp, window = window)
  )

  purrr::imap_dfr(graph_series, function(g, nm) {
    period_data <- dplyr::filter(data, as.character(data$.period) == nm)
    if (nrow(period_data) == 0 || igraph::vcount(g) == 0) return(NULL)

    node_groups <- period_data |>
      dplyr::select(node = "actor_id", group = dplyr::all_of(group_var)) |>
      dplyr::distinct(.data$node, .keep_all = TRUE)

    deg_tbl <- tibble::tibble(
      node      = igraph::V(g)$name,
      indegree  = igraph::degree(g, mode = "in"),
      outdegree = igraph::degree(g, mode = "out")
    )

    deg_tbl |>
      dplyr::left_join(node_groups, by = "node") |>
      dplyr::filter(!is.na(.data$group)) |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(
        mean_indegree  = mean(.data$indegree,  na.rm = TRUE),
        mean_outdegree = mean(.data$outdegree, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        period           = nm,
        engagement_ratio = .data$mean_indegree / (.data$mean_outdegree + 1e-9),
        .before          = 1L
      )
  })
}


#' Audit network metrics by user group over time
#'
#' Computes per-group summaries of structural position (degree, betweenness)
#' across all periods in a graph series. Useful for detecting systematic
#' disparities in network access or visibility.
#'
#' @param data A standardized social event tibble with group membership columns.
#' @param graph_series A `social_graph_series`.
#' @param group_var Column name identifying actor groups. Default `"actor_group"`.
#' @param window Aggregation window. Default `"month"`.
#'
#' @return A tibble with one row per period x group, including:
#'   `mean_indegree`, `mean_outdegree`, `mean_betweenness`, `isolation_rate`
#'   (proportion of isolated nodes), and `n_users`.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(
#'   sim_social_events,
#'   actor_group  = "actor_group",
#'   target_group = "target_group"
#' )
#' gs <- build_graph_series(ev, window = "month")
#' audit_group_disparities(ev, gs)
#'
#' @export
audit_group_disparities <- function(data,
                                    graph_series,
                                    group_var = "actor_group",
                                    window    = c("month", "week", "day", "quarter", "year")) {
  window <- match.arg(window)
  check_graph_series(graph_series)

  if (!group_var %in% names(data)) {
    stop("`", group_var, "` not found in data.", call. = FALSE)
  }

  data <- dplyr::mutate(data,
    .period = floor_time_window(.data$timestamp, window = window)
  )

  purrr::imap_dfr(graph_series, function(g, nm) {
    period_data <- dplyr::filter(data, as.character(data$.period) == nm)
    if (nrow(period_data) == 0 || igraph::vcount(g) == 0) return(NULL)

    node_groups <- period_data |>
      dplyr::select(node = "actor_id", group = dplyr::all_of(group_var)) |>
      dplyr::distinct(.data$node, .keep_all = TRUE)

    btw <- igraph::betweenness(g,
                                directed   = igraph::is_directed(g),
                                normalized = TRUE)

    deg_tbl <- tibble::tibble(
      node        = igraph::V(g)$name,
      indegree    = igraph::degree(g, mode = "in"),
      outdegree   = igraph::degree(g, mode = "out"),
      betweenness = btw,
      isolated    = (igraph::degree(g, mode = "all") == 0L)
    )

    deg_tbl |>
      dplyr::left_join(node_groups, by = "node") |>
      dplyr::filter(!is.na(.data$group)) |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(
        n_users          = dplyr::n(),
        mean_indegree    = mean(.data$indegree,    na.rm = TRUE),
        mean_outdegree   = mean(.data$outdegree,   na.rm = TRUE),
        mean_betweenness = mean(.data$betweenness, na.rm = TRUE),
        isolation_rate   = mean(.data$isolated,    na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(period = nm, .before = 1L)
  })
}

#' Track user structural roles over time
#'
#' Applies [classify_user_roles()] to every snapshot in a graph series and
#' returns a longitudinal tibble of role assignments.
#'
#' @param graph_series A `social_graph_series` (output of [build_graph_series()]).
#'
#' @return A tibble with columns `period`, `node`, `indegree`, `outdegree`,
#'   `betweenness`, and `role`.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' role_trajectories(gs)
#'
#' @export
role_trajectories <- function(graph_series) {
  check_graph_series(graph_series)

  purrr::imap_dfr(graph_series, function(g, nm) {
    classify_user_roles(g) |>
      dplyr::mutate(period = nm, .before = 1L)
  })
}


#' Role Mobility Index (RMI)
#'
#' Quantifies how much users move between structural roles across time periods.
#' A high RMI indicates a dynamic network where role transitions are frequent;
#' a low RMI indicates structural stability.
#'
#' @details
#' For each user observed in at least two consecutive periods, the RMI counts
#' the proportion of adjacent-period pairs where the role changed. The
#' overall RMI is the mean across all such users.
#'
#' @param role_tbl Output of [role_trajectories()].
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{rmi_global}{Overall Role Mobility Index (0--1).}
#'     \item{n_users_tracked}{Number of users with >= 2 observed periods.}
#'     \item{mean_transitions}{Mean number of role transitions per user.}
#'   }
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' rt <- role_trajectories(gs)
#' role_mobility_index(rt)
#'
#' @export
role_mobility_index <- function(role_tbl) {
  if (!is.data.frame(role_tbl) || !all(c("period", "node", "role") %in% names(role_tbl))) {
    stop("`role_tbl` must be output of `role_trajectories()`.", call. = FALSE)
  }

  user_rmi <- role_tbl |>
    dplyr::arrange(.data$node, .data$period) |>
    dplyr::group_by(.data$node) |>
    dplyr::summarise(
      n_periods     = dplyr::n(),
      n_transitions = sum(.data$role != dplyr::lag(.data$role), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_periods >= 2L)

  if (nrow(user_rmi) == 0) {
    return(tibble::tibble(rmi_global = NA_real_,
                          n_users_tracked = 0L,
                          mean_transitions = NA_real_))
  }

  tibble::tibble(
    rmi_global       = mean(user_rmi$n_transitions / (user_rmi$n_periods - 1L),
                            na.rm = TRUE),
    n_users_tracked  = nrow(user_rmi),
    mean_transitions = mean(user_rmi$n_transitions, na.rm = TRUE)
  )
}

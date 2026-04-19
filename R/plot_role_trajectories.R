#' Plot role composition over time
#'
#' Shows how the proportion of each structural role changes across periods
#' as a stacked bar chart.
#'
#' @param role_tbl Output of [role_trajectories()].
#' @param type `"stacked"` (default) for a stacked proportional bar chart,
#'   or `"line"` for a line chart of role proportions.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' rt <- role_trajectories(gs)
#' plot_role_trajectories(rt)
#'
#' @export
plot_role_trajectories <- function(role_tbl, type = c("stacked", "line")) {
  type <- match.arg(type)

  if (!is.data.frame(role_tbl) || !all(c("period", "node", "role") %in% names(role_tbl))) {
    stop("`role_tbl` must be output of `role_trajectories()`.", call. = FALSE)
  }

  role_colours <- c(
    isolated    = "#d73027",
    peripheral  = "#fc8d59",
    broadcaster = "#fee090",
    popular     = "#e0f3f8",
    core        = "#4575b4",
    bridge      = "#1a9641"
  )

  prop_tbl <- role_tbl |>
    dplyr::group_by(.data$period, .data$role) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$period) |>
    dplyr::mutate(prop = .data$n / sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .period_date = as.Date(.data$period),
      role         = factor(.data$role,
                            levels = c("isolated", "peripheral", "broadcaster",
                                       "popular", "core", "bridge"))
    )

  if (type == "stacked") {
    ggplot2::ggplot(prop_tbl,
                    ggplot2::aes(x    = .data$.period_date,
                                 y    = .data$prop,
                                 fill = .data$role)) +
      ggplot2::geom_col(width = 20) +
      ggplot2::scale_fill_manual(values = role_colours, name = "Role",
                                 drop = FALSE) +
      ggplot2::labs(
        x     = "Period",
        y     = "Proportion of users",
        title = "Structural role composition over time"
      ) +
      ggplot2::theme_minimal(base_size = 13)

  } else {
    ggplot2::ggplot(prop_tbl,
                    ggplot2::aes(x      = .data$.period_date,
                                 y      = .data$prop,
                                 colour = .data$role,
                                 group  = .data$role)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 2.2) +
      ggplot2::scale_colour_manual(values = role_colours, name = "Role",
                                   drop = FALSE) +
      ggplot2::labs(
        x     = "Period",
        y     = "Proportion of users",
        title = "Structural role trajectories over time"
      ) +
      ggplot2::theme_minimal(base_size = 13)
  }
}

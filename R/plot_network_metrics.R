#' Plot one or more time-varying network metrics
#'
#' Produces a line chart of a metric (or faceted chart for multiple metrics)
#' from the output of metric functions such as [network_density_ts()],
#' [reciprocity_ts()], or [summarize_network_series()].
#'
#' @param data A tibble with a `period` column and at least one numeric metric.
#' @param metric Character vector of column name(s) to plot. If more than one,
#'   a faceted chart is produced. If `NULL`, all numeric columns except
#'   `period` are plotted.
#' @param title Plot title. Default `"Network Metrics Over Time"`.
#' @param colour Line colour. Default `"#2c7bb6"`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' data(sim_social_events)
#' ev  <- as_social_events(sim_social_events)
#' gs  <- build_graph_series(ev, window = "month")
#' tbl <- network_density_ts(gs)
#' plot_network_metrics(tbl, metric = "density")
#'
#' @export
plot_network_metrics <- function(data,
                                 metric = NULL,
                                 title  = "Network Metrics Over Time",
                                 colour = "#2c7bb6") {
  if (is.null(metric)) {
    metric <- setdiff(names(data)[sapply(data, is.numeric)], "period")
  }
  if (length(metric) == 0) stop("No numeric metric columns found.", call. = FALSE)

  data <- dplyr::mutate(data, .period_date = as.Date(.data$period))

  if (length(metric) == 1L) {
    ggplot2::ggplot(data,
                    ggplot2::aes(x = .data$.period_date,
                                 y = .data[[metric]])) +
      ggplot2::geom_line(colour = colour, linewidth = 0.9) +
      ggplot2::geom_point(colour = colour, size = 2.5) +
      ggplot2::labs(x = "Period", y = metric, title = title) +
      ggplot2::theme_minimal(base_size = 13)

  } else {
    long_data <- tidyr::pivot_longer(
      data,
      cols      = dplyr::all_of(metric),
      names_to  = "metric_name",
      values_to = "value"
    )
    ggplot2::ggplot(long_data,
                    ggplot2::aes(x = .data$.period_date,
                                 y = .data$value)) +
      ggplot2::geom_line(colour = colour, linewidth = 0.9) +
      ggplot2::geom_point(colour = colour, size = 2) +
      ggplot2::facet_wrap(~ metric_name, scales = "free_y") +
      ggplot2::labs(x = "Period", y = NULL, title = title) +
      ggplot2::theme_minimal(base_size = 12)
  }
}

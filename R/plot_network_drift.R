#' Plot the Network Drift Index over time
#'
#' Visualises the composite NDI and its four component scores across periods.
#'
#' @param drift_tbl Output of [network_drift()].
#' @param show_components Logical; if `TRUE` (default), component scores are
#'   shown as a stacked area chart beneath the NDI line.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' dt <- network_drift(gs)
#' plot_network_drift(dt)
#'
#' @export
plot_network_drift <- function(drift_tbl, show_components = TRUE) {
  if (!is.data.frame(drift_tbl) || !"ndi" %in% names(drift_tbl)) {
    stop("`drift_tbl` must be output of `network_drift()`.", call. = FALSE)
  }

  tbl <- drift_tbl |>
    dplyr::filter(!is.na(.data$ndi)) |>
    dplyr::mutate(.period_date = as.Date(.data$period))

  if (nrow(tbl) == 0) {
    message("No non-NA NDI values to plot.")
    return(ggplot2::ggplot())
  }

  if (show_components) {
    component_cols <- c("edge_turnover", "degree_shift",
                        "modularity_change", "centralization_change")
    available <- intersect(component_cols, names(tbl))

    long_comp <- tidyr::pivot_longer(
      tbl,
      cols      = dplyr::all_of(available),
      names_to  = "component",
      values_to = "value"
    )

    comp_colours <- c(
      edge_turnover         = "#d7191c",
      degree_shift          = "#fdae61",
      modularity_change     = "#abd9e9",
      centralization_change = "#2c7bb6"
    )

    p <- ggplot2::ggplot(long_comp,
                         ggplot2::aes(x     = .data$.period_date,
                                      y     = .data$value,
                                      fill  = .data$component,
                                      colour = .data$component)) +
      ggplot2::geom_col(position = "stack", alpha = 0.5, width = 20) +
      ggplot2::geom_line(
        data    = tbl,
        mapping = ggplot2::aes(x = .data$.period_date,
                               y = .data$ndi),
        inherit.aes = FALSE,
        colour      = "black",
        linewidth   = 1.2
      ) +
      ggplot2::geom_point(
        data    = tbl,
        mapping = ggplot2::aes(x = .data$.period_date,
                               y = .data$ndi),
        inherit.aes = FALSE,
        colour      = "black",
        size        = 3
      ) +
      ggplot2::scale_fill_manual(values = comp_colours, name = "Component") +
      ggplot2::scale_colour_manual(values = comp_colours, guide = "none") +
      ggplot2::labs(
        x     = "Period",
        y     = "NDI / Component contribution",
        title = "Network Drift Index over time",
        caption = "Black line = composite NDI; bars = weighted component scores"
      ) +
      ggplot2::theme_minimal(base_size = 13)

  } else {
    p <- ggplot2::ggplot(tbl,
                         ggplot2::aes(x = .data$.period_date,
                                      y = .data$ndi)) +
      ggplot2::geom_line(colour = "#2c7bb6", linewidth = 1.1) +
      ggplot2::geom_point(colour = "#2c7bb6", size = 3) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
      ggplot2::labs(
        x     = "Period",
        y     = "Network Drift Index (NDI)",
        title = "Network Drift Index over time"
      ) +
      ggplot2::theme_minimal(base_size = 13)
  }

  p
}

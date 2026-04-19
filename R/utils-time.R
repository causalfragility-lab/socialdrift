#' Floor timestamps to a time window
#'
#' @param x A POSIXct, POSIXt, or Date vector.
#' @param window One of `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`.
#'
#' @return A Date vector aligned to the start of each period.
#' @keywords internal
floor_time_window <- function(x, window = c("day", "week", "month", "quarter", "year")) {
  window <- match.arg(window)
  x_date <- as.Date(x)

  switch(window,
    day     = x_date,
    week    = x_date - as.integer(format(x_date, "%u")) + 1L,
    month   = as.Date(format(x_date, "%Y-%m-01")),
    quarter = {
      mo <- as.integer(format(x_date, "%m"))
      q_start_mo <- ((mo - 1L) %/% 3L) * 3L + 1L
      as.Date(paste(format(x_date, "%Y"),
                    sprintf("%02d", q_start_mo), "01", sep = "-"))
    },
    year    = as.Date(format(x_date, "%Y-01-01")),
    stop("Unsupported window: '", window, "'", call. = FALSE)
  )
}


#' Compute Gini coefficient
#'
#' @param x A non-negative numeric vector.
#' @return A scalar Gini coefficient between 0 and 1.
#' @keywords internal
gini_coeff <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) == 0 || sum(x) == 0) return(0)
  x <- sort(x)
  n <- length(x)
  idx <- seq_len(n)
  (2 * sum(idx * x) / (n * sum(x))) - (n + 1L) / n
}


#' Get top-p share of a distribution
#'
#' @param x Non-negative numeric vector.
#' @param p Proportion (0--1) of top actors.
#' @return Share of total held by top-p proportion.
#' @keywords internal
top_p_share <- function(x, p = 0.10) {
  x <- x[!is.na(x)]
  if (length(x) == 0 || sum(x) == 0) return(NA_real_)
  x <- sort(x, decreasing = TRUE)
  k <- max(1L, ceiling(length(x) * p))
  sum(x[seq_len(k)]) / sum(x)
}

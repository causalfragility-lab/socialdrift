#' Coerce a data frame into a standardized social event table
#'
#' Validates and standardizes a data frame of user-to-user interaction events
#' for use in temporal social network analysis with \pkg{socialdrift}.
#'
#' @param data A data frame or tibble containing interaction events.
#' @param actor Column name (character) for the source actor. Default `"actor_id"`.
#' @param target Column name (character) for the target actor. Default `"target_id"`.
#' @param time Column name (character) for the event timestamp. Must be
#'   `POSIXct`, `POSIXt`, or `Date`. Default `"timestamp"`.
#' @param event_type Optional column name (character) for interaction type
#'   (e.g., `"follow"`, `"reply"`). If `NULL`, defaults to `"interaction"`.
#' @param weight Optional column name (character) for edge weight. If `NULL`,
#'   defaults to `1` for all events.
#' @param actor_group Optional column name (character) for actor group membership
#'   (used in group disparity analyses).
#' @param target_group Optional column name (character) for target group membership.
#'
#' @return A tibble of class `social_events` with standardized columns:
#'   `actor_id`, `target_id`, `timestamp`, `event_type`, `weight`, and
#'   optionally `actor_group`, `target_group`.
#'
#' @examples
#' events <- data.frame(
#'   from   = c("u1", "u1", "u2", "u3"),
#'   to     = c("u2", "u3", "u3", "u4"),
#'   when   = as.POSIXct(c("2025-01-01", "2025-01-03",
#'                          "2025-01-04", "2025-01-06"))
#' )
#' ev <- as_social_events(events, actor = "from", target = "to", time = "when")
#' ev
#'
#' @export
as_social_events <- function(data,
                             actor       = "actor_id",
                             target      = "target_id",
                             time        = "timestamp",
                             event_type  = NULL,
                             weight      = NULL,
                             actor_group = NULL,
                             target_group = NULL) {
  data <- tibble::as_tibble(data)

  required <- c(actor, target, time)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  out <- dplyr::transmute(
    data,
    actor_id   = .data[[actor]],
    target_id  = .data[[target]],
    timestamp  = .data[[time]]
  )
  out$event_type <- if (!is.null(event_type) && event_type %in% names(data)) {
    data[[event_type]]
  } else {
    "interaction"
  }
  out$weight <- if (!is.null(weight) && weight %in% names(data)) {
    as.numeric(data[[weight]])
  } else {
    1
  }

  if (!is.null(actor_group) && actor_group %in% names(data)) {
    out$actor_group <- data[[actor_group]]
  }
  if (!is.null(target_group) && target_group %in% names(data)) {
    out$target_group <- data[[target_group]]
  }

  if (!inherits(out$timestamp, c("POSIXct", "POSIXt", "Date"))) {
    stop("`", time, "` must be POSIXct, POSIXt, or Date. ",
         "Use `as.POSIXct()` or `as.Date()` to convert.", call. = FALSE)
  }

  out <- validate_social_events(out)
  class(out) <- c("social_events", class(out))
  out
}

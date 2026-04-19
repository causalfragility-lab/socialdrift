#' Validate a standardized social event table
#'
#' Checks that a social events tibble contains required columns with valid
#' values. Called automatically by [as_social_events()].
#'
#' @param data A standardized social event tibble (output of [as_social_events()]).
#'
#' @return The validated tibble, invisibly.
#' @export
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' validate_social_events(ev)
validate_social_events <- function(data) {
  needed <- c("actor_id", "target_id", "timestamp", "event_type", "weight")
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required standardized columns: ",
         paste(missing_cols, collapse = ", "),
         "\nDid you run `as_social_events()` first?", call. = FALSE)
  }

  if (anyNA(data$actor_id))  stop("`actor_id` contains NA values.",  call. = FALSE)
  if (anyNA(data$target_id)) stop("`target_id` contains NA values.", call. = FALSE)
  if (anyNA(data$timestamp)) stop("`timestamp` contains NA values.", call. = FALSE)
  if (anyNA(data$weight))    stop("`weight` contains NA values.",    call. = FALSE)

  if (!is.numeric(data$weight)) {
    stop("`weight` must be numeric.", call. = FALSE)
  }

  if (any(data$weight < 0, na.rm = TRUE)) {
    warning("`weight` contains negative values. This may cause unexpected results.",
            call. = FALSE)
  }

  n_self <- sum(data$actor_id == data$target_id)
  if (n_self > 0) {
    message("Note: ", n_self, " self-loop(s) detected (actor_id == target_id). ",
            "These will be included in the graph unless filtered.")
  }

  tibble::as_tibble(data)
}

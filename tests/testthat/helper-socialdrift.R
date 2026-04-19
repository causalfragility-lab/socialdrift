# tests/testthat/helper-socialdrift.R
# Shared helpers loaded automatically by testthat

#' Minimal valid social events data frame
minimal_events <- function(n = 6) {
  data.frame(
    actor_id   = paste0("u", sample(1:5, n, replace = TRUE)),
    target_id  = paste0("u", sample(1:5, n, replace = TRUE)),
    timestamp  = as.POSIXct(
      seq.Date(as.Date("2025-01-01"), by = "day", length.out = n), tz = "UTC"
    ),
    event_type = "reply",
    weight     = 1L,
    stringsAsFactors = FALSE
  )
}

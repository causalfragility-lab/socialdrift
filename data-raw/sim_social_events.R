## data-raw/sim_social_events.R
## Run this once to generate the package's built-in toy dataset.
## Do NOT source directly --- use: source("data-raw/sim_social_events.R")

set.seed(42L)

n_users   <- 60L
n_events  <- 600L
users     <- paste0("u", seq_len(n_users))
groups    <- rep(c("A", "B", "C"), length.out = n_users)
names(groups) <- users

start_date <- as.Date("2025-01-01")
end_date   <- as.Date("2025-06-30")
all_dates  <- seq(start_date, end_date, by = "day")

# Preferential attachment weights: earlier users slightly more popular
pop_weight <- rev(seq_len(n_users))

actor_ids  <- sample(users, n_events, replace = TRUE)
target_ids <- sample(users, n_events, replace = TRUE, prob = pop_weight)
timestamps <- as.POSIXct(
  paste(sample(all_dates, n_events, replace = TRUE),
        sprintf("%02d:%02d:%02d",
                sample(0:23, n_events, replace = TRUE),
                sample(0:59, n_events, replace = TRUE),
                sample(0:59, n_events, replace = TRUE))),
  tz = "UTC"
)

sim_social_events <- tibble::tibble(
  actor_id     = actor_ids,
  target_id    = target_ids,
  timestamp    = timestamps,
  event_type   = sample(c("follow", "reply", "mention", "like", "repost"),
                         n_events, replace = TRUE,
                         prob     = c(0.15, 0.30, 0.20, 0.25, 0.10)),
  weight       = 1L,
  actor_group  = groups[actor_ids],
  target_group = groups[target_ids]
) |>
  dplyr::filter(actor_id != target_id) |>
  dplyr::arrange(timestamp)

usethis::use_data(sim_social_events, overwrite = TRUE)

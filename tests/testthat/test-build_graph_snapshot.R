make_events <- function(n = 5) {
  data.frame(
    actor_id  = paste0("u", sample(1:6, n, replace = TRUE)),
    target_id = paste0("u", sample(1:6, n, replace = TRUE)),
    timestamp = as.POSIXct(
      seq.Date(as.Date("2025-01-01"), by = "day", length.out = n), tz = "UTC"
    ),
    event_type = "reply",
    weight     = 1L,
    stringsAsFactors = FALSE
  )
}

test_that("build_graph_snapshot returns an igraph", {
  ev <- as_social_events(make_events(10))
  g  <- build_graph_snapshot(ev)
  expect_true(igraph::is_igraph(g))
})

test_that("build_graph_snapshot removes self-loops by default", {
  x <- data.frame(
    actor_id  = c("u1", "u1"),
    target_id = c("u1", "u2"),
    timestamp = as.POSIXct(c("2025-01-01", "2025-01-02"), tz = "UTC"),
    event_type = "reply", weight = 1L,
    stringsAsFactors = FALSE
  )
  ev <- as_social_events(x)
  g  <- build_graph_snapshot(ev)
  expect_equal(igraph::ecount(g), 1L)
})

test_that("build_graph_snapshot aggregates duplicate edges correctly", {
  x <- data.frame(
    actor_id  = c("u1", "u1", "u1"),
    target_id = c("u2", "u2", "u3"),
    timestamp = as.POSIXct(c("2025-01-01","2025-01-02","2025-01-03"), tz = "UTC"),
    event_type = "reply", weight = 1L,
    stringsAsFactors = FALSE
  )
  ev <- as_social_events(x)
  g  <- build_graph_snapshot(ev, weighted = TRUE)
  expect_equal(igraph::ecount(g), 2L)
  wts <- sort(igraph::E(g)$weight)
  expect_equal(wts, c(1, 2))
})

test_that("build_graph_snapshot respects date filters", {
  x <- data.frame(
    actor_id  = c("u1", "u2"),
    target_id = c("u2", "u3"),
    timestamp = as.POSIXct(c("2025-01-01","2025-03-01"), tz = "UTC"),
    event_type = "reply", weight = 1L,
    stringsAsFactors = FALSE
  )
  ev <- as_social_events(x)
  g  <- build_graph_snapshot(ev, end = as.POSIXct("2025-02-01", tz = "UTC"))
  expect_equal(igraph::ecount(g), 1L)
})

test_that("build_graph_snapshot returns empty graph on empty input", {
  x <- data.frame(
    actor_id  = character(), target_id = character(),
    timestamp = as.POSIXct(character(), tz = "UTC"),
    event_type = character(), weight = numeric(),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(as_social_events(x))
  g  <- build_graph_snapshot(ev)
  expect_equal(igraph::vcount(g), 0L)
})

make_series_events <- function() {
  data.frame(
    actor_id  = c("u1","u1","u2","u3","u4","u5"),
    target_id = c("u2","u3","u3","u4","u5","u6"),
    timestamp = as.POSIXct(
      c("2025-01-10","2025-01-20","2025-02-05",
        "2025-02-15","2025-03-01","2025-03-20"), tz = "UTC"),
    event_type = "reply", weight = 1L,
    stringsAsFactors = FALSE
  )
}

test_that("build_graph_series returns social_graph_series", {
  ev <- as_social_events(make_series_events())
  gs <- build_graph_series(ev, window = "month")
  expect_s3_class(gs, "social_graph_series")
})

test_that("build_graph_series splits into correct number of periods", {
  ev <- as_social_events(make_series_events())
  gs <- build_graph_series(ev, window = "month")
  expect_equal(length(gs), 3L)
})

test_that("build_graph_series names are ISO dates", {
  ev <- as_social_events(make_series_events())
  gs <- build_graph_series(ev, window = "month")
  expect_equal(names(gs), c("2025-01-01", "2025-02-01", "2025-03-01"))
})

test_that("build_graph_series all elements are igraph objects", {
  ev <- as_social_events(make_series_events())
  gs <- build_graph_series(ev, window = "month")
  expect_true(all(sapply(gs, igraph::is_igraph)))
})

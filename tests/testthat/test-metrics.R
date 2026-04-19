make_gs <- function() {
  x <- data.frame(
    actor_id  = c("u1","u1","u2","u3","u1","u4"),
    target_id = c("u2","u3","u3","u4","u4","u1"),
    timestamp = as.POSIXct(
      c("2025-01-10","2025-01-20","2025-02-05",
        "2025-02-15","2025-03-01","2025-03-20"), tz = "UTC"),
    event_type = "reply", weight = 1L,
    stringsAsFactors = FALSE
  )
  ev <- as_social_events(x)
  build_graph_series(ev, window = "month")
}

test_that("network_density_ts returns tibble with density column", {
  gs  <- make_gs()
  out <- network_density_ts(gs)
  expect_true(is.data.frame(out))
  expect_true("density" %in% names(out))
  expect_equal(nrow(out), length(gs))
})

test_that("reciprocity_ts returns values in [0,1] or NA", {
  gs  <- make_gs()
  out <- reciprocity_ts(gs)
  vals <- out$reciprocity[!is.na(out$reciprocity)]
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("clustering_ts returns values in [0,1] or NA", {
  gs  <- make_gs()
  out <- clustering_ts(gs)
  vals <- out$clustering[!is.na(out$clustering)]
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("degree_inequality_ts Gini is in [0,1]", {
  gs  <- make_gs()
  out <- degree_inequality_ts(gs)
  expect_true(all(out$degree_gini >= 0 & out$degree_gini <= 1))
})

test_that("summarize_network_series combines all metrics", {
  gs  <- make_gs()
  out <- summarize_network_series(gs)
  expect_true(all(c("density","reciprocity","clustering","degree_gini") %in% names(out)))
})

test_that("as_social_events standardizes column names", {
  x <- data.frame(
    a = c("u1", "u2"),
    b = c("u2", "u3"),
    t = as.POSIXct(c("2025-01-01", "2025-01-02"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out <- as_social_events(x, actor = "a", target = "b", time = "t")
  expect_true(all(c("actor_id", "target_id", "timestamp", "event_type", "weight") %in% names(out)))
})

test_that("as_social_events errors on missing required columns", {
  x <- data.frame(actor_id = "u1", target_id = "u2",
                  stringsAsFactors = FALSE)
  expect_error(as_social_events(x), "Missing required columns")
})

test_that("as_social_events errors on non-POSIXct timestamp", {
  x <- data.frame(
    actor_id  = "u1",
    target_id = "u2",
    timestamp = "2025-01-01",
    stringsAsFactors = FALSE
  )
  expect_error(as_social_events(x), "POSIXct")
})

test_that("as_social_events defaults event_type and weight correctly", {
  x <- data.frame(
    actor_id  = "u1",
    target_id = "u2",
    timestamp = as.POSIXct("2025-01-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out <- as_social_events(x)
  expect_equal(out$event_type, "interaction")
  expect_equal(out$weight, 1)
})

test_that("as_social_events returns correct nrow", {
  x <- data.frame(
    actor_id  = c("u1", "u2", "u3"),
    target_id = c("u2", "u3", "u4"),
    timestamp = as.POSIXct(c("2025-01-01","2025-01-02","2025-01-03"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out <- as_social_events(x)
  expect_equal(nrow(out), 3)
})

test_that("as_social_events propagates group columns", {
  x <- data.frame(
    actor_id     = "u1",
    target_id    = "u2",
    timestamp    = as.POSIXct("2025-01-01", tz = "UTC"),
    actor_group  = "A",
    target_group = "B",
    stringsAsFactors = FALSE
  )
  out <- as_social_events(x, actor_group = "actor_group", target_group = "target_group")
  expect_true("actor_group" %in% names(out))
  expect_equal(out$actor_group, "A")
})

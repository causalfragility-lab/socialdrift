make_gs2 <- function() {
  x <- data.frame(
    actor_id  = c("u1","u1","u2","u3","u1","u4","u5","u2","u3","u6"),
    target_id = c("u2","u3","u3","u4","u4","u1","u2","u6","u1","u4"),
    timestamp = as.POSIXct(
      c("2025-01-10","2025-01-20","2025-02-05","2025-02-15",
        "2025-03-01","2025-03-20","2025-01-05","2025-02-22",
        "2025-03-10","2025-03-25"), tz = "UTC"),
    event_type = "reply", weight = 1L,
    stringsAsFactors = FALSE
  )
  ev <- as_social_events(x)
  build_graph_series(ev, window = "month")
}

test_that("classify_user_roles returns expected columns", {
  gs <- make_gs2()
  r  <- classify_user_roles(gs[[1]])
  expect_true(all(c("node","indegree","outdegree","betweenness","role") %in% names(r)))
})

test_that("classify_user_roles assigns valid roles only", {
  gs    <- make_gs2()
  r     <- classify_user_roles(gs[[1]])
  valid <- c("isolated","peripheral","broadcaster","popular","core","bridge")
  expect_true(all(r$role %in% valid))
})

test_that("role_trajectories has period column", {
  gs <- make_gs2()
  rt <- role_trajectories(gs)
  expect_true("period" %in% names(rt))
  expect_true(nrow(rt) > 0)
})

test_that("role_mobility_index returns scalar in [0,1]", {
  gs  <- make_gs2()
  rt  <- role_trajectories(gs)
  rmi <- role_mobility_index(rt)
  expect_true(is.data.frame(rmi))
  if (!is.na(rmi$rmi_global)) {
    expect_true(rmi$rmi_global >= 0 && rmi$rmi_global <= 1)
  }
})

test_that("network_drift returns ndi column", {
  gs <- make_gs2()
  dt <- network_drift(gs)
  expect_true("ndi" %in% names(dt))
  expect_equal(nrow(dt), length(gs))
})

test_that("network_drift first period NDI is NA", {
  gs <- make_gs2()
  dt <- network_drift(gs)
  expect_true(is.na(dt$ndi[1]))
})

test_that("visibility_concentration_index VCI in [0,1]", {
  gs  <- make_gs2()
  vci <- visibility_concentration_index(gs)
  vals <- vci$vci[!is.na(vci$vci)]
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("community_fragmentation_index returns cfi column", {
  gs <- make_gs2()
  ct <- detect_communities_ts(gs)
  fi <- community_fragmentation_index(ct)
  expect_true("cfi" %in% names(fi))
})

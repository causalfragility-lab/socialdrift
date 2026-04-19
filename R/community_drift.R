#' Compute community drift between adjacent time periods
#'
#' Derives period-over-period changes in community structure metrics.
#' Acts as a first-order approximation of how communities are evolving.
#'
#' @param community_tbl Output of [detect_communities_ts()].
#'
#' @return The input tibble augmented with columns:
#'   \describe{
#'     \item{delta_n_communities}{Change in number of communities.}
#'     \item{delta_modularity}{Change in modularity.}
#'     \item{delta_largest_community}{Change in size of largest community.}
#'   }
#'
#' @examples
#' data(sim_social_events)
#' ev  <- as_social_events(sim_social_events)
#' gs  <- build_graph_series(ev, window = "month")
#' ct  <- detect_communities_ts(gs)
#' community_drift(ct)
#'
#' @export
community_drift <- function(community_tbl) {
  if (!is.data.frame(community_tbl)) {
    stop("`community_tbl` must be a data frame (output of `detect_communities_ts()`).",
         call. = FALSE)
  }

  community_tbl |>
    dplyr::arrange(.data$period) |>
    dplyr::mutate(
      delta_n_communities    = .data$n_communities - dplyr::lag(.data$n_communities),
      delta_modularity       = .data$modularity    - dplyr::lag(.data$modularity),
      delta_largest_community = .data$largest_community_size -
                                  dplyr::lag(.data$largest_community_size)
    )
}


#' Community Fragmentation Index (CFI)
#'
#' Computes a composite Community Fragmentation Index for each period,
#' combining modularity, number of communities, and singleton prevalence
#' into a single bounded score between 0 and 1. Higher values indicate a more
#' fragmented, siloed network.
#'
#' @details
#' CFI is computed as:
#' \deqn{CFI = 0.5 \cdot \hat{Q} + 0.3 \cdot \hat{C} + 0.2 \cdot \hat{S}}
#' where \eqn{\hat{Q}} is min-max scaled modularity, \eqn{\hat{C}} is
#' scaled community count, and \eqn{\hat{S}} is singleton proportion.
#'
#' @param community_tbl Output of [detect_communities_ts()].
#'
#' @return A tibble with columns `period` and `cfi`.
#'
#' @examples
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events)
#' gs <- build_graph_series(ev, window = "month")
#' ct <- detect_communities_ts(gs)
#' community_fragmentation_index(ct)
#'
#' @export
community_fragmentation_index <- function(community_tbl) {
  mm_scale <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(rep(0.5, length(x)))
    (x - rng[1]) / diff(rng)
  }

  tbl <- community_tbl |> dplyr::arrange(.data$period)

  tbl |>
    dplyr::mutate(
      q_scaled  = mm_scale(.data$modularity),
      c_scaled  = mm_scale(.data$n_communities),
      s_prop    = ifelse(
        !is.na(.data$n_communities) & .data$n_communities > 0,
        .data$singleton_count / .data$n_communities,
        NA_real_
      ),
      cfi = 0.5 * .data$q_scaled + 0.3 * .data$c_scaled + 0.2 * .data$s_prop
    ) |>
    dplyr::select("period", "cfi")
}

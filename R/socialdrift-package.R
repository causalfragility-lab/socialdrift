#' socialdrift: Temporal Auditing of Social Interaction Networks
#'
#' @description
#' \pkg{socialdrift} provides a full workflow for constructing, auditing, and
#' visualising temporal social interaction networks from raw event-log data.
#'
#' @details
#' The package is organised into five functional modules:
#'
#' \describe{
#'   \item{**Data engineering**}{
#'     [as_social_events()], [validate_social_events()],
#'     [build_graph_snapshot()], [build_graph_series()]
#'   }
#'   \item{**Structural metrics**}{
#'     [network_density_ts()], [reciprocity_ts()], [clustering_ts()],
#'     [degree_inequality_ts()], [summarize_network_series()]
#'   }
#'   \item{**Community dynamics**}{
#'     [detect_communities_ts()], [community_drift()],
#'     [community_fragmentation_index()]
#'   }
#'   \item{**Role trajectories**}{
#'     [classify_user_roles()], [role_trajectories()],
#'     [role_mobility_index()]
#'   }
#'   \item{**Drift & inequality auditing**}{
#'     [network_drift()], [network_drift_index()],
#'     [visibility_concentration_index()], [creator_concentration()],
#'     [engagement_gap()], [audit_group_disparities()]
#'   }
#'   \item{**Plotting**}{
#'     [plot_network_metrics()], [plot_network_drift()],
#'     [plot_role_trajectories()]
#'   }
#' }
#'
#' ## Signature indices
#'
#' - **NDI** (Network Drift Index): composite measure of structural change
#'   between periods ([network_drift()]).
#' - **CFI** (Community Fragmentation Index): degree to which communities
#'   have become siloed ([community_fragmentation_index()]).
#' - **VCI** (Visibility Concentration Index): how concentrated incoming
#'   attention is ([visibility_concentration_index()]).
#' - **RMI** (Role Mobility Index): how frequently users transition between
#'   structural roles ([role_mobility_index()]).
#'
#' ## Quick start
#'
#' ```r
#' library(socialdrift)
#'
#' data(sim_social_events)
#' ev <- as_social_events(sim_social_events,
#'                        actor_group  = "actor_group",
#'                        target_group = "target_group")
#'
#' gs <- build_graph_series(ev, window = "month")
#'
#' summarize_network_series(gs)
#' network_drift(gs)
#' role_trajectories(gs)
#' audit_group_disparities(ev, gs)
#' ```
#'
#' @seealso
#' - Package vignette: `vignette("socialdrift-intro")`
#'
#' @keywords internal
"_PACKAGE"

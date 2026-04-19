# socialdrift <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**socialdrift** is an R package for building, auditing, and visualising
**temporal social interaction networks** from raw event-log data.

Most existing tools start *after* the graph already exists. `socialdrift`
takes you from a table of raw platform events (follows, replies, mentions,
likes) all the way through to longitudinal diagnostics, community health
reports, and inequality audits.

---

## Installation

```r
# Install from GitHub (development version)
# install.packages("remotes")
```

---

## Workflow at a glance

```r
library(socialdrift)

# 1. Standardise event data
ev <- as_social_events(sim_social_events,
                       actor_group  = "actor_group",
                       target_group = "target_group")

# 2. Build monthly graph snapshots
gs <- build_graph_series(ev, window = "month")

# 3. Structural metrics
summarize_network_series(gs)

# 4. Network Drift Index
ndi <- network_drift(gs)
plot_network_drift(ndi)

# 5. Community dynamics
comm <- detect_communities_ts(gs)
community_fragmentation_index(comm)

# 6. User role trajectories
roles <- role_trajectories(gs)
plot_role_trajectories(roles)

# 7. Group disparity audit
audit_group_disparities(ev, gs)
```

---

## Signature indices

| Index | Function | Description |
|-------|----------|-------------|
| **NDI** | `network_drift()` | How much did overall network structure change? |
| **CFI** | `community_fragmentation_index()` | Are communities becoming more siloed? |
| **VCI** | `visibility_concentration_index()` | Is attention concentrating on fewer actors? |
| **RMI** | `role_mobility_index()` | How often do users transition between roles? |

---

## Structural user roles

Each node is assigned one of six interpretable roles per period:

| Role | Description |
|------|-------------|
| `isolated` | No connections in this period |
| `peripheral` | Low degree, not a bridge |
| `broadcaster` | High out-degree, low in-degree |
| `popular` | High in-degree, low out-degree |
| `core` | High in- and out-degree |
| `bridge` | High betweenness centrality |

---

## Example use cases

- **Online learning communities** --- Who becomes isolated? Are discussions clique-based?
- **Social media platforms** --- Is recommendation driving unequal visibility?
- **Workplace collaboration** --- Are teams siloing after a reorg?
- **Educational peer networks** --- Do minority students become more peripheral over time?

---

## Package modules

| Module | Key functions |
|--------|--------------|
| Data engineering | `as_social_events()`, `build_graph_series()` |
| Structural metrics | `network_density_ts()`, `reciprocity_ts()`, `clustering_ts()`, `degree_inequality_ts()` |
| Community dynamics | `detect_communities_ts()`, `community_drift()`, `community_fragmentation_index()` |
| Role trajectories | `classify_user_roles()`, `role_trajectories()`, `role_mobility_index()` |
| Drift & inequality | `network_drift()`, `visibility_concentration_index()`, `audit_group_disparities()` |
| Plotting | `plot_network_metrics()`, `plot_network_drift()`, `plot_role_trajectories()` |

---

## Citation

```bibtex
@Manual{socialdrift,
  title  = {socialdrift: Temporal Auditing of Social Interaction Networks},
  author = {Subir Hait},
  year   = {2025},
  note   = {R package version 0.1.0},
}
```

---

## License

GPL-3 (c) Subir Hait

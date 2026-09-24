# ============================================================================
# SCRIPT: PT_commute_comparison.R
# PURPOSE:
#   Compare public transport (PT) connectivity with commuting-centre assignments
#   of functional regions (EE: toimepiirkondade keskused) at Estonian locality
#   (EE: kant) level. For one typical weekday the script detects direct PT trips
#   between localities, measures service from each locality to its commuting
#   centre (trips, travel time, frequency, AM arrival / PM return), checks
#   whether PT serves the assigned centre, and writes tabular and spatial outputs.
#
# INPUTS (paths set in CONFIG below):
#   - GTFS feed (gtfs_folder): routes, trips, stops, stop_times, calendar,
#     calendar_dates
#   - Localities GeoPackage (toimepiirkonnad_gpkg): locality polygons
#     (EPSG:3301) with code (CODE), name (KANT_NI) and commuting centre
#     (keskus, keskus_CODE) from the functional regions dataset.
#     Optional columns, not in the public file: population (RAHVAAR),
#     employed residents (t66tajad_n) and % of them working in the centre
#     (osak_t66tajatest). Without them these columns are NA in the output.
#
# OUTPUTS (results/):
#   - pt_commute_comparison.csv / .gpkg   one row per locality: PT metrics,
#                                         match with commuting centre,
#                                         AM/PM round-trip feasibility
#   - locality_stop_service_rankings.csv  stops per locality ranked by services
#   - pt_connections_lines.gpkg           lines between representative stops of
#                                         every connected locality pair (trips
#                                         in both directions)
#   - pt_adjacent_links.gpkg              lines between representative stops of
#                                         neighbouring localities with the number
#                                         of direct services (layers
#                                         adjacent_links, non_adjacent_links)
#   - pt_validation_inputs.rds            inputs for validate_locality.R
#
# STEPS:
#   1)  Load GTFS and functional region data
#   2)  Keep services active on target_date
#   3)  Assign stops to localities (spatial join)
#   4)  Build ordered locality sequences per trip -> origin/destination pairs
#   5)  Trips per locality pair, top PT destination, departing trips per locality
#   5b) Trips from each locality to its centre: travel time, frequency,
#       AM peak arrivals (07-09)
#   5c) PM return trips from the centre (departing 15-19)
#   5d) Save inputs for the per-locality validation script
#   6)  Join with commuting data; match and AM/PM feasibility indicators
#   7)  Summary statistics (console), CSV/GPKG export,
#       connection lines and adjacent-locality links
#
# METHOD NOTES:
#   - A trip counts once per locality pair, however many stops it makes there.
#   - Stops with the same name within rep_stop_merge_distance_m are merged.
#   - Stops outside all locality polygons are assigned to the nearest locality
#     if within stop_snap_distance_m (50 m; e.g. harbour piers). Stops abroad
#     (international lines) are left out.
#   - Travel time to the centre is measured to the centre stop with the most
#     incoming services from the centre's localities (usually the bus station).
#   - On loop routes passing the origin stop twice, the last visit is used.
#   - Trips of 240 min or more are excluded in Steps 5b and 5c.
#   - Neighbouring localities: polygons within 1 m of each other.
#   - Lines are drawn between representative stops: the stop (group) of each
#     locality with the most services on the target date.
#
# USAGE:
#   Run from the project root (paths are relative), e.g.
#     Rscript scripts/PT_commute_comparison.R
#   Then optionally: Rscript scripts/validate_locality.R K372
#   Packages: tidyverse, sf, scales
#
# LAST UPDATED:
#   - 2026-09-24
# ============================================================================

library(tidyverse)
library(sf)

options(scipen = 999)

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------

gtfs_folder <- "data/GTFS/gtfs_2025_04/gtfs"
toimepiirkonnad_gpkg <- "data/localities/localities.gpkg"

# Use 2025-04-15 
target_date <- as.Date("2025-04-15")
# When ranking representative stops, merge same-name stops within this distance
rep_stop_merge_distance_m <- 250
# Stops just outside all locality polygons (e.g. harbour piers) are assigned
# to the nearest locality if within this distance; stops abroad stay unassigned
stop_snap_distance_m <- 50

dir.create("results", showWarnings = FALSE)

# ============================================================
# STEP 1: LOAD DATA
# ============================================================

read_gtfs <- function(name) {
  readr::read_csv(
    file.path(gtfs_folder, paste0(name, ".txt")),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )
}

cat("Loading GTFS data...\n")
routes     <- read_gtfs("routes")
trips      <- read_gtfs("trips")
stops      <- read_gtfs("stops")
stop_times <- read_gtfs("stop_times")
calendar   <- read_gtfs("calendar")
cal_dates  <- read_gtfs("calendar_dates")

cat("Loading toimepiirkonnad...\n")
kants <- st_read(toimepiirkonnad_gpkg, quiet = TRUE)

# Type conversions
stops <- stops %>%
  mutate(
    stop_lat = as.numeric(stop_lat),
    stop_lon = as.numeric(stop_lon)
  )

calendar <- calendar %>%
  mutate(
    start_date = as.Date(start_date, "%Y%m%d"),
    end_date   = as.Date(end_date, "%Y%m%d"),
    tuesday    = as.integer(tuesday)
  )

cal_dates <- cal_dates %>%
  mutate(
    date = as.Date(date, "%Y%m%d"),
    exception_type = as.integer(exception_type)
  )

stop_times <- stop_times %>%
  mutate(stop_sequence = as.integer(stop_sequence))

# ============================================================
# STEP 2: FILTER ACTIVE SERVICES FOR TARGET DATE
# ============================================================

cat("Filtering active services for", as.character(target_date), "...\n")

weekday_col <- tolower(weekdays(target_date, abbreviate = FALSE))
# Map English weekday to calendar column
weekday_map <- c(
  "monday" = "monday", "tuesday" = "tuesday", "wednesday" = "wednesday",
  "thursday" = "thursday", "friday" = "friday", "saturday" = "saturday",
  "sunday" = "sunday",
  # Estonian weekdays (if locale is Estonian)
  "esmaspäev" = "monday", "teisipäev" = "tuesday", "kolmapäev" = "wednesday",
  "neljapäev" = "thursday", "reede" = "friday", "laupäev" = "saturday",
  "pühapäev" = "sunday"
)
weekday_field <- weekday_map[weekday_col]

services_base <- calendar %>%
  filter(
    .data[[weekday_field]] == 1,
    start_date <= target_date,
    end_date >= target_date
  ) %>%
  pull(service_id)

services_added <- cal_dates %>%
  filter(date == target_date, exception_type == 1) %>%
  pull(service_id)

services_removed <- cal_dates %>%
  filter(date == target_date, exception_type == 2) %>%
  pull(service_id)

active_services <- union(
  setdiff(services_base, services_removed),
  services_added
)

trips_active <- trips %>%
  filter(service_id %in% active_services)

cat("  Active services:", length(active_services), "\n")
cat("  Active trips:", nrow(trips_active), "\n")

# ============================================================
# STEP 3: SPATIAL JOIN - STOPS → LOCALITIES
# ============================================================

cat("Performing spatial join: stops → localities...\n")

# Convert stops to sf
stops_sf <- stops %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Transform to Estonian CRS (EPSG:3301) to match localities
stops_sf <- st_transform(stops_sf, st_crs(kants))

# Spatial join: find which locality each stop falls into
stops_with_kant <- st_join(stops_sf, kants %>% select(CODE, KANT_NI, keskus_CODE, keskus), left = TRUE)

# Extract stop-locality mapping (drop geometry for efficiency)
# locality_name will be added later from kant_to_keskus to avoid column collision
stop_kant_map <- stops_with_kant %>%
  st_drop_geometry() %>%
  select(stop_id, kant_CODE = CODE)

# Assign unmatched stops to the nearest locality within stop_snap_distance_m
# (e.g. Heltermaa sadam lies 10 m outside the Suuremõisa polygon)
unmatched <- stops_sf %>% filter(stop_id %in% stop_kant_map$stop_id[is.na(stop_kant_map$kant_CODE)])
nearest_idx <- st_nearest_feature(unmatched, kants)
snapped <- tibble(
  stop_id = unmatched$stop_id,
  snap_CODE = kants$CODE[nearest_idx],
  snap_dist_m = as.numeric(st_distance(unmatched, kants[nearest_idx, ], by_element = TRUE))
) %>%
  filter(snap_dist_m <= stop_snap_distance_m)

stop_kant_map <- stop_kant_map %>%
  left_join(snapped %>% select(stop_id, snap_CODE), by = "stop_id") %>%
  mutate(kant_CODE = coalesce(kant_CODE, snap_CODE)) %>%
  select(-snap_CODE)

cat("  Stops assigned to nearest locality (within", stop_snap_distance_m, "m):", nrow(snapped), "\n")

cat("  Stops matched to localities:", sum(!is.na(stop_kant_map$kant_CODE)), "/", nrow(stop_kant_map), "\n")

# ============================================================
# STEP 4: BUILD ALL STOP PAIRS ALONG EACH TRIP
# ============================================================

cat("Building all stop pairs along each trip (this may take a moment)...\n")

# Get stop sequences for active trips, joined with locality codes
trip_stops <- stop_times %>%
  filter(trip_id %in% trips_active$trip_id) %>%
  select(trip_id, stop_id, stop_sequence) %>%
  left_join(stop_kant_map, by = "stop_id") %>%
  filter(!is.na(kant_CODE)) %>%
  arrange(trip_id, stop_sequence)

# Generate all origin-destination pairs within each trip
# For a trip with stops in localities A → B → C → D, this creates pairs:
# A→B, A→C, A→D, B→C, B→D, C→D
# We only count unique locality pairs per trip (not per stop pair)

trip_kant_pairs <- trip_stops %>%
  group_by(trip_id) %>%
  # Get consecutive distinct localities (preserves direction when route returns)
  # e.g., K270 K270 K394 K394 K270 → [K270, K394, K270]
  summarise(
    kant_sequence = list({
      r <- rle(kant_CODE)
      r$values
    }),
    .groups = "drop"
  ) %>%
  # Expand to all forward-direction locality pairs based on sequence position
  mutate(
    kant_pairs = map(kant_sequence, function(kants) {
      if (length(kants) < 2) return(tibble(origin_kant = character(), dest_kant = character()))
      # Generate all i < j pairs (based on position in consecutive sequence)
      expand_grid(i = seq_along(kants), j = seq_along(kants)) %>%
        filter(i < j) %>%
        mutate(
          origin_kant = kants[i],
          dest_kant = kants[j]
        ) %>%
        select(origin_kant, dest_kant)
    })
  ) %>%
  select(trip_id, kant_pairs) %>%
  unnest(kant_pairs)

cat("  Total locality-pair connections generated:", nrow(trip_kant_pairs), "\n")

# Summarize to unique trip-level connections (one connection per locality pair per trip)
trip_od <- trip_kant_pairs %>%
  distinct(trip_id, origin_kant, dest_kant)

cat("  Unique trip-locality-pair connections:", nrow(trip_od), "\n")

# ============================================================
# STEP 5: AGGREGATE PT SERVICES BY LOCALITY PAIR
# ============================================================

cat("Aggregating PT services by locality pair...\n")

# Count trips between locality pairs
# Each trip contributes one connection per locality pair it serves
kant_od_counts <- trip_od %>%
  count(origin_kant, dest_kant, name = "n_trips")

# For each origin locality, find the top destination
# (excluding trips where origin = destination, i.e., internal circulation)
# When there's a tie, prefer the centre (commuting center)

# Get centre lookup
keskus_lookup <- kants %>%
  st_drop_geometry() %>%
  select(origin_kant = CODE, keskus_CODE)

top_pt_destination <- kant_od_counts %>%
  filter(origin_kant != dest_kant) %>%
  # Join to know which destination is the centre for this origin
  left_join(keskus_lookup, by = "origin_kant") %>%
  mutate(is_keskus = (dest_kant == keskus_CODE)) %>%
  # Sort by n_trips DESC, then prefer centre in ties
  arrange(origin_kant, desc(n_trips), desc(is_keskus)) %>%
  group_by(origin_kant) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  rename(n_trips_to_top = n_trips) %>%
  select(kant_CODE = origin_kant, top_pt_dest = dest_kant, n_trips_to_top)

# Total outbound trips per locality: number of distinct services departing
# from the locality towards any other locality. Each trip counts once,
# regardless of how many localities it serves afterwards
# (connections per destination are in kant_od_counts).
total_outbound <- trip_od %>%
  filter(origin_kant != dest_kant) %>%
  group_by(origin_kant) %>%
  summarise(total_outbound_trips = n_distinct(trip_id), .groups = "drop") %>%
  rename(kant_CODE = origin_kant)

# ============================================================
# STEP 5b: SERVICE FREQUENCY & TRAVEL TIME TO CENTRE
# ============================================================

cat("Analyzing service frequency and travel time to keskus...\n")

# Get keskus_CODE and names for each locality (needed for filtering and labeling)
kant_to_keskus <- kants %>%
  st_drop_geometry() %>%
  select(kant_CODE = CODE, kant_name = KANT_NI, keskus_CODE, keskus_name = keskus)

# Helper function to parse GTFS time (handles times > 24:00)
parse_gtfs_time <- function(time_str) {
  if (is.na(time_str) || !nzchar(time_str)) return(NA_real_)
  parts <- strsplit(time_str, ":")[[1]]
  if (length(parts) != 3) return(NA_real_)
  as.numeric(parts[1]) * 60 + as.numeric(parts[2]) + as.numeric(parts[3]) / 60
}

# Get detailed stop times for active trips with locality codes
trip_stop_details <- stop_times %>%
  filter(trip_id %in% trips_active$trip_id) %>%
  select(trip_id, stop_id, stop_sequence, arrival_time, departure_time) %>%
  left_join(stop_kant_map, by = "stop_id") %>%
  filter(!is.na(kant_CODE)) %>%
  # Add stop names
  left_join(stops %>% select(stop_id, stop_name), by = "stop_id") %>%
  arrange(trip_id, stop_sequence)

# For each trip, find connections from each locality to its centre
# with departure and arrival times
# Using vectorized tidyverse approach instead of for loops

trip_stop_details <- trip_stop_details %>%
  left_join(kant_to_keskus, by = "kant_CODE")

# Cluster same-name nearby stops so service ranking combines split platforms/stops
cluster_stop_name_group <- function(df, threshold_m, target_crs) {
  n <- nrow(df)
  if (n == 0) return(df %>% mutate(stop_name_cluster = integer()))
  if (n == 1) return(df %>% mutate(stop_name_cluster = 1L))

  pts <- df %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    st_transform(target_crs)

  dist_mat <- matrix(as.numeric(st_distance(pts)), nrow = n, ncol = n)
  connected <- dist_mat <= threshold_m
  visited <- rep(FALSE, n)
  cluster <- integer(n)
  cid <- 0L

  for (i in seq_len(n)) {
    if (visited[i]) next
    cid <- cid + 1L
    queue <- i
    visited[i] <- TRUE
    cluster[i] <- cid

    while (length(queue) > 0) {
      v <- queue[1]
      queue <- queue[-1]
      nbrs <- which(connected[v, ] & !visited)
      if (length(nbrs) > 0) {
        visited[nbrs] <- TRUE
        cluster[nbrs] <- cid
        queue <- c(queue, nbrs)
      }
    }
  }

  df %>% mutate(stop_name_cluster = cluster)
}

stop_points_for_grouping <- trip_stop_details %>%
  distinct(kant_CODE, stop_id, stop_name) %>%
  left_join(stops %>% select(stop_id, stop_lat, stop_lon), by = "stop_id") %>%
  filter(!is.na(stop_lat), !is.na(stop_lon))

stop_group_lookup <- stop_points_for_grouping %>%
  group_by(kant_CODE, stop_name) %>%
  group_modify(~cluster_stop_name_group(.x, rep_stop_merge_distance_m, st_crs(kants))) %>%
  ungroup() %>%
  mutate(
    stop_group_id = paste(kant_CODE, stop_name, stop_name_cluster, sep = "__"),
    stop_group_name = stop_name
  ) %>%
  select(kant_CODE, stop_id, stop_name, stop_group_id, stop_group_name)

trip_stop_details <- trip_stop_details %>%
  left_join(
    stop_group_lookup %>%
      select(kant_CODE, stop_id, stop_group_id, stop_group_name),
    by = c("kant_CODE", "stop_id")
  ) %>%
  mutate(
    stop_group_id = coalesce(stop_group_id, paste(kant_CODE, stop_name, stop_id, sep = "__")),
    stop_group_name = coalesce(stop_group_name, stop_name)
  )

stop_group_sizes <- trip_stop_details %>%
  distinct(kant_CODE, stop_group_id, stop_id) %>%
  count(kant_CODE, stop_group_id, name = "n_physical_stops")

# Build stop list per locality (combined same-name nearby stops), ranked by active-day services
locality_stop_service_rank <- trip_stop_details %>%
  distinct(kant_CODE, stop_group_id, stop_group_name, trip_id) %>%
  count(kant_CODE, stop_group_id, stop_group_name, name = "n_services_stop_total") %>%
  left_join(stop_group_sizes, by = c("kant_CODE", "stop_group_id")) %>%
  arrange(kant_CODE, desc(n_services_stop_total), stop_group_name, stop_group_id) %>%
  group_by(kant_CODE) %>%
  mutate(
    stop_rank_total = row_number(),
    is_top_stop_total = stop_rank_total == 1L
  ) %>%
  ungroup()

# Self-join within each trip: pair each stop with all subsequent stops
# Then filter to keep only pairs where destination locality = origin's centre
keskus_stop_pairs <- trip_stop_details %>%
  # Rename for origin (include stop_id and stop_name)
  select(
    trip_id,
    origin_stop_id = stop_id,
    origin_stop_name = stop_name,
    origin_stop_group_id = stop_group_id,
    origin_stop_group_name = stop_group_name,
    origin_seq = stop_sequence,
    origin_kant = kant_CODE,
    origin_kant_name = kant_name,
    origin_keskus = keskus_CODE,
    keskus_name,
    departure_time
  ) %>%
  # Join with destination stops from same trip
  inner_join(
    trip_stop_details %>%
      select(
        trip_id,
        dest_stop_id = stop_id,
        dest_stop_name = stop_name,
        dest_stop_group_id = stop_group_id,
        dest_stop_group_name = stop_group_name,
        dest_seq = stop_sequence,
        dest_kant = kant_CODE,
        dest_kant_name = kant_name,
        arrival_time
      ),
    by = "trip_id",
    relationship = "many-to-many"
  ) %>%
  # Keep only forward connections (origin before destination)
  filter(dest_seq > origin_seq) %>%
  # Keep only connections where destination is the origin's centre
  filter(dest_kant == origin_keskus) %>%
  # Exclude trips where origin and destination are the same locality (locality is its own center)
  filter(origin_kant != dest_kant)

# Destination stops in each centre, ranked by the number of incoming services
# from its localities (any centre stop reached by a trip coming from one of them).
# Regional services mostly end at the bus/train station, so it ranks first.
eligible_dest_stop_rank <- keskus_stop_pairs %>%
  distinct(dest_kant, dest_stop_group_id, dest_stop_group_name, trip_id) %>%
  count(dest_kant, dest_stop_group_id, dest_stop_group_name, name = "n_services_from_localities") %>%
  arrange(dest_kant, desc(n_services_from_localities), dest_stop_group_name, dest_stop_group_id) %>%
  group_by(dest_kant) %>%
  mutate(
    stop_rank_dest_to_keskus = row_number(),
    is_representative_dest_to_keskus = stop_rank_dest_to_keskus == 1L
  ) %>%
  ungroup() %>%
  rename(
    kant_CODE = dest_kant,
    stop_group_id = dest_stop_group_id,
    stop_group_name = dest_stop_group_name
  )

# For each origin stop, keep the centre stop on the trip with the best
# incoming-service rank (stop_rank_dest_to_keskus), not the last centre stop.
# Ties (same stop group visited twice): first visit.
trips_to_keskus_candidates <- keskus_stop_pairs %>%
  left_join(
    eligible_dest_stop_rank %>%
      select(dest_kant = kant_CODE, dest_stop_group_id = stop_group_id,
             dest_stop_rank = stop_rank_dest_to_keskus),
    by = c("dest_kant", "dest_stop_group_id")
  ) %>%
  group_by(trip_id, origin_kant, origin_seq) %>%
  arrange(dest_stop_rank, dest_seq, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(trip_id,
         origin_stop_id, origin_stop_name, origin_stop_group_id, origin_stop_group_name,
         origin_seq, origin_kant, origin_kant_name,
         dest_stop_id, dest_stop_name, dest_stop_group_id, dest_stop_group_name,
         dest_seq, dest_kant, dest_kant_name, keskus_name,
         departure_time, arrival_time)

# Eligible origin stops: have at least one direct onward service to the designated centre
eligible_origin_stop_rank <- trips_to_keskus_candidates %>%
  distinct(origin_kant, origin_stop_group_id, origin_stop_group_name, trip_id) %>%
  count(origin_kant, origin_stop_group_id, origin_stop_group_name, name = "n_services_to_keskus") %>%
  arrange(origin_kant, desc(n_services_to_keskus), origin_stop_group_name, origin_stop_group_id) %>%
  group_by(origin_kant) %>%
  mutate(
    stop_rank_to_keskus = row_number(),
    is_representative_to_keskus = stop_rank_to_keskus == 1L
  ) %>%
  ungroup() %>%
  rename(
    kant_CODE = origin_kant,
    stop_group_id = origin_stop_group_id,
    stop_group_name = origin_stop_group_name
  )

representative_origin_stops <- eligible_origin_stop_rank %>%
  filter(is_representative_to_keskus) %>%
  transmute(
    origin_kant = kant_CODE,
    representative_origin_stop_group_id = stop_group_id,
    representative_origin_stop_name = stop_group_name,
    representative_origin_services_to_keskus = n_services_to_keskus
  )

# Export stop list and representative flags for all localities
# (origin side: rank to own centre; centre side: rank by incoming services)
locality_stop_rankings <- locality_stop_service_rank %>%
  left_join(
    eligible_origin_stop_rank %>%
      select(
        kant_CODE, stop_group_id,
        n_services_to_keskus,
        stop_rank_to_keskus,
        is_representative_to_keskus
      ),
    by = c("kant_CODE", "stop_group_id")
  ) %>%
  left_join(
    eligible_dest_stop_rank %>%
      select(
        kant_CODE, stop_group_id,
        n_services_from_localities,
        stop_rank_dest_to_keskus,
        is_representative_dest_to_keskus
      ),
    by = c("kant_CODE", "stop_group_id")
  ) %>%
  left_join(
    kant_to_keskus %>% select(kant_CODE, kant_name, keskus_CODE, keskus_name),
    by = "kant_CODE"
  ) %>%
  relocate(kant_CODE, kant_name, keskus_CODE, keskus_name,
           stop_group_id, stop_group_name) %>%
  arrange(kant_CODE, stop_rank_total)

stop_rankings_output <- "results/locality_stop_service_rankings.csv"
write_csv(locality_stop_rankings, stop_rankings_output)
cat("  Stop ranking list saved:", stop_rankings_output, "\n")
cat("  Localities with representative outbound stop:", nrow(representative_origin_stops), "\n")

# Keep all detected locality -> centre trip connections.
# For each trip+origin locality, choose representative origin/destination stops
# by stop-service rank among the eligible stops on that trip, then
# apply those stop/time fields without dropping any detected trip connections.
trip_keskus_rep_pair <- trips_to_keskus_candidates %>%
  left_join(
    eligible_origin_stop_rank %>%
      select(
        kant_CODE,
        stop_group_id,
        stop_rank_to_keskus
      ),
    by = c("origin_kant" = "kant_CODE", "origin_stop_group_id" = "stop_group_id")
  ) %>%
  left_join(
    eligible_dest_stop_rank %>%
      select(
        kant_CODE,
        stop_group_id,
        stop_rank_dest_to_keskus
      ),
    by = c("dest_kant" = "kant_CODE", "dest_stop_group_id" = "stop_group_id")
  ) %>%
  mutate(
    origin_rank_to_keskus = coalesce(stop_rank_to_keskus, .Machine$integer.max),
    dest_rank_to_keskus = coalesce(stop_rank_dest_to_keskus, .Machine$integer.max)
  ) %>%
  group_by(trip_id, origin_kant) %>%
  # If a (loop) trip passes the chosen origin stop more than once before the
  # centre, use the LAST visit (desc(origin_seq)): a passenger boards on the
  # way back, not before the bus runs its loop.
  arrange(origin_rank_to_keskus, dest_rank_to_keskus,
          desc(origin_seq), dest_seq, departure_time, arrival_time, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    trip_id,
    origin_kant,
    rep_origin_stop_id = origin_stop_id,
    rep_origin_stop_name = origin_stop_name,
    rep_origin_stop_group_id = origin_stop_group_id,
    rep_origin_stop_group_name = origin_stop_group_name,
    rep_dest_stop_id = dest_stop_id,
    rep_dest_stop_name = dest_stop_name,
    rep_dest_stop_group_id = dest_stop_group_id,
    rep_dest_stop_group_name = dest_stop_group_name,
    rep_departure_time = departure_time,
    rep_arrival_time = arrival_time
  )

trips_to_keskus_detailed <- trips_to_keskus_candidates %>%
  left_join(trip_keskus_rep_pair, by = c("trip_id", "origin_kant")) %>%
  mutate(
    origin_stop_id = coalesce(rep_origin_stop_id, origin_stop_id),
    origin_stop_name = coalesce(rep_origin_stop_name, origin_stop_name),
    origin_stop_group_id = coalesce(rep_origin_stop_group_id, origin_stop_group_id),
    origin_stop_group_name = coalesce(rep_origin_stop_group_name, origin_stop_group_name),
    dest_stop_id = coalesce(rep_dest_stop_id, dest_stop_id),
    dest_stop_name = coalesce(rep_dest_stop_name, dest_stop_name),
    dest_stop_group_id = coalesce(rep_dest_stop_group_id, dest_stop_group_id),
    dest_stop_group_name = coalesce(rep_dest_stop_group_name, dest_stop_group_name),
    departure_time = coalesce(rep_departure_time, departure_time),
    arrival_time = coalesce(rep_arrival_time, arrival_time)
  ) %>%
  select(-rep_origin_stop_id, -rep_origin_stop_name,
         -rep_origin_stop_group_id, -rep_origin_stop_group_name,
         -rep_dest_stop_id, -rep_dest_stop_name,
         -rep_dest_stop_group_id, -rep_dest_stop_group_name,
         -rep_departure_time, -rep_arrival_time) %>%
  # One row per trip and origin locality: a trip with several stops in the
  # origin locality otherwise appears once per stop (identical copies after
  # the representative pair is applied), inflating all Step 5b metrics.
  distinct(trip_id, origin_kant, .keep_all = TRUE)

# Parse times and calculate travel time
trips_to_keskus_detailed <- trips_to_keskus_detailed %>%
  filter(!is.na(departure_time), !is.na(arrival_time)) %>%
  rowwise() %>%
  mutate(
    dep_minutes = parse_gtfs_time(departure_time),
    arr_minutes = parse_gtfs_time(arrival_time),
    travel_time_min = arr_minutes - dep_minutes,
    departure_hour = floor(dep_minutes / 60) %% 24,
    arrival_hour = floor(arr_minutes / 60) %% 24
  ) %>%
  ungroup() %>%
  # Exclude unrealistic travel times (negative or > 4 hours)
  filter(travel_time_min > 0, travel_time_min < 240)

cat("  Trips with keskus connections (all preserved, representative stops selected):",
    nrow(trips_to_keskus_detailed), "\n")

# Aggregate service metrics per locality
# (locality names will come from kant_keskus when joined to comparison)
# Using MEDIAN travel time to reduce impact of edge-of-locality stops
service_to_keskus <- trips_to_keskus_detailed %>%
  group_by(origin_kant) %>%
  summarise(
    # Number of daily services to centre
    n_services_to_keskus = n(),

    # Travel time statistics (median as primary metric)
    median_travel_time_min = round(median(travel_time_min), 1),
    avg_travel_time_min = round(mean(travel_time_min), 1),
    min_travel_time_min = round(min(travel_time_min), 1),
    max_travel_time_min = round(max(travel_time_min), 1),

    # Service span
    first_departure_hour = min(departure_hour),
    last_departure_hour = max(departure_hour),
    service_span_hours = last_departure_hour - first_departure_hour,

    # Frequency: average gap between services (in minutes)
    avg_frequency_min = if (n() > 1) {
      dep_times <- sort(dep_minutes)
      round(mean(diff(dep_times)), 1)
    } else NA_real_,

    # Peak indicators:
    # AM peak to centre is based on ARRIVAL time at centre (07:00-09:00).
    # (PM return from the centre is measured in Step 5c: pm_return_services.)
    am_peak_services = sum(arrival_hour >= 7 & arrival_hour < 9),
    off_peak_services = n_services_to_keskus - am_peak_services,

    .groups = "drop"
  ) %>%
  rename(kant_CODE = origin_kant)

# Calculate hourly distribution evenness
hourly_distribution <- trips_to_keskus_detailed %>%
  count(origin_kant, departure_hour) %>%
  group_by(origin_kant) %>%
  summarise(
    hours_with_service = n(),
    cv_hourly = sd(n) / mean(n),  # Coefficient of variation
    .groups = "drop"
  ) %>%
  rename(kant_CODE = origin_kant)

service_to_keskus <- service_to_keskus %>%
  left_join(hourly_distribution, by = "kant_CODE")

cat("  Localities with keskus service metrics:", nrow(service_to_keskus), "\n")

# ============================================================
# STEP 5c: PM RETURN SERVICES (CENTRE → LOCALITY, 15:00–19:00)
# ============================================================
# Mirrors Step 5b but in the return direction: trips that depart
# FROM the commuting centre during the PM window and arrive at
# a non-centre locality whose designated centre is that origin.
# ============================================================

cat("Computing PM return services (centre → locality, 15:00–19:00)...\n")

pm_return_hours <- 15:18   # 15:00–19:00

# Origin side: stops located inside a centre locality, departing in PM window
origin_pm <- trip_stop_details %>%
  filter(kant_CODE == keskus_CODE) %>%          # stop is in a centre locality
  select(trip_id, origin_seq = stop_sequence,
         origin_kant = kant_CODE, departure_time) %>%
  rowwise() %>%
  mutate(
    dep_minutes = parse_gtfs_time(departure_time),
    dep_hour    = floor(dep_minutes / 60) %% 24
  ) %>%
  ungroup() %>%
  filter(dep_hour %in% pm_return_hours)

# Destination side: all stops from those same trips
dest_pm <- trip_stop_details %>%
  select(trip_id, dest_seq = stop_sequence,
         dest_kant = kant_CODE, dest_keskus = keskus_CODE, arrival_time) %>%
  rowwise() %>%
  mutate(arr_minutes = parse_gtfs_time(arrival_time)) %>%
  ungroup()

# Pair origin (centre, PM departure) with later stops where
# the origin IS the designated centre of the destination locality
pm_return_pairs <- origin_pm %>%
  inner_join(dest_pm, by = "trip_id", relationship = "many-to-many") %>%
  filter(
    dest_seq > origin_seq,         # forward direction
    dest_kant != origin_kant,      # different locality
    origin_kant == dest_keskus     # origin IS the centre for the destination
  ) %>%
  mutate(travel_time_min = arr_minutes - dep_minutes) %>%
  filter(travel_time_min > 0, travel_time_min < 240) %>%
  # Keep only first stop reached in each destination locality per trip/origin.
  # This preserves return service for farther localities on through-routes.
  group_by(trip_id, origin_kant, origin_seq, dest_kant) %>%
  slice_min(dest_seq, n = 1) %>%
  ungroup()

cat("  PM return trip-stop pairs:", nrow(pm_return_pairs), "\n")

# Aggregate to locality level
pm_return_services <- pm_return_pairs %>%
  group_by(kant_CODE = dest_kant) %>%
  summarise(
    # One service per trip/day for a locality (avoid multiple counts from multiple centre stops)
    pm_return_services     = n_distinct(trip_id),
    pm_return_hours_covered = n_distinct(dep_hour),
    .groups = "drop"
  )

cat("  Localities with PM return service:", nrow(pm_return_services), "\n")

# ============================================================
# STEP 5d: SAVE INPUTS FOR VALIDATION
# ============================================================
# Per-locality validation (outbound/inbound trip lists + QGIS layers) is in
# scripts/validate_locality.R, e.g.:
#   Rscript scripts/validate_locality.R K372
# It reads the objects saved here, so this script must be run first.

validation_inputs_rds <- "results/pt_validation_inputs.rds"
saveRDS(
  list(
    kants = kants,
    stops = stops,
    trips = trips,
    routes = routes,
    target_date = target_date,
    trip_stop_details = trip_stop_details,
    trips_to_keskus_candidates = trips_to_keskus_candidates,
    trips_to_keskus_detailed = trips_to_keskus_detailed
  ),
  validation_inputs_rds
)
cat("  Validation inputs saved:", validation_inputs_rds, "\n")

# ============================================================
# STEP 6: COMPARE WITH COMMUTING DATA
# ============================================================

cat("Comparing PT destinations with commuting centers...\n")

# Get keskus_CODE for each locality from original data.
# Commuting attributes are optional: the public localities file has only
# CODE, KANT_NI, keskus and keskus_CODE. Missing ones are set to NA.
commuting_cols <- c(population = "RAHVAAR", workers = "t66tajad_n",
                    share_to_center = "osak_t66tajatest")

kant_keskus <- kants %>%
  st_drop_geometry() %>%
  select(kant_CODE = CODE, kant_name = KANT_NI, keskus_CODE, keskus_name = keskus,
         any_of(commuting_cols))

missing_commuting_cols <- setdiff(names(commuting_cols), names(kant_keskus))
for (col in missing_commuting_cols) kant_keskus[[col]] <- NA_real_
if (length(missing_commuting_cols) > 0) {
  cat("  Commuting data not in localities file (set to NA):",
      paste(missing_commuting_cols, collapse = ", "), "\n")
}

# Calculate trips from each locality to its specific centre
trips_to_keskus <- kant_od_counts %>%
  filter(origin_kant != dest_kant) %>%
  # Join to get keskus_CODE for each origin locality
  left_join(
    kant_keskus %>% select(kant_CODE, keskus_CODE),
    by = c("origin_kant" = "kant_CODE")
  ) %>%
  # Keep only trips that go to the locality's centre
  filter(dest_kant == keskus_CODE) %>%
  group_by(origin_kant) %>%
  summarise(n_trips_to_keskus = sum(n_trips), .groups = "drop") %>%
  rename(kant_CODE = origin_kant)

# Join everything together
comparison <- kant_keskus %>%
  left_join(top_pt_destination,  by = "kant_CODE") %>%
  left_join(total_outbound,      by = "kant_CODE") %>%
  left_join(trips_to_keskus,     by = "kant_CODE") %>%
  left_join(service_to_keskus,   by = "kant_CODE") %>%
  left_join(pm_return_services,  by = "kant_CODE") %>%
  replace_na(list(pm_return_services = 0L, pm_return_hours_covered = 0L)) %>%
  mutate(
    # Base PT flags
    n_trips_to_keskus  = replace_na(n_trips_to_keskus, 0),
    prop_to_keskus     = ifelse(total_outbound_trips > 0,
                                n_trips_to_keskus / total_outbound_trips,
                                NA_real_),
    pt_matches_commute = (top_pt_dest == keskus_CODE),
    has_pt_service     = n_trips_to_keskus > 0,

    # ── AM / PM feasibility indicators ──────────────────────
    # has_am_service: at least one ARRIVAL at centre in 07:00–09:00
    has_am_service  = !is.na(am_peak_services) & am_peak_services > 0,
    # has_pm_return: at least one return from centre in 15:00–19:00
    has_pm_return   = pm_return_services > 0,
    # return_feasible: full round-trip by PT is possible on this day
    return_feasible = has_am_service & has_pm_return
  )

# ============================================================
# STEP 7: ANALYSIS OUTPUT
# ============================================================

cat("\n============================================================\n")
cat("ANALYSIS RESULTS\n")
cat("============================================================\n\n")

# Summary statistics
# Exclude localities that are centers themselves (locality_name == keskus_name) from match analysis
comparison <- comparison %>%
  mutate(is_center = (kant_name == keskus_name))

n_total <- nrow(comparison)
n_centers <- sum(comparison$is_center, na.rm = TRUE)
n_non_centers <- n_total - n_centers
n_with_pt <- sum(comparison$has_pt_service & !comparison$is_center, na.rm = TRUE)
n_matches <- sum(comparison$pt_matches_commute & !comparison$is_center, na.rm = TRUE)

cat("Total localities:", n_total, "\n")
cat("Localities that are centers themselves:", n_centers, "(excluded from match analysis)\n")
cat("Non-center localities with outbound PT service:", n_with_pt, "\n")
cat("Non-center localities where top PT destination = commuting center:", n_matches, "\n")
cat("Match rate (of non-center localities with PT):", round(100 * n_matches / n_with_pt, 1), "%\n")

# Proportion to centre statistics (excluding centers)
prop_stats <- comparison %>%
  filter(!is_center, has_pt_service) %>%
  summarise(
    mean_prop = mean(prop_to_keskus, na.rm = TRUE),
    median_prop = median(prop_to_keskus, na.rm = TRUE),
    min_prop = min(prop_to_keskus, na.rm = TRUE),
    max_prop = max(prop_to_keskus, na.rm = TRUE)
  )
cat("\nProportion of PT trips going to keskus (non-center localities with PT):\n")
cat("  Mean:", round(100 * prop_stats$mean_prop, 1), "%\n")
cat("  Median:", round(100 * prop_stats$median_prop, 1), "%\n")
cat("  Range:", round(100 * prop_stats$min_prop, 1), "% -", round(100 * prop_stats$max_prop, 1), "%\n")

# Service frequency and travel time statistics
freq_stats <- comparison %>%
  filter(!is_center, !is.na(avg_frequency_min)) %>%
  summarise(
    n_with_frequency = n(),
    mean_freq = mean(avg_frequency_min, na.rm = TRUE),
    median_freq = median(avg_frequency_min, na.rm = TRUE),
    mean_median_travel = mean(median_travel_time_min, na.rm = TRUE),
    median_median_travel = median(median_travel_time_min, na.rm = TRUE),
    mean_span = mean(service_span_hours, na.rm = TRUE),
    mean_hours_served = mean(hours_with_service, na.rm = TRUE)
  )

cat("\nService frequency to keskus (non-center localities with keskus service):\n")
cat("  Localities with frequency data:", freq_stats$n_with_frequency, "\n")
cat("  Avg gap between services: Mean", round(freq_stats$mean_freq, 0), "min, Median", round(freq_stats$median_freq, 0), "min\n")
cat("  Median travel time to keskus: Mean", round(freq_stats$mean_median_travel, 0), "min, Median", round(freq_stats$median_median_travel, 0), "min\n")
cat("  Service span: Mean", round(freq_stats$mean_span, 1), "hours\n")
cat("  Hours with service: Mean", round(freq_stats$mean_hours_served, 1), "distinct hours\n")

# Peak vs off-peak distribution
peak_stats <- comparison %>%
  filter(!is_center, !is.na(n_services_to_keskus), n_services_to_keskus > 0) %>%
  summarise(
    total_am_peak = sum(am_peak_services, na.rm = TRUE),
    total_off_peak = sum(off_peak_services, na.rm = TRUE),
    total = total_am_peak + total_off_peak
  )
cat("\nPeak hour distribution of keskus services:\n")
cat("  AM peak (arrival 7-9):", peak_stats$total_am_peak, "(", round(100 * peak_stats$total_am_peak / peak_stats$total, 1), "%)\n")
cat("  Rest of day:", peak_stats$total_off_peak, "(", round(100 * peak_stats$total_off_peak / peak_stats$total, 1), "%)\n")

# Breakdown by match status
cat("\n--- Breakdown ---\n")
comparison %>%
  mutate(
    status = case_when(
      is_center ~ "Is a center (excluded)",
      !has_pt_service ~ "No PT service",
      pt_matches_commute ~ "PT matches commute",
      TRUE ~ "PT differs from commute"
    )
  ) %>%
  count(status) %>%
  print()

# Examples of mismatches (where PT goes elsewhere than commuting center)
# Exclude localities that ARE the centre itself (locality_name == keskus_name)
cat("\n--- Example mismatches (PT ≠ commute center) ---\n")
mismatches <- comparison %>%
  filter(
    has_pt_service,
    !pt_matches_commute,
    kant_name != keskus_name  # Exclude centers themselves
  ) %>%
  # Join to get the name of top_pt_dest locality
  left_join(
    kants %>% st_drop_geometry() %>% select(CODE, top_pt_dest_name = KANT_NI),
    by = c("top_pt_dest" = "CODE")
  ) %>%
  arrange(desc(total_outbound_trips)) %>%
  head(10) %>%
  select(kant_name, keskus_name, top_pt_dest_name, n_trips_to_top, total_outbound_trips)
print(mismatches)

# ── AM / PM feasibility summary ─────────────────────────────
non_centre <- comparison %>% filter(!is_center)
non_centre_pt <- non_centre %>% filter(has_pt_service)

cat("\n--- AM / PM round-trip feasibility ---\n")
cat("Non-centre localities with any PT to centre:       ", nrow(non_centre_pt), "\n")
cat("  with AM outbound service (07–09):            ",
    sum(non_centre_pt$has_am_service, na.rm = TRUE), "\n")
cat("  with PM return service (15–19):              ",
    sum(non_centre_pt$has_pm_return, na.rm = TRUE), "\n")
cat("  with full round-trip (AM + PM feasible):     ",
    sum(non_centre_pt$return_feasible, na.rm = TRUE), "\n")

# Worker-weighted share (only if worker counts are in the localities file)
non_centre_pt_weighted <- non_centre_pt %>%
  filter(!is.na(workers), is.finite(workers), workers > 0)
if (nrow(non_centre_pt_weighted) > 0) {
  pct_feasible_wtd <- weighted.mean(non_centre_pt_weighted$return_feasible,
                                    non_centre_pt_weighted$workers) * 100
  cat(sprintf("  %% feasible (worker-weighted):    %.1f%%\n", pct_feasible_wtd))
}

# Save results
output_file <- "results/pt_commute_comparison.csv"
write_csv(comparison, output_file)
cat("\nResults saved to:", output_file, "\n")

# ============================================================
# OPTIONAL: Create spatial output for mapping
# ============================================================

# Rejoin geometry for visualization
comparison_sf <- kants %>%
  select(CODE) %>%
  left_join(comparison, by = c("CODE" = "kant_CODE"))

output_gpkg <- "results/pt_commute_comparison.gpkg"
st_write(comparison_sf, output_gpkg, delete_dsn = TRUE, quiet = TRUE)
cat("Spatial results saved to:", output_gpkg, "\n")

# ============================================================
# PT CONNECTION LINES BETWEEN LOCALITIES
# ============================================================

cat("\nCreating PT connection lines...\n")

# Line end points: the representative stop of each locality, i.e. the stop
# group with the most services on the target date (stop_rank_total == 1).
# For a group of nearby same-name stops the mean of their coordinates is used.
stop_coords <- tibble(
  stop_id = stops_sf$stop_id,
  stop_x = st_coordinates(stops_sf)[, 1],
  stop_y = st_coordinates(stops_sf)[, 2]
)

locality_points <- locality_stop_service_rank %>%
  filter(is_top_stop_total) %>%
  select(CODE = kant_CODE, stop_group_id, rep_stop_name = stop_group_name) %>%
  left_join(stop_group_lookup %>% select(CODE = kant_CODE, stop_group_id, stop_id),
            by = c("CODE", "stop_group_id")) %>%
  left_join(stop_coords, by = "stop_id") %>%
  group_by(CODE, rep_stop_name) %>%
  summarise(point_x = mean(stop_x), point_y = mean(stop_y), .groups = "drop")

cat("  Localities with a representative stop:", nrow(locality_points), "\n")

# Combine both directions into single locality pairs with bidirectional counts
pt_bidirectional <- kant_od_counts %>%
  filter(origin_kant != dest_kant) %>%
  # Create canonical pair key (alphabetically smaller code first)
  mutate(
    kant_a = pmin(origin_kant, dest_kant),
    kant_b = pmax(origin_kant, dest_kant),
    # Track direction: is this A→B or B→A?
    is_a_to_b = (origin_kant == kant_a)
  ) %>%
  group_by(kant_a, kant_b) %>%
  summarise(
    n_trips_total = sum(n_trips),
    n_trips_a_to_b = sum(n_trips[is_a_to_b]),
    n_trips_b_to_a = sum(n_trips[!is_a_to_b]),
    .groups = "drop"
  )

# Join representative stop points
pt_connections <- pt_bidirectional %>%
  # Join locality_a point
  left_join(
    locality_points %>% rename(x_a = point_x, y_a = point_y, stop_a = rep_stop_name),
    by = c("kant_a" = "CODE")
  ) %>%
  # Join locality_b point
  left_join(
    locality_points %>% rename(x_b = point_x, y_b = point_y, stop_b = rep_stop_name),
    by = c("kant_b" = "CODE")
  ) %>%
  filter(!is.na(x_a), !is.na(x_b))

# Create line geometries
pt_lines <- pt_connections %>%
  rowwise() %>%
  mutate(
    geometry = st_sfc(
      st_linestring(matrix(c(x_a, y_a, x_b, y_b), ncol = 2, byrow = TRUE)),
      crs = st_crs(kants)
    )
  ) %>%
  ungroup() %>%
  st_as_sf() %>%
  select(kant_a, kant_b, stop_a, stop_b,
         n_trips_total, n_trips_a_to_b, n_trips_b_to_a, geometry)

# Add locality names for labeling
pt_lines <- pt_lines %>%
  left_join(
    kants %>% st_drop_geometry() %>% select(CODE, name_a = KANT_NI),
    by = c("kant_a" = "CODE")
  ) %>%
  left_join(
    kants %>% st_drop_geometry() %>% select(CODE, name_b = KANT_NI),
    by = c("kant_b" = "CODE")
  )

# Export PT connection lines
# Note: Use n_trips_total field for line width styling in QGIS
output_lines_gpkg <- "results/pt_connections_lines.gpkg"
st_write(pt_lines, output_lines_gpkg, delete_dsn = TRUE, quiet = TRUE)

cat("PT connection lines saved to:", output_lines_gpkg, "\n")
cat("  Total connections:", nrow(pt_lines), "\n")
cat("  Use 'n_trips_total' field for line width styling in QGIS\n")

# Create a summary of top connections
cat("\n--- Top 20 PT connections by trip count (bidirectional) ---\n")
pt_lines %>%
  st_drop_geometry() %>%
  arrange(desc(n_trips_total)) %>%
  head(20) %>%
  select(name_a, name_b, n_trips_total, n_trips_a_to_b, n_trips_b_to_a) %>%
  print()

# ============================================================
# PT SERVICES BETWEEN ADJACENT LOCALITIES
# ============================================================
# One line per pair of neighbouring localities (representative stop to
# representative stop, as for the connection lines),
# carrying the number of services that travel directly from one to the
# other. Unlike pt_connections_lines, a trip A → B → C only counts on
# A–B and B–C, so line thickness shows the service pattern on the network.

cat("\nCreating PT links between adjacent localities...\n")

# Neighbours: polygons within 1 m of each other. A strict shared-edge test
# misses ~330 pairs whose polygons overlap slightly along the border.
adjacency_tolerance_m <- 1

neighbours <- st_is_within_distance(kants, kants, dist = adjacency_tolerance_m)
adjacent_pairs <- tibble(
  i = rep(seq_along(neighbours), lengths(neighbours)),
  j = unlist(neighbours)
) %>%
  filter(i != j) %>%
  transmute(kant_a = pmin(kants$CODE[i], kants$CODE[j]),
            kant_b = pmax(kants$CODE[i], kants$CODE[j])) %>%
  distinct() %>%
  mutate(is_adjacent = TRUE)

cat("  Adjacent locality pairs:", nrow(adjacent_pairs), "\n")

# Direct locality-to-locality moves along each trip: consecutive distinct
# localities in stop order (same run-length collapse as Step 4)
trip_transitions <- trip_stops %>%
  group_by(trip_id) %>%
  summarise(seq = list(rle(kant_CODE)$values), .groups = "drop") %>%
  filter(lengths(seq) >= 2) %>%
  mutate(moves = map(seq, ~tibble(from_kant = head(.x, -1), to_kant = tail(.x, -1)))) %>%
  select(trip_id, moves) %>%
  unnest(moves) %>%
  distinct(trip_id, from_kant, to_kant)

transition_counts <- trip_transitions %>%
  mutate(
    kant_a = pmin(from_kant, to_kant),
    kant_b = pmax(from_kant, to_kant),
    is_a_to_b = from_kant == kant_a
  ) %>%
  group_by(kant_a, kant_b) %>%
  summarise(
    n_services_a_to_b = n_distinct(trip_id[is_a_to_b]),
    n_services_b_to_a = n_distinct(trip_id[!is_a_to_b]),
    .groups = "drop"
  ) %>%
  mutate(n_services_total = n_services_a_to_b + n_services_b_to_a) %>%
  left_join(adjacent_pairs, by = c("kant_a", "kant_b")) %>%
  mutate(is_adjacent = coalesce(is_adjacent, FALSE))

# Moves between non-adjacent localities happen when a bus passes through
# a locality without stopping there (or stops fall outside the polygons)
cat("  Locality pairs with direct services:", nrow(transition_counts), "\n")
cat("    adjacent:    ", sum(transition_counts$is_adjacent),
    "(", sum(transition_counts$n_services_total[transition_counts$is_adjacent]), "services )\n")
cat("    non-adjacent:", sum(!transition_counts$is_adjacent),
    "(", sum(transition_counts$n_services_total[!transition_counts$is_adjacent]), "services )\n")

# Build stop-to-stop lines
make_stop_lines <- function(df) {
  df <- df %>%
    left_join(locality_points %>% rename(x_a = point_x, y_a = point_y, stop_a = rep_stop_name),
              by = c("kant_a" = "CODE")) %>%
    left_join(locality_points %>% rename(x_b = point_x, y_b = point_y, stop_b = rep_stop_name),
              by = c("kant_b" = "CODE")) %>%
    filter(!is.na(x_a), !is.na(x_b)) %>%
    left_join(kants %>% st_drop_geometry() %>% select(CODE, name_a = KANT_NI),
              by = c("kant_a" = "CODE")) %>%
    left_join(kants %>% st_drop_geometry() %>% select(CODE, name_b = KANT_NI),
              by = c("kant_b" = "CODE"))

  geom <- st_sfc(
    pmap(list(df$x_a, df$y_a, df$x_b, df$y_b),
         function(xa, ya, xb, yb) st_linestring(matrix(c(xa, ya, xb, yb), ncol = 2, byrow = TRUE))),
    crs = st_crs(kants)
  )

  df %>%
    select(kant_a, name_a, stop_a, kant_b, name_b, stop_b,
           n_services_total, n_services_a_to_b, n_services_b_to_a) %>%
    st_sf(geometry = geom)
}

adjacent_links <- make_stop_lines(transition_counts %>% filter(is_adjacent))
non_adjacent_links <- make_stop_lines(transition_counts %>% filter(!is_adjacent))

# Export: style adjacent_links by n_services_total in QGIS
output_adjacent_gpkg <- "results/pt_adjacent_links.gpkg"
if (file.exists(output_adjacent_gpkg)) file.remove(output_adjacent_gpkg)
st_write(adjacent_links, output_adjacent_gpkg, layer = "adjacent_links", quiet = TRUE)
st_write(non_adjacent_links, output_adjacent_gpkg, layer = "non_adjacent_links",
         append = TRUE, quiet = TRUE)

cat("Adjacent PT links saved to:", output_adjacent_gpkg, "\n")
cat("  Layers: adjacent_links (", nrow(adjacent_links), "), non_adjacent_links (",
    nrow(non_adjacent_links), ")\n")
cat("  Use 'n_services_total' field for line width styling in QGIS\n")

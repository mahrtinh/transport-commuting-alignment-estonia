# ============================================================================
# SCRIPT: PT_commute_comparison.R
# PURPOSE:
#   Compare public transport connectivity against functional region commuting-centre
#   assignments (EE: toimepiirkondade keskused) at Estonian locality level. The script detects
#   trip-based locality-to-locality connectivity, computes service/feasibility metrics,
#   writes tabular + spatial outputs, and creates two maps.
#
# INPUTS:
#   - GTFS folder: routes.txt, trips.txt, stops.txt, stop_times.txt,
#                  calendar.txt, calendar_dates.txt
#   - functional urban regions (EE: toimepiirkonnad) GeoPackage with locality polygons and commuting-centre layers
#
# MAIN OUTPUTS:
#   - output/pt_commute_comparison.csv
#   - output/pt_commute_comparison.gpkg
#   - output/pt_connections_lines.gpkg
#   - output/locality_stop_service_rankings.csv
#   - output/pt_connections_lines_map.png
#   - output/pt_commute_mismatch_map.png
#   - Optional validation outputs via run_validation("KXXX"):
#     output/validation_<KANT>.gpkg and output/validation_<KANT>_report.txt
#
# STEPS:
#   1) Load GTFS + functional region data and parse fields
#   2) Filter services active on target_date
#   3) Spatially assign stops to localities
#   4) Build trip-level origin/destination locality pairs (direction-preserving)
#   5) Aggregate locality-to-locality service counts and rankings
#   6) Build centre-focused metrics and AM/PM feasibility indicators
#   7) Export CSV/GPKG outputs and produce two maps:
#      - PT connection lines between localities
#      - PT vs commuting mismatch
#
# LAST UPDATED:
#   - 2026-04-09
# ============================================================================

library(tidyverse)
library(sf)

options(scipen = 999)

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------

gtfs_folder <- "data/GTFS/gtfs_2025_04/gtfs"
toimepiirkonnad_gpkg <- "data/Toimepiirkonnad/pendelr6nde_toimepiirkonnad_3.gpkg"

# Use 2025-04-15 
target_date <- as.Date("2025-04-15")
# When ranking representative stops, merge same-name stops within this distance
rep_stop_merge_distance_m <- 250

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
# STEP 3: SPATIAL JOIN - STOPS → KANTS
# ============================================================

cat("Performing spatial join: stops → kants...\n")

# Convert stops to sf
stops_sf <- stops %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Transform to Estonian CRS (EPSG:3301) to match kants
stops_sf <- st_transform(stops_sf, st_crs(kants))

# Spatial join: find which kant each stop falls into
stops_with_kant <- st_join(stops_sf, kants %>% select(CODE, KANT_NI, keskus_CODE, keskus), left = TRUE)

# Extract stop-kant mapping (drop geometry for efficiency)
# kant_name will be added later from kant_to_keskus to avoid column collision
stop_kant_map <- stops_with_kant %>%
  st_drop_geometry() %>%
  select(stop_id, kant_CODE = CODE)

cat("  Stops matched to kants:", sum(!is.na(stop_kant_map$kant_CODE)), "/", nrow(stop_kant_map), "\n")

# ============================================================
# STEP 4: BUILD ALL STOP PAIRS ALONG EACH TRIP
# ============================================================

cat("Building all stop pairs along each trip (this may take a moment)...\n")

# Get stop sequences for active trips, joined with kant codes
trip_stops <- stop_times %>%
  filter(trip_id %in% trips_active$trip_id) %>%
  select(trip_id, stop_id, stop_sequence) %>%
  left_join(stop_kant_map, by = "stop_id") %>%
  filter(!is.na(kant_CODE)) %>%
  arrange(trip_id, stop_sequence)

# Generate all origin-destination pairs within each trip
# For a trip with stops in kants A → B → C → D, this creates pairs:
# A→B, A→C, A→D, B→C, B→D, C→D
# We only count unique kant pairs per trip (not per stop pair)

trip_kant_pairs <- trip_stops %>%
  group_by(trip_id) %>%
  # Get consecutive distinct kants (preserves direction when route returns)
  # e.g., K270 K270 K394 K394 K270 → [K270, K394, K270]
  summarise(
    kant_sequence = list({
      r <- rle(kant_CODE)
      r$values
    }),
    .groups = "drop"
  ) %>%
  # Expand to all forward-direction kant pairs based on sequence position
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

cat("  Total kant-pair connections generated:", nrow(trip_kant_pairs), "\n")

# Summarize to unique trip-level connections (one connection per kant pair per trip)
trip_od <- trip_kant_pairs %>%
  distinct(trip_id, origin_kant, dest_kant)

cat("  Unique trip-kant-pair connections:", nrow(trip_od), "\n")

# ============================================================
# STEP 5: AGGREGATE PT SERVICES BY KANT PAIR
# ============================================================

cat("Aggregating PT services by kant pair...\n")

# Count trips between kant pairs
# Each trip contributes one connection per kant pair it serves
kant_od_counts <- trip_od %>%
  count(origin_kant, dest_kant, name = "n_trips")

# For each origin kant, find the top destination
# (excluding trips where origin = destination, i.e., internal circulation)
# When there's a tie, prefer the keskus (commuting center)

# Get keskus lookup
keskus_lookup <- kants %>%
  st_drop_geometry() %>%
  select(origin_kant = CODE, keskus_CODE)

top_pt_destination <- kant_od_counts %>%
  filter(origin_kant != dest_kant) %>%
  # Join to know which destination is the keskus for this origin
  left_join(keskus_lookup, by = "origin_kant") %>%
  mutate(is_keskus = (dest_kant == keskus_CODE)) %>%
  # Sort by n_trips DESC, then prefer keskus in ties
  arrange(origin_kant, desc(n_trips), desc(is_keskus)) %>%
  group_by(origin_kant) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  rename(n_trips_to_top = n_trips) %>%
  select(kant_CODE = origin_kant, top_pt_dest = dest_kant, n_trips_to_top)

# Also calculate total outbound trips per kant
total_outbound <- kant_od_counts %>%
  filter(origin_kant != dest_kant) %>%
  group_by(origin_kant) %>%
  summarise(total_outbound_trips = sum(n_trips), .groups = "drop") %>%
  rename(kant_CODE = origin_kant)

# ============================================================
# STEP 5b: SERVICE FREQUENCY & TRAVEL TIME TO KESKUS
# ============================================================

cat("Analyzing service frequency and travel time to keskus...\n")

# Get keskus_CODE and names for each kant (needed for filtering and labeling)
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

# Get detailed stop times for active trips with kant codes
trip_stop_details <- stop_times %>%
  filter(trip_id %in% trips_active$trip_id) %>%
  select(trip_id, stop_id, stop_sequence, arrival_time, departure_time) %>%
  left_join(stop_kant_map, by = "stop_id") %>%
  filter(!is.na(kant_CODE)) %>%
  # Add stop names
  left_join(stops %>% select(stop_id, stop_name), by = "stop_id") %>%
  arrange(trip_id, stop_sequence)

# For each trip, find connections from each kant to its keskus
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
# Then filter to keep only pairs where destination kant = origin's keskus
trips_to_keskus_candidates <- trip_stop_details %>%
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
  # Keep only connections where destination is the origin's keskus
  filter(dest_kant == origin_keskus) %>%
  # Exclude trips where origin and destination are the same kant (kant is its own center)
  filter(origin_kant != dest_kant) %>%
  # Keep only the last stop in keskus for each origin stop (represents final destination)
  group_by(trip_id, origin_kant, origin_seq) %>%
  slice_max(dest_seq, n = 1) %>%
  ungroup() %>%
  select(trip_id,
         origin_stop_id, origin_stop_name, origin_stop_group_id, origin_stop_group_name,
         origin_seq, origin_kant, origin_kant_name,
         dest_stop_id, dest_stop_name, dest_stop_group_id, dest_stop_group_name,
         dest_seq, dest_kant, dest_kant_name, keskus_name,
         departure_time, arrival_time)

# Eligible origin stops: have at least one direct onward service to the designated keskus
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

# Eligible destination stops in each keskus, ranked by number of incoming services
eligible_dest_stop_rank <- trips_to_keskus_candidates %>%
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

# Export stop list and representative flags for all localities
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
    kant_to_keskus %>% select(kant_CODE, kant_name, keskus_CODE, keskus_name),
    by = "kant_CODE"
  ) %>%
  relocate(kant_CODE, kant_name, keskus_CODE, keskus_name,
           stop_group_id, stop_group_name) %>%
  arrange(kant_CODE, stop_rank_total)

stop_rankings_output <- "output/locality_stop_service_rankings.csv"
write_csv(locality_stop_rankings, stop_rankings_output)
cat("  Stop ranking list saved:", stop_rankings_output, "\n")
cat("  Localities with representative outbound stop:", nrow(representative_origin_stops), "\n")

# Keep all detected locality -> keskus trip connections.
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
  arrange(origin_rank_to_keskus, dest_rank_to_keskus,
          origin_seq, dest_seq, departure_time, arrival_time, .by_group = TRUE) %>%
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
         -rep_departure_time, -rep_arrival_time)

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

# Aggregate service metrics per kant
# (kant names will come from kant_keskus when joined to comparison)
# Using MEDIAN travel time to reduce impact of edge-of-kant stops
service_to_keskus <- trips_to_keskus_detailed %>%
  group_by(origin_kant) %>%
  summarise(
    # Number of daily services to keskus
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
    am_peak_services = sum(arrival_hour >= 7 & arrival_hour < 9),
    # PM indicator kept as departure-time based towards centre (16:00-18:00).
    pm_peak_services = sum(departure_hour >= 16 & departure_hour < 18),
    off_peak_services = n_services_to_keskus - am_peak_services - pm_peak_services,

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

cat("  Kants with keskus service metrics:", nrow(service_to_keskus), "\n")

# ============================================================
# STEP 5c: PM RETURN SERVICES (CENTRE → LOCALITY, 15:00–19:00)
# ============================================================
# Mirrors Step 5b but in the return direction: trips that depart
# FROM the commuting centre during the PM window and arrive at
# a non-centre locality whose designated centre is that origin.
# ============================================================

cat("Computing PM return services (centre → locality, 15:00–19:00)...\n")

pm_return_hours <- 15:18   # 15:00–19:00

# Origin side: stops located inside a centre kant, departing in PM window
origin_pm <- trip_stop_details %>%
  filter(kant_CODE == keskus_CODE) %>%          # stop is in a centre kant
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
  # Keep only first stop reached in each destination kant per trip/origin.
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
# STEP 5d: VALIDATION — outbound & inbound trips for one locality
# ============================================================
#
# CONFIG: set the locality kant code to inspect.
# The commuting centre is derived automatically.
#
# Time windows (edit here to change thresholds):
#   AM outbound:  07:00–09:00  (arrival at centre)
#   PM return:    15:00–19:00  (departure from centre)
#
# QGIS output layers:
#   outbound_trips   – one line per outbound service, with time-window flag
#   inbound_trips    – one line per inbound/return service, with time-window flag
#   outbound_stops   – stops used in outbound trips
#   inbound_stops    – stops used in inbound trips
#   kants            – locality + centre polygons with summary counts
# ============================================================

run_validation <- function(validate_kant = "K270",
                           am_window_start = 7,
                           am_window_end = 8,
                           pm_window_start = 15,
                           pm_window_end = 18,
                           output_dir = "output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Derive centre automatically
validate_centre <- kants %>%
  st_drop_geometry() %>%
  filter(CODE == validate_kant) %>%
  pull(keskus_CODE) %>%
  first()

validate_kant_name   <- kants %>% st_drop_geometry() %>%
  filter(CODE == validate_kant)   %>% pull(KANT_NI) %>% first()
validate_centre_name <- kants %>% st_drop_geometry() %>%
  filter(CODE == validate_centre) %>% pull(KANT_NI) %>% first()

cat("\n── Validation: ", validate_kant_name, "(", validate_kant, ")",
    "→ centre:", validate_centre_name, "(", validate_centre, ") ──\n")

# Eligible stop pairs for validation: locality -> centre (all times)
outbound_validation_candidates <- trips_to_keskus_candidates %>%
  filter(origin_kant == validate_kant, dest_kant == validate_centre) %>%
  filter(!is.na(departure_time), !is.na(arrival_time)) %>%
  rowwise() %>%
  mutate(
    dep_minutes = parse_gtfs_time(departure_time),
    arr_minutes = parse_gtfs_time(arrival_time),
    travel_time_min = arr_minutes - dep_minutes
  ) %>%
  ungroup() %>%
  filter(travel_time_min > 0, travel_time_min < 240)

# Eligible stop pairs for validation: centre -> locality (all times)
inbound_validation_candidates <- trip_stop_details %>%
  select(
    trip_id,
    origin_seq = stop_sequence,
    origin_stop_id = stop_id,
    origin_stop_name = stop_name,
    origin_stop_group_id = stop_group_id,
    origin_stop_group_name = stop_group_name,
    origin_kant = kant_CODE,
    departure_time
  ) %>%
  inner_join(
    trip_stop_details %>%
      select(
        trip_id,
        dest_seq = stop_sequence,
        dest_stop_id = stop_id,
        dest_stop_name = stop_name,
        dest_stop_group_id = stop_group_id,
        dest_stop_group_name = stop_group_name,
        dest_kant = kant_CODE,
        arrival_time
      ),
    by = "trip_id",
    relationship = "many-to-many"
  ) %>%
  filter(dest_seq > origin_seq,
         origin_kant == validate_centre,
         dest_kant == validate_kant) %>%
  filter(!is.na(departure_time), !is.na(arrival_time)) %>%
  rowwise() %>%
  mutate(
    dep_minutes = parse_gtfs_time(departure_time),
    arr_minutes = parse_gtfs_time(arrival_time),
    travel_time_min = arr_minutes - dep_minutes
  ) %>%
  ungroup() %>%
  filter(travel_time_min > 0, travel_time_min < 240)

# Build one representative stop per locality (validate_kant + validate_centre)
# based on highest service count among stops eligible in this locality-centre pair
validation_stop_rank <- bind_rows(
  outbound_validation_candidates %>%
    transmute(kant_CODE = validate_kant,
              stop_group_id = origin_stop_group_id,
              stop_group_name = origin_stop_group_name,
              trip_id),
  inbound_validation_candidates %>%
    transmute(kant_CODE = validate_kant,
              stop_group_id = dest_stop_group_id,
              stop_group_name = dest_stop_group_name,
              trip_id),
  outbound_validation_candidates %>%
    transmute(kant_CODE = validate_centre,
              stop_group_id = dest_stop_group_id,
              stop_group_name = dest_stop_group_name,
              trip_id),
  inbound_validation_candidates %>%
    transmute(kant_CODE = validate_centre,
              stop_group_id = origin_stop_group_id,
              stop_group_name = origin_stop_group_name,
              trip_id)
) %>%
  distinct(kant_CODE, stop_group_id, stop_group_name, trip_id) %>%
  count(kant_CODE, stop_group_id, stop_group_name, name = "n_services_pair") %>%
  arrange(kant_CODE, desc(n_services_pair), stop_group_name, stop_group_id) %>%
  group_by(kant_CODE) %>%
  mutate(
    stop_rank_pair = row_number(),
    is_representative_pair = stop_rank_pair == 1L
  ) %>%
  ungroup()

validate_locality_rep_stop_group_id <- validation_stop_rank %>%
  filter(kant_CODE == validate_kant, is_representative_pair) %>%
  pull(stop_group_id) %>%
  first()
validate_locality_rep_stop_name <- validation_stop_rank %>%
  filter(kant_CODE == validate_kant, is_representative_pair) %>%
  pull(stop_group_name) %>%
  first()
validate_locality_rep_services <- validation_stop_rank %>%
  filter(kant_CODE == validate_kant, is_representative_pair) %>%
  pull(n_services_pair) %>%
  first()

validate_centre_rep_stop_group_id <- validation_stop_rank %>%
  filter(kant_CODE == validate_centre, is_representative_pair) %>%
  pull(stop_group_id) %>%
  first()
validate_centre_rep_stop_name <- validation_stop_rank %>%
  filter(kant_CODE == validate_centre, is_representative_pair) %>%
  pull(stop_group_name) %>%
  first()
validate_centre_rep_services <- validation_stop_rank %>%
  filter(kant_CODE == validate_centre, is_representative_pair) %>%
  pull(n_services_pair) %>%
  first()

cat("  Representative stop (locality): ",
    coalesce(validate_locality_rep_stop_name, "N/A"),
    " [services: ", coalesce(validate_locality_rep_services, 0L), "]\n", sep = "")
cat("  Representative stop (centre):   ",
    coalesce(validate_centre_rep_stop_name, "N/A"),
    " [services: ", coalesce(validate_centre_rep_services, 0L), "]\n", sep = "")

validation_stop_rank_lookup <- validation_stop_rank %>%
  select(kant_CODE, stop_group_id, stop_rank_pair)

# Stop coordinates (needed for line geometry)
trip_stops_with_coords <- trip_stop_details %>%
  left_join(stops %>% select(stop_id, stop_lat, stop_lon), by = "stop_id")

# ------------------------------------------------------------
# Helper: build a stop-sequence polyline for one trip
# from_kant departs first, to_kant is the destination
# ------------------------------------------------------------
build_segment_directed <- function(tid, trip_data, from_kant, to_kant,
                                   stop_rank_lookup = NULL) {
  s <- trip_data %>% filter(trip_id == tid) %>% arrange(stop_sequence)
  from_rows <- which(s$kant_CODE == from_kant)
  to_rows   <- which(s$kant_CODE == to_kant)
  if (!length(from_rows) || !length(to_rows)) return(NULL)

  candidate_pairs <- expand_grid(from_row = from_rows, to_row = to_rows) %>%
    filter(to_row > from_row)
  if (nrow(candidate_pairs) == 0) return(NULL)

  if (!is.null(stop_rank_lookup)) {
    from_rank_tbl <- stop_rank_lookup %>%
      filter(kant_CODE == from_kant) %>%
      select(stop_group_id, stop_rank_pair)
    to_rank_tbl <- stop_rank_lookup %>%
      filter(kant_CODE == to_kant) %>%
      select(stop_group_id, stop_rank_pair)

    candidate_pairs <- candidate_pairs %>%
      mutate(
        from_stop_group_id = s$stop_group_id[from_row],
        to_stop_group_id = s$stop_group_id[to_row]
      ) %>%
      left_join(from_rank_tbl, by = c("from_stop_group_id" = "stop_group_id")) %>%
      rename(from_rank = stop_rank_pair) %>%
      left_join(to_rank_tbl, by = c("to_stop_group_id" = "stop_group_id")) %>%
      rename(to_rank = stop_rank_pair) %>%
      mutate(
        from_rank = coalesce(from_rank, .Machine$integer.max),
        to_rank = coalesce(to_rank, .Machine$integer.max)
      ) %>%
      arrange(from_rank, to_rank, from_row, to_row)
  } else {
    candidate_pairs <- candidate_pairs %>%
      arrange(from_row, to_row)
  }

  from_row <- candidate_pairs$from_row[1]
  to_row   <- candidate_pairs$to_row[1]

  seg    <- s[from_row:to_row, ]
  coords <- cbind(seg$stop_lon, seg$stop_lat)
  if (nrow(coords) < 2 || anyNA(coords)) return(NULL)

  tibble(
    trip_id    = tid,
    dep_stop   = seg$stop_name[1],
    arr_stop   = seg$stop_name[nrow(seg)],
    dep_time   = seg$departure_time[1],
    arr_time   = seg$arrival_time[nrow(seg)],
    n_stops    = nrow(seg),
    geometry   = st_sfc(st_linestring(coords), crs = 4326)
  )
}

# ------------------------------------------------------------
# OUTBOUND  (locality → centre, all times)
# ------------------------------------------------------------
outbound_trip_ids <- trips_to_keskus_detailed %>%
  filter(origin_kant == validate_kant) %>%
  pull(trip_id) %>%
  unique()

cat("  Outbound trip IDs found:", length(outbound_trip_ids), "\n")

outbound_segs <- map_dfr(
  outbound_trip_ids,
  ~build_segment_directed(
    .x, trip_stops_with_coords, validate_kant, validate_centre,
    stop_rank_lookup = validation_stop_rank_lookup
  )
)

  if (nrow(outbound_segs) > 0) {
  outbound_sf <- outbound_segs %>%
    st_as_sf() %>%
    st_transform(st_crs(kants)) %>%
    mutate(distance_km = round(as.numeric(st_length(geometry)) / 1000, 2)) %>%
    left_join(trips %>% select(trip_id, trip_headsign, route_id), by = "trip_id") %>%
    left_join(routes %>% select(route_id,
                                route_number = route_short_name,
                                route_name   = route_long_name), by = "route_id") %>%
    rowwise() %>%
    mutate(
      dep_minutes  = parse_gtfs_time(dep_time),
      arr_minutes  = parse_gtfs_time(arr_time),
      dep_hour     = floor(dep_minutes / 60) %% 24,
      arr_hour     = floor(arr_minutes / 60) %% 24,
      travel_time_min = round(parse_gtfs_time(arr_time) - dep_minutes, 1)
    ) %>%
    ungroup() %>%
    mutate(
      in_am_window = arr_hour >= am_window_start & arr_hour <= am_window_end,
      direction    = "outbound"
    ) %>%
    select(trip_id, route_number, route_name, headsign = trip_headsign,
           direction, dep_stop, arr_stop, dep_time, arr_time,
           dep_hour, arr_hour, in_am_window, travel_time_min, distance_km, n_stops, geometry)

  cat("  Outbound trips with geometry:  ", nrow(outbound_sf), "\n")
  cat("    in AM window (", am_window_start, ":00–",
      am_window_end + 1, ":00): ",
      sum(outbound_sf$in_am_window), "\n", sep = "")
} else {
  cat("  No outbound trip geometries found.\n")
  outbound_sf <- NULL
}

# ------------------------------------------------------------
# INBOUND  (centre → locality, all times)
# ------------------------------------------------------------
# All departures from the centre kant
origin_inbound <- trip_stop_details %>%
  filter(kant_CODE == validate_centre) %>%
  select(trip_id, origin_seq = stop_sequence, departure_time) %>%
  rowwise() %>%
  mutate(dep_minutes = parse_gtfs_time(departure_time),
         dep_hour    = floor(dep_minutes / 60) %% 24) %>%
  ungroup()

# Later stops in the validate_kant from the same trips
dest_inbound <- trip_stop_details %>%
  filter(kant_CODE == validate_kant) %>%
  select(trip_id, dest_seq = stop_sequence, arrival_time) %>%
  rowwise() %>%
  mutate(arr_minutes = parse_gtfs_time(arrival_time)) %>%
  ungroup()

inbound_pairs_val <- origin_inbound %>%
  inner_join(dest_inbound, by = "trip_id", relationship = "many-to-many") %>%
  filter(dest_seq > origin_seq) %>%
  mutate(travel_time_min = arr_minutes - dep_minutes) %>%
  filter(travel_time_min > 0, travel_time_min < 240) %>%
  group_by(trip_id, origin_seq) %>%
  slice_min(dest_seq, n = 1) %>%
  ungroup()

inbound_trip_ids <- unique(inbound_pairs_val$trip_id)
cat("  Inbound trip IDs found:", length(inbound_trip_ids), "\n")

inbound_segs <- map_dfr(
  inbound_trip_ids,
  ~build_segment_directed(
    .x, trip_stops_with_coords, validate_centre, validate_kant,
    stop_rank_lookup = validation_stop_rank_lookup
  )
)

if (nrow(inbound_segs) > 0) {
  inbound_sf <- inbound_segs %>%
    st_as_sf() %>%
    st_transform(st_crs(kants)) %>%
    mutate(distance_km = round(as.numeric(st_length(geometry)) / 1000, 2)) %>%
    left_join(trips %>% select(trip_id, trip_headsign, route_id), by = "trip_id") %>%
    left_join(routes %>% select(route_id,
                                route_number = route_short_name,
                                route_name   = route_long_name), by = "route_id") %>%
    rowwise() %>%
    mutate(
      dep_minutes     = parse_gtfs_time(dep_time),
      dep_hour        = floor(dep_minutes / 60) %% 24,
      travel_time_min = round(parse_gtfs_time(arr_time) - dep_minutes, 1)
    ) %>%
    ungroup() %>%
    mutate(
      in_pm_window = dep_hour >= pm_window_start & dep_hour <= pm_window_end,
      direction    = "inbound"
    ) %>%
    select(trip_id, route_number, route_name, headsign = trip_headsign,
           direction, dep_stop, arr_stop, dep_time, arr_time,
           dep_hour, in_pm_window, travel_time_min, distance_km, n_stops, geometry)

  cat("  Inbound trips with geometry:   ", nrow(inbound_sf), "\n")
  cat("    in PM window (", pm_window_start, ":00–",
      pm_window_end + 1, ":00): ",
      sum(inbound_sf$in_pm_window), "\n", sep = "")
} else {
  cat("  No inbound trip geometries found.\n")
  inbound_sf <- NULL
}

# ------------------------------------------------------------
# STOP POINTS
# ------------------------------------------------------------
collect_stops <- function(trip_ids, kant_codes) {
  trip_stops_with_coords %>%
    filter(trip_id %in% trip_ids, kant_CODE %in% kant_codes) %>%
    distinct(stop_id, stop_name, kant_CODE, stop_lat, stop_lon) %>%
    filter(!is.na(stop_lat)) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    st_transform(st_crs(kants))
}

outbound_stops_sf <- collect_stops(outbound_trip_ids,
                                   c(validate_kant, validate_centre))
inbound_stops_sf  <- collect_stops(inbound_trip_ids,
                                   c(validate_kant, validate_centre))

# ------------------------------------------------------------
# KANT POLYGONS with summary counts
# ------------------------------------------------------------
validation_kants <- kants %>%
  filter(CODE %in% c(validate_kant, validate_centre)) %>%
  select(CODE, KANT_NI, keskus_CODE) %>%
  mutate(
    role             = if_else(CODE == validate_kant, "locality", "centre"),
    n_outbound       = if_else(CODE == validate_kant,
                               as.integer(length(outbound_trip_ids)), NA_integer_),
    n_outbound_am    = if_else(CODE == validate_kant & !is.null(outbound_sf),
                               as.integer(sum(outbound_sf$in_am_window)), NA_integer_),
    n_inbound        = if_else(CODE == validate_kant,
                               as.integer(length(inbound_trip_ids)), NA_integer_),
    n_inbound_pm     = if_else(CODE == validate_kant & !is.null(inbound_sf),
                               as.integer(sum(inbound_sf$in_pm_window)), NA_integer_),
    return_feasible  = if_else(CODE == validate_kant,
                               n_outbound_am > 0 & n_inbound_pm > 0, NA)
  )

# ------------------------------------------------------------
# EXPORT GEOPACKAGE
# ------------------------------------------------------------
output_gpkg <- file.path(output_dir, paste0("validation_", validate_kant, ".gpkg"))
if (file.exists(output_gpkg)) file.remove(output_gpkg)

st_write(validation_kants,  output_gpkg, layer = "kants",          quiet = TRUE)
if (!is.null(outbound_sf))
  st_write(outbound_sf,     output_gpkg, layer = "outbound_trips",  append = TRUE, quiet = TRUE)
if (!is.null(inbound_sf))
  st_write(inbound_sf,      output_gpkg, layer = "inbound_trips",   append = TRUE, quiet = TRUE)
st_write(outbound_stops_sf, output_gpkg, layer = "outbound_stops",  append = TRUE, quiet = TRUE)
st_write(inbound_stops_sf,  output_gpkg, layer = "inbound_stops",   append = TRUE, quiet = TRUE)

cat("\n  Exported to:", output_gpkg, "\n")
cat("    kants:          ", nrow(validation_kants),  "(locality + centre)\n")
cat("    outbound_trips: ", if (!is.null(outbound_sf)) nrow(outbound_sf) else 0, "\n")
cat("    inbound_trips:  ", if (!is.null(inbound_sf))  nrow(inbound_sf)  else 0, "\n")
cat("    outbound_stops: ", nrow(outbound_stops_sf), "\n")
cat("    inbound_stops:  ", nrow(inbound_stops_sf),  "\n")
cat("\n  Key QGIS fields:\n")
cat("    outbound_trips.in_am_window  — TRUE if arrival at centre within AM window\n")
cat("    inbound_trips.in_pm_window   — TRUE if departure within PM window\n")
cat("    outbound_trips.dep_hour/arr_hour — departure/arrival hour (0–23)\n")
cat("    inbound_trips.dep_hour        — departure hour (0–23)\n")

# ------------------------------------------------------------
# VALIDATION REPORT (text file)
# ------------------------------------------------------------
output_report <- file.path(output_dir, paste0("validation_", validate_kant, "_report.txt"))

n_out        <- if (!is.null(outbound_sf)) nrow(outbound_sf)      else 0L
n_out_am     <- if (!is.null(outbound_sf)) sum(outbound_sf$in_am_window) else 0L
n_in         <- if (!is.null(inbound_sf))  nrow(inbound_sf)       else 0L
n_in_pm      <- if (!is.null(inbound_sf))  sum(inbound_sf$in_pm_window)  else 0L
feasible_str <- if (n_out_am > 0 & n_in_pm > 0) "YES" else "NO"

fmt_rep_stop <- function(stop_name, stop_group_id, n_services) {
  if (is.na(stop_group_id)) return("N/A")
  sprintf("%s (%d services)", stop_name, as.integer(coalesce(n_services, 0L)))
}

rep_locality <- fmt_rep_stop(validate_locality_rep_stop_name,
                             validate_locality_rep_stop_group_id,
                             validate_locality_rep_services)
rep_centre   <- fmt_rep_stop(validate_centre_rep_stop_name,
                             validate_centre_rep_stop_group_id,
                             validate_centre_rep_services)

write_report <- function(path) {
  sep  <- paste(rep("=", 72), collapse = "")
  sep2 <- paste(rep("-", 72), collapse = "")
  lns  <- c(
    sep,
    sprintf("VALIDATION REPORT  –  %s  (%s)", validate_kant_name, validate_kant),
    sprintf("Centre:  %s  (%s)", validate_centre_name, validate_centre),
    sprintf("Date:    %s", as.character(target_date)),
    sep,
    "",
    "REPRESENTATIVE STOPS (highest service count among eligible stops)",
    sep2,
    sprintf("  Locality stop:  %s", rep_locality),
    sprintf("  Centre stop:    %s", rep_centre),
    "",
    "SUMMARY",
    sep2,
    sprintf("  Outbound trips (locality → centre):  %d", n_out),
    sprintf("    in AM window (%d:00–%d:00):           %d",
            am_window_start, am_window_end + 1, n_out_am),
    sprintf("  Inbound trips  (centre → locality):  %d", n_in),
    sprintf("    in PM window (%d:00–%d:00):           %d",
            pm_window_start, pm_window_end + 1, n_in_pm),
    "",
    sprintf("  Round-trip feasible (AM + PM both exist):  %s", feasible_str),
    ""
  )

  # ── OUTBOUND TABLE ─────────────────────────────────────────
  lns <- c(lns, "",
           sprintf("OUTBOUND TRIPS  (from %s  →  %s)",
                   validate_kant_name, validate_centre_name),
           sprintf("  [*] = arrival within AM window (%d:00–%d:00)",
                   am_window_start, am_window_end + 1),
           sep2)

  if (!is.null(outbound_sf) && nrow(outbound_sf) > 0) {
    header <- sprintf("%-4s %-8s %-8s %-9s %-8s  %-28s  %-28s  %s",
                      "Win", "Dep", "Arr", "Travel", "Dist",
                      "Dep stop", "Arr stop", "Route / Headsign")
    lns <- c(lns, header, paste(rep("-", nchar(header)), collapse = ""))
    rows <- outbound_sf %>%
      st_drop_geometry() %>%
      arrange(dep_time) %>%
      mutate(
        win   = if_else(in_am_window, "[*]", "   "),
        rl    = if_else(!is.na(route_number) & route_number != "",
                        paste0(route_number, " – ", coalesce(route_name, "")),
                        coalesce(route_name, trip_id)),
        route_info = if_else(!is.na(headsign) & headsign != "",
                             paste0(rl, "  /  ", headsign), rl)
      )
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      lns <- c(lns, sprintf("%-4s %-8s %-8s %4.0f min  %5.1f km  %-28s  %-28s  %s",
                             r$win, r$dep_time, r$arr_time,
                             r$travel_time_min, r$distance_km,
                             substr(r$dep_stop, 1, 28), substr(r$arr_stop, 1, 28),
                             substr(r$route_info, 1, 60)))
    }
  } else {
    lns <- c(lns, "  (no outbound trips found)")
  }

  # ── INBOUND TABLE ──────────────────────────────────────────
  lns <- c(lns, "",
           sprintf("INBOUND TRIPS  (from %s  →  %s)",
                   validate_centre_name, validate_kant_name),
           sprintf("  [*] = departure within PM window (%d:00–%d:00)",
                   pm_window_start, pm_window_end + 1),
           sep2)

  if (!is.null(inbound_sf) && nrow(inbound_sf) > 0) {
    header <- sprintf("%-4s %-8s %-8s %-9s %-8s  %-28s  %-28s  %s",
                      "Win", "Dep", "Arr", "Travel", "Dist",
                      "Dep stop", "Arr stop", "Route / Headsign")
    lns <- c(lns, header, paste(rep("-", nchar(header)), collapse = ""))
    rows <- inbound_sf %>%
      st_drop_geometry() %>%
      arrange(dep_time) %>%
      mutate(
        win   = if_else(in_pm_window, "[*]", "   "),
        rl    = if_else(!is.na(route_number) & route_number != "",
                        paste0(route_number, " – ", coalesce(route_name, "")),
                        coalesce(route_name, trip_id)),
        route_info = if_else(!is.na(headsign) & headsign != "",
                             paste0(rl, "  /  ", headsign), rl)
      )
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      lns <- c(lns, sprintf("%-4s %-8s %-8s %4.0f min  %5.1f km  %-28s  %-28s  %s",
                             r$win, r$dep_time, r$arr_time,
                             r$travel_time_min, r$distance_km,
                             substr(r$dep_stop, 1, 28), substr(r$arr_stop, 1, 28),
                             substr(r$route_info, 1, 60)))
    }
  } else {
    lns <- c(lns, "  (no inbound trips found)")
  }

  lns <- c(lns, "", sep, "")
  writeLines(lns, path)
}

write_report(output_report)
cat("\n  Report saved to:", output_report, "\n")

invisible(list(
  validate_kant = validate_kant,
  validate_centre = validate_centre,
  output_gpkg = output_gpkg,
  output_report = output_report,
  outbound_trip_count = length(outbound_trip_ids),
  inbound_trip_count = length(inbound_trip_ids),
  outbound_am_count = n_out_am,
  inbound_pm_count = n_in_pm,
  representative_locality_stop = validate_locality_rep_stop_name,
  representative_centre_stop = validate_centre_rep_stop_name
))
}

# Run validation on demand, e.g.:
run_validation("K486")

# ============================================================
# STEP 6: COMPARE WITH COMMUTING DATA
# ============================================================

cat("Comparing PT destinations with commuting centers...\n")

# Get keskus_CODE for each kant from original data
kant_keskus <- kants %>%
  st_drop_geometry() %>%
  select(kant_CODE = CODE, kant_name = KANT_NI, keskus_CODE, keskus_name = keskus,
         population = RAHVAAR, workers = t66tajad_n,
         share_to_center = osak_t66tajatest)

# Calculate trips from each kant to its specific keskus
trips_to_keskus <- kant_od_counts %>%
  filter(origin_kant != dest_kant) %>%
  # Join to get keskus_CODE for each origin kant
  left_join(
    kant_keskus %>% select(kant_CODE, keskus_CODE),
    by = c("origin_kant" = "kant_CODE")
  ) %>%
  # Keep only trips that go to the kant's keskus
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
# Exclude kants that ARE centers themselves (kant_name == keskus_name) from match analysis
comparison <- comparison %>%
  mutate(
    is_center = (kant_name == keskus_name),

    # ── Demand–supply gap indicators (non-centre kants only) ─
    # share_to_center: proportion of workers commuting to this centre (0–1)
    # A positive gap means commuting demand that PT does not serve.

    # Binary gap: commuting share minus return feasibility (0 or 1)
    demand_supply_gap = if_else(
      !is_center,
      share_to_center - as.numeric(return_feasible),
      NA_real_
    ),

    # Continuous gap: commuting share minus normalised hours-of-service
    # Normalise hours_with_service to [0,1] relative to the dataset maximum
    hours_supply_norm = if_else(
      has_pt_service,
      hours_with_service / max(hours_with_service, na.rm = TRUE),
      0
    ),
    hours_demand_gap = if_else(
      !is_center,
      share_to_center - hours_supply_norm,
      NA_real_
    )
  )

n_total <- nrow(comparison)
n_centers <- sum(comparison$is_center, na.rm = TRUE)
n_non_centers <- n_total - n_centers
n_with_pt <- sum(comparison$has_pt_service & !comparison$is_center, na.rm = TRUE)
n_matches <- sum(comparison$pt_matches_commute & !comparison$is_center, na.rm = TRUE)

cat("Total kants:", n_total, "\n")
cat("Kants that are centers themselves:", n_centers, "(excluded from match analysis)\n")
cat("Non-center kants with outbound PT service:", n_with_pt, "\n")
cat("Non-center kants where top PT destination = commuting center:", n_matches, "\n")
cat("Match rate (of non-center kants with PT):", round(100 * n_matches / n_with_pt, 1), "%\n")

# Proportion to keskus statistics (excluding centers)
prop_stats <- comparison %>%
  filter(!is_center, has_pt_service) %>%
  summarise(
    mean_prop = mean(prop_to_keskus, na.rm = TRUE),
    median_prop = median(prop_to_keskus, na.rm = TRUE),
    min_prop = min(prop_to_keskus, na.rm = TRUE),
    max_prop = max(prop_to_keskus, na.rm = TRUE)
  )
cat("\nProportion of PT trips going to keskus (non-center kants with PT):\n")
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

cat("\nService frequency to keskus (non-center kants with keskus service):\n")
cat("  Kants with frequency data:", freq_stats$n_with_frequency, "\n")
cat("  Avg gap between services: Mean", round(freq_stats$mean_freq, 0), "min, Median", round(freq_stats$median_freq, 0), "min\n")
cat("  Median travel time to keskus: Mean", round(freq_stats$mean_median_travel, 0), "min, Median", round(freq_stats$median_median_travel, 0), "min\n")
cat("  Service span: Mean", round(freq_stats$mean_span, 1), "hours\n")
cat("  Hours with service: Mean", round(freq_stats$mean_hours_served, 1), "distinct hours\n")

# Peak vs off-peak distribution
peak_stats <- comparison %>%
  filter(!is_center, !is.na(n_services_to_keskus), n_services_to_keskus > 0) %>%
  summarise(
    total_am_peak = sum(am_peak_services, na.rm = TRUE),
    total_pm_peak = sum(pm_peak_services, na.rm = TRUE),
    total_off_peak = sum(off_peak_services, na.rm = TRUE),
    total = total_am_peak + total_pm_peak + total_off_peak
  )
cat("\nPeak hour distribution of keskus services:\n")
cat("  AM peak (7-9):", peak_stats$total_am_peak, "(", round(100 * peak_stats$total_am_peak / peak_stats$total, 1), "%)\n")
cat("  PM peak (16-18):", peak_stats$total_pm_peak, "(", round(100 * peak_stats$total_pm_peak / peak_stats$total, 1), "%)\n")
cat("  Off-peak:", peak_stats$total_off_peak, "(", round(100 * peak_stats$total_off_peak / peak_stats$total, 1), "%)\n")

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
# Exclude kants that ARE the keskus itself (kant_name == keskus_name)
cat("\n--- Example mismatches (PT ≠ commute center) ---\n")
mismatches <- comparison %>%
  filter(
    has_pt_service,
    !pt_matches_commute,
    kant_name != keskus_name  # Exclude centers themselves
  ) %>%
  # Join to get the name of top_pt_dest kant
  left_join(
    kants %>% st_drop_geometry() %>% select(CODE, top_pt_dest_name = KANT_NI),
    by = c("top_pt_dest" = "CODE")
  ) %>%
  arrange(desc(total_outbound_trips)) %>%
  head(10) %>%
  select(kant_name, keskus_name, top_pt_dest_name, n_trips_to_top, total_outbound_trips)
print(mismatches)

# ── AM / PM feasibility & demand–supply summary ─────────────
non_centre <- comparison %>% filter(!is_center)
non_centre_pt <- non_centre %>% filter(has_pt_service)

cat("\n--- AM / PM round-trip feasibility ---\n")
cat("Non-centre kants with any PT to centre:       ", nrow(non_centre_pt), "\n")
cat("  with AM outbound service (07–09):            ",
    sum(non_centre_pt$has_am_service, na.rm = TRUE), "\n")
cat("  with PM return service (15–19):              ",
    sum(non_centre_pt$has_pm_return, na.rm = TRUE), "\n")
cat("  with full round-trip (AM + PM feasible):     ",
    sum(non_centre_pt$return_feasible, na.rm = TRUE), "\n")

cat("\n--- Population-weighted demand–supply gap ---\n")
non_centre_pt_weighted <- non_centre_pt %>%
  filter(!is.na(workers), is.finite(workers), workers > 0)
pop_summary <- non_centre_pt %>%
  summarise(
    pct_return_feasible = weighted.mean(non_centre_pt_weighted$return_feasible,
                                        non_centre_pt_weighted$workers) * 100,
    mean_demand_gap_wtd = weighted.mean(non_centre_pt_weighted$demand_supply_gap,
                                        non_centre_pt_weighted$workers)
  )
cat(sprintf("  %% feasible (worker-weighted):    %.1f%%\n", pop_summary$pct_return_feasible))
cat(sprintf("  Mean demand–supply gap (wtd):    %.3f\n",  pop_summary$mean_demand_gap_wtd))

# Save results
output_file <- "output/pt_commute_comparison.csv"
write_csv(comparison, output_file)
cat("\nResults saved to:", output_file, "\n")

# ============================================================
# OPTIONAL: Create spatial output for mapping
# ============================================================

# Rejoin geometry for visualization
comparison_sf <- kants %>%
  select(CODE) %>%
  left_join(comparison, by = c("CODE" = "kant_CODE"))

output_gpkg <- "output/pt_commute_comparison.gpkg"
st_write(comparison_sf, output_gpkg, delete_dsn = TRUE, quiet = TRUE)
cat("Spatial results saved to:", output_gpkg, "\n")

# ============================================================
# PT CONNECTION LINES BETWEEN KANTS
# ============================================================

cat("\nCreating PT connection lines...\n")

# Get kant centroids
kant_centroids <- kants %>%
  st_centroid() %>%
  select(CODE) %>%
  mutate(
    centroid_x = st_coordinates(.)[, 1],
    centroid_y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Combine both directions into single kant pairs with bidirectional counts
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

# Join centroids
pt_connections <- pt_bidirectional %>%
  # Join kant_a centroid
  left_join(
    kant_centroids %>% rename(x_a = centroid_x, y_a = centroid_y),
    by = c("kant_a" = "CODE")
  ) %>%
  # Join kant_b centroid
  left_join(
    kant_centroids %>% rename(x_b = centroid_x, y_b = centroid_y),
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
  select(kant_a, kant_b, n_trips_total, n_trips_a_to_b, n_trips_b_to_a, geometry)

# Add kant names for labeling
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
output_lines_gpkg <- "output/pt_connections_lines.gpkg"
st_write(pt_lines, output_lines_gpkg, delete_dsn = TRUE, quiet = TRUE)

cat("PT connection lines saved to:", output_lines_gpkg, "\n")
cat("  Total connections:", nrow(pt_lines), "\n")
cat("  Use 'n_trips_total' field for line width styling in QGIS\n")

# Also create a summary of top connections
cat("\n--- Top 20 PT connections by trip count (bidirectional) ---\n")
pt_lines %>%
  st_drop_geometry() %>%
  arrange(desc(n_trips_total)) %>%
  head(20) %>%
  select(name_a, name_b, n_trips_total, n_trips_a_to_b, n_trips_b_to_a) %>%
  print()

# ============================================================
# MAP: PT CONNECTION LINES BETWEEN KANTS
# ============================================================

cat("\nCreating PT connection lines map...\n")

pt_lines_map <- pt_lines %>%
  mutate(
    line_width = scales::rescale(log1p(n_trips_total), to = c(0.15, 1.2))
  )

p_connections <- ggplot() +
  geom_sf(data = kants, fill = "#f5f7fa", color = "#d0d6dd", linewidth = 0.1) +
  geom_sf(
    data = pt_lines_map,
    aes(color = n_trips_total, linewidth = line_width),
    alpha = 0.8
  ) +
  scale_linewidth_identity() +
  scale_color_viridis_c(name = "Trips/day", trans = "log1p", option = "C") +
  coord_sf() +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "#e7ebef", linewidth = 0.2),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  labs(
    title = "PT Connection Lines Between Kants",
    subtitle = "Line width and color intensity indicate total daily trips (both directions)",
    caption = paste("Date:", target_date, "| Source: Maanteeamet GTFS")
  )

print(p_connections)

output_lines_map <- "output/pt_connections_lines_map.png"
ggsave(output_lines_map, p_connections, width = 14, height = 10, dpi = 150)
cat("PT connection lines map saved to:", output_lines_map, "\n")

# ============================================================
# MAP: PT vs COMMUTING MISMATCH
# ============================================================

cat("\nCreating PT vs commuting mismatch map...\n")

# Prepare kant polygons with match status
kants_match_status <- kants %>%
  left_join(
    comparison %>% select(kant_CODE, keskus_CODE, top_pt_dest, pt_matches_commute,
                          has_pt_service, is_center, n_trips_to_top),
    by = c("CODE" = "kant_CODE")
  ) %>%
  mutate(
    match_status = case_when(
      is_center ~ "Center",
      !has_pt_service ~ "No PT service",
      pt_matches_commute ~ "PT matches commute",
      TRUE ~ "Mismatch"
    )
  )

# Count mismatch kants
mismatch_kants <- comparison %>%
  filter(!is_center, has_pt_service, !pt_matches_commute)

cat("  Mismatch kants:", nrow(mismatch_kants), "\n")

# Color palette
match_colors <- c(
  "Center" = "#4a4a6a",
  "No PT service" = "#2a2a3a",
  "PT matches commute" = "#1a472a",
  "Mismatch" = "#5c1a1a"
)

# Create the mismatch map
p_mismatch <- ggplot() +
  # Background: kant polygons colored by match status
  geom_sf(
    data = kants_match_status,
    aes(fill = match_status),
    color = "#3d3d5c",
    linewidth = 0.1
  ) +

  scale_fill_manual(values = match_colors, name = "Status") +

  # Keep aspect ratio
  coord_sf() +

  # Dark theme
  theme_void(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0d0d1a", color = NA),
    panel.background = element_rect(fill = "#0d0d1a", color = NA),
    plot.title = element_text(face = "bold", color = "#ffffff", size = 14),
    plot.subtitle = element_text(color = "#aaaaaa", size = 10),
    plot.caption = element_text(color = "#666666", size = 8),
    legend.background = element_rect(fill = "#0d0d1a", color = NA),
    legend.text = element_text(color = "#cccccc"),
    legend.title = element_text(color = "#ffffff"),
    legend.position = "right",
    plot.margin = margin(15, 15, 15, 15)
  ) +

  labs(
    title = "Mismatch: PT Destinations vs Commuting Patterns",
    subtitle = "Fill colors show PT alignment status against commuting centres",
    caption = paste("Mismatched kants:", nrow(mismatch_kants),
                    "| Target date:", target_date, "| Source: Maanteeamet GTFS")
  )

print(p_mismatch)

# Save the mismatch map
output_mismatch_map <- "output/pt_commute_mismatch_map.png"
ggsave(output_mismatch_map, p_mismatch, width = 14, height = 10, dpi = 150)
cat("Mismatch map saved to:", output_mismatch_map, "\n")

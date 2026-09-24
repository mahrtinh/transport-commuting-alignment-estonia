#!/usr/bin/env Rscript
# ============================================================================
# SCRIPT: validate_locality.R
# PURPOSE:
#   Validate PT connections between one locality and its commuting centre:
#   lists every outbound (locality → centre) and inbound (centre → locality)
#   trip on the target date, with AM/PM window flags, for checking in QGIS.
#
# REQUIRES:
#   Run PT_commute_comparison.R first. It saves the intermediate objects
#   used here to results/pt_validation_inputs.rds.
#
# USAGE (from the project root):
#   Rscript scripts/validate_locality.R
#     -> asks for a locality name (e.g. Lüllemäe) or code (e.g. K372),
#        repeatedly until an empty answer. Partial names work; if several
#        localities match (duplicate village names), you choose from a list.
#   Rscript scripts/validate_locality.R K372 Pihtla     # no prompt
#   In RStudio: source the script and answer the prompt in the console.
#
# OUTPUTS (per locality):
#   results/validation_<LOCALITY>.gpkg
#   results/validation_<LOCALITY>_report.txt
# ============================================================================

library(tidyverse)
library(sf)

options(scipen = 999)

# ------------------------------------------------------------
# LOAD INPUTS SAVED BY PT_commute_comparison.R
# ------------------------------------------------------------

validation_inputs_rds <- "results/pt_validation_inputs.rds"
if (!file.exists(validation_inputs_rds)) {
  stop("Missing ", validation_inputs_rds, " - run scripts/PT_commute_comparison.R first.")
}
validation_inputs <- readRDS(validation_inputs_rds)
list2env(validation_inputs, envir = environment())

# Helper function to parse GTFS time (handles times > 24:00)
parse_gtfs_time <- function(time_str) {
  if (is.na(time_str) || !nzchar(time_str)) return(NA_real_)
  parts <- strsplit(time_str, ":")[[1]]
  if (length(parts) != 3) return(NA_real_)
  as.numeric(parts[1]) * 60 + as.numeric(parts[2]) + as.numeric(parts[3]) / 60
}

# ============================================================
# VALIDATION — outbound & inbound trips for one locality
# ============================================================
#
# CONFIG: set the locality locality code to inspect.
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
#   localities            – locality + centre polygons with summary counts
# ============================================================

run_validation <- function(validate_kant = "K270",
                           am_window_start = 7,
                           am_window_end = 8,
                           pm_window_start = 15,
                           pm_window_end = 18,
                           output_dir = "results") {
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

# Build one representative stop per locality (validate_locality + validate_centre)
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
# from_locality departs first, to_locality is the destination
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
      arrange(from_rank, to_rank, desc(from_row), to_row)  # last visit of from-stop (loops)
  } else {
    candidate_pairs <- candidate_pairs %>%
      arrange(desc(from_row), to_row)  # last visit of from-stop (loops)
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
# All departures from the centre locality
origin_inbound <- trip_stop_details %>%
  filter(kant_CODE == validate_centre) %>%
  select(trip_id, origin_seq = stop_sequence, departure_time) %>%
  rowwise() %>%
  mutate(dep_minutes = parse_gtfs_time(departure_time),
         dep_hour    = floor(dep_minutes / 60) %% 24) %>%
  ungroup()

# Later stops in the validate_locality from the same trips
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
# LOCALITY POLYGONS with summary counts
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

# ------------------------------------------------------------
# ASK FOR A LOCALITY (NAME OR CODE) AND RUN
# ------------------------------------------------------------

locality_lookup <- kants %>%
  st_drop_geometry() %>%
  select(CODE, KANT_NI, keskus, keskus_CODE)

# Read one line from the user (works in RStudio/console and with Rscript)
stdin_con <- if (interactive()) NULL else file("stdin", open = "r")
ask <- function(prompt) {
  if (interactive()) return(trimws(readline(prompt)))
  cat(prompt)
  answer <- readLines(stdin_con, n = 1, encoding = "UTF-8")
  if (length(answer) == 0) "" else trimws(answer)
}

# Resolve user input to one locality code (NA if cancelled / not found)
resolve_locality <- function(input) {
  # Code, e.g. K372 or k372
  if (grepl("^[Kk][0-9]+$", input)) {
    code <- toupper(input)
    if (code %in% locality_lookup$CODE) return(code)
    cat("  No locality with code", code, "\n")
    return(NA_character_)
  }

  # Name: exact match first (case-insensitive), then partial match
  hits <- locality_lookup %>% filter(tolower(KANT_NI) == tolower(input))
  if (nrow(hits) == 0) {
    hits <- locality_lookup %>%
      filter(str_detect(tolower(KANT_NI), fixed(tolower(input))))
  }
  if (nrow(hits) == 0) {
    cat("  No locality matching \"", input, "\"\n", sep = "")
    return(NA_character_)
  }
  if (nrow(hits) == 1) return(hits$CODE)

  # Several matches (e.g. duplicate village names): let the user choose
  hits <- hits %>% arrange(KANT_NI, keskus)
  cat("  Several localities match:\n")
  for (i in seq_len(nrow(hits))) {
    cat(sprintf("    %2d) %s (%s) - centre: %s\n",
                i, hits$KANT_NI[i], hits$CODE[i], hits$keskus[i]))
  }
  choice <- suppressWarnings(as.integer(ask("  Choose a number: ")))
  if (is.na(choice) || choice < 1 || choice > nrow(hits)) {
    cat("  Invalid choice\n")
    return(NA_character_)
  }
  hits$CODE[choice]
}

# Command-line codes/names are used if given; otherwise ask until empty input
cli_args <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)
queue <- cli_args

repeat {
  input <- if (length(queue) > 0) {
    x <- queue[1]; queue <- queue[-1]; x
  } else if (length(cli_args) > 0) {
    ""                                  # all command-line entries done
  } else {
    ask("\nLocality name or code (empty to quit): ")
  }
  if (!nzchar(input)) break

  code <- resolve_locality(input)
  if (is.na(code)) next

  row <- locality_lookup %>% filter(CODE == code)
  if (row$CODE == row$keskus_CODE) {
    cat("  ", row$KANT_NI, " (", code, ") is itself a commuting centre - nothing to validate\n", sep = "")
    next
  }
  run_validation(code)
}

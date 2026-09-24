# ============================================================
# SUMMARY STATISTICS FOR PAPER
# PT connectivity from localities to commuting centres
# ============================================================
# Requires: output/pt_commute_comparison.csv
# ============================================================

library(tidyverse)

options(scipen = 999)

df <- read_csv("output/pt_commute_comparison.csv", show_col_types = FALSE)

# ============================================================
# HELPERS
# ============================================================

pct <- function(n, total) sprintf("%.1f%%", 100 * n / total)
fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

section <- function(title) {
  cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")
  cat(" ", title, "\n", sep = "")
  cat(paste(rep("=", 64), collapse = ""), "\n", sep = "")
}

subsection <- function(title) {
  cat("\n  -- ", title, " --\n", sep = "")
}

row <- function(label, n, total, pop = NULL, pop_total = NULL, indent = 4) {
  pad <- paste(rep(" ", indent), collapse = "")
  if (!is.null(pop)) {
    cat(sprintf("%s%-46s %5s  (%s)    pop: %s  (%s)\n",
                pad, label,
                fmt(n),    pct(n, total),
                fmt(pop),  pct(pop, pop_total)))
  } else {
    cat(sprintf("%s%-46s %5s  (%s)\n",
                pad, label, fmt(n), pct(n, total)))
  }
}

# ============================================================
# PARTITION NON-CENTRE LOCALITIES
# ============================================================

centres    <- df %>% filter( is_center)
non_centre <- df %>% filter(!is_center)

# Three mutually exclusive PT-access categories
no_pt_any   <- non_centre %>%
  filter(is.na(total_outbound_trips) | total_outbound_trips == 0)

pt_not_to_centre <- non_centre %>%
  filter(!is.na(total_outbound_trips), total_outbound_trips > 0,
         !has_pt_service)

pt_to_centre <- non_centre %>%
  filter(has_pt_service)

# Among those with PT to centre
pt_primary   <- pt_to_centre %>% filter( pt_matches_commute)
pt_secondary <- pt_to_centre %>% filter(!pt_matches_commute)

# AM/PM feasibility (base: localities with any PT to centre)
feas_both   <- pt_to_centre %>% filter( has_am_service &  has_pm_return)
feas_am     <- pt_to_centre %>% filter( has_am_service & !has_pm_return)
feas_pm     <- pt_to_centre %>% filter(!has_am_service &  has_pm_return)
feas_none   <- pt_to_centre %>% filter(!has_am_service & !has_pm_return)

N  <- nrow(non_centre)
Nc <- nrow(centres)
P  <- sum(non_centre$population, na.rm = TRUE)
Pc <- sum(centres$population,    na.rm = TRUE)

# ============================================================
# TRAVEL-TIME OVERVIEW (all localities, including centres)
# ============================================================

travel_time_levels <- c(
  "Commuting centre (N/A)",
  "0–15 min",
  "15–30 min",
  "30–45 min",
  "45–60 min",
  "60–90 min",
  "90+ min",
  "No direct PT to centre"
)

travel_time_overview <- df %>%
  mutate(
    tt_bucket = case_when(
      is_center ~ "Commuting centre (N/A)",
      !has_pt_service | is.na(median_travel_time_min) ~ "No direct PT to centre",
      median_travel_time_min < 15 ~ "0–15 min",
      median_travel_time_min < 30 ~ "15–30 min",
      median_travel_time_min < 45 ~ "30–45 min",
      median_travel_time_min < 60 ~ "45–60 min",
      median_travel_time_min < 90 ~ "60–90 min",
      TRUE ~ "90+ min"
    ),
    tt_bucket = factor(tt_bucket, levels = travel_time_levels)
  ) %>%
  group_by(tt_bucket) %>%
  summarise(
    n_localities = n(),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    tt_bucket = factor(travel_time_levels, levels = travel_time_levels),
    fill = list(n_localities = 0L, population = 0)
  ) %>%
  mutate(
    pct_localities = 100 * n_localities / nrow(df),
    pct_population = 100 * population / sum(df$population, na.rm = TRUE)
  )

print_travel_time_overview <- function(tt_df) {
  cat(sprintf("  %-30s %10s %16s %12s %16s\n",
              "Travel time to commuting centre",
              "Localities", "% of localities",
              "Population", "% of population"))
  cat(sprintf("  %s\n", paste(rep("-", 94), collapse = "")))
  for (i in seq_len(nrow(tt_df))) {
    r <- tt_df[i, ]
    cat(sprintf("  %-30s %10s %15.1f%% %12s %15.1f%%\n",
                as.character(r$tt_bucket),
                fmt(r$n_localities),
                r$pct_localities,
                fmt(r$population),
                r$pct_population))
  }
}

# ============================================================
# OUTPUT
# ============================================================

section("OVERVIEW")
cat(sprintf("  Total localities (kants):           %s\n", fmt(nrow(df))))
cat(sprintf("  Commuting centres:                  %s\n", fmt(Nc)))
cat(sprintf("  Non-centre localities:              %s\n", fmt(N)))
cat(sprintf("\n  Total population (all kants):       %s\n", fmt(sum(df$population, na.rm=TRUE))))
cat(sprintf("  Population in centres:              %s  (%s of total)\n",
            fmt(Pc), pct(Pc, Pc + P)))
cat(sprintf("  Population in non-centre localities:%s  (%s of total)\n",
            fmt(P),  pct(P,  Pc + P)))

# ============================================================
section("1. PT CONNECTIVITY TO COMMUTING CENTRE")
cat(sprintf("  Base: %s non-centre localities  |  population: %s\n\n", fmt(N), fmt(P)))
# ============================================================

row("No PT service at all",
    nrow(no_pt_any), N,
    sum(no_pt_any$population, na.rm=TRUE), P)

row("PT exists but does not reach centre",
    nrow(pt_not_to_centre), N,
    sum(pt_not_to_centre$population, na.rm=TRUE), P)

row("At least one PT connection to centre",
    nrow(pt_to_centre), N,
    sum(pt_to_centre$population, na.rm=TRUE), P)

subsection("Among localities WITH PT to centre")
cat(sprintf("  Base: %s localities  |  population: %s\n\n",
            fmt(nrow(pt_to_centre)),
            fmt(sum(pt_to_centre$population, na.rm=TRUE))))

row("Centre is primary PT destination (#1)",
    nrow(pt_primary), nrow(pt_to_centre),
    sum(pt_primary$population, na.rm=TRUE),
    sum(pt_to_centre$population, na.rm=TRUE))

row("Centre is NOT primary PT destination",
    nrow(pt_secondary), nrow(pt_to_centre),
    sum(pt_secondary$population, na.rm=TRUE),
    sum(pt_to_centre$population, na.rm=TRUE))

# Median service frequency among those with PT to centre
med_freq  <- median(pt_to_centre$avg_frequency_min,  na.rm = TRUE)
med_ttime <- median(pt_to_centre$median_travel_time_min, na.rm = TRUE)
med_daily <- median(pt_to_centre$n_services_to_keskus,   na.rm = TRUE)
subsection("Service level summary (localities with PT to centre)")
cat(sprintf("  Median daily services to centre:    %.0f\n", med_daily))
cat(sprintf("  Median travel time to centre:       %.0f min\n", med_ttime))
cat(sprintf("  Median gap between services:        %.0f min\n", med_freq))

# ============================================================
section("2. MINIMUM FEASIBLE ROUND-TRIP CONNECTIVITY")
cat("  Definition:\n")
cat("    AM outbound: at least one arrival at centre 07:00–09:00\n")
cat("    PM return:   at least one departure from centre 15:00–19:00\n")
cat(sprintf("\n  Base: %s non-centre localities  |  population: %s\n\n", fmt(N), fmt(P)))
# ============================================================

# Using full non-centre base (including no-PT localities) for overall picture
row("Full round-trip feasible (AM + PM)",
    nrow(feas_both), N,
    sum(feas_both$population, na.rm=TRUE), P)

row("AM service only (no PM return)",
    nrow(feas_am), N,
    sum(feas_am$population, na.rm=TRUE), P)

row("PM return only (no AM outbound)",
    nrow(feas_pm), N,
    sum(feas_pm$population, na.rm=TRUE), P)

row("PT to centre but outside peak windows",
    nrow(feas_none), N,
    sum(feas_none$population, na.rm=TRUE), P)

row("No PT connection to centre at all",
    nrow(no_pt_any) + nrow(pt_not_to_centre), N,
    sum(c(no_pt_any$population, pt_not_to_centre$population), na.rm=TRUE), P)

subsection("Among localities WITH PT to centre")
cat(sprintf("  Base: %s localities  |  population: %s\n\n",
            fmt(nrow(pt_to_centre)),
            fmt(sum(pt_to_centre$population, na.rm=TRUE))))

row("Full round-trip feasible (AM + PM)",
    nrow(feas_both), nrow(pt_to_centre),
    sum(feas_both$population, na.rm=TRUE),
    sum(pt_to_centre$population, na.rm=TRUE))

row("AM service only",
    nrow(feas_am), nrow(pt_to_centre),
    sum(feas_am$population, na.rm=TRUE),
    sum(pt_to_centre$population, na.rm=TRUE))

row("PM return only",
    nrow(feas_pm), nrow(pt_to_centre),
    sum(feas_pm$population, na.rm=TRUE),
    sum(pt_to_centre$population, na.rm=TRUE))

row("PT to centre but outside peak windows",
    nrow(feas_none), nrow(pt_to_centre),
    sum(feas_none$population, na.rm=TRUE),
    sum(pt_to_centre$population, na.rm=TRUE))

# ============================================================
section("3. POPULATION-WEIGHTED FEASIBILITY SUMMARY")
# ============================================================

# Worker-weighted (commuters are the relevant population for PT-to-work)
W  <- sum(non_centre$workers, na.rm = TRUE)

cat(sprintf("  Total workers in non-centre localities: %s\n\n", fmt(W)))

wrow <- function(label, sub_df) {
  n   <- nrow(sub_df)
  p   <- sum(sub_df$population, na.rm = TRUE)
  w   <- sum(sub_df$workers, na.rm = TRUE)
  cat(sprintf("  %-46s %5s  |  pop %s (%s)  |  workers %s (%s)\n",
              label,
              fmt(n),
              fmt(p),  pct(p, P),
              fmt(w),  pct(w, W)))
}

wrow("No PT service at all",                no_pt_any)
wrow("PT but not to designated centre",     pt_not_to_centre)
wrow("PT to centre (any service)",          pt_to_centre)
wrow("  of which: centre is primary dest.", pt_primary)
wrow("  of which: full round-trip (AM+PM)", feas_both)
wrow("  of which: AM only",                 feas_am)
wrow("  of which: PM only",                 feas_pm)
wrow("  of which: off-peak PT only",        feas_none)

# Worker-weighted mean feasibility rate
wt_feasible <- weighted.mean(non_centre$return_feasible,
                              non_centre$workers, na.rm = TRUE)
wt_any_pt   <- weighted.mean(non_centre$has_pt_service,
                              non_centre$workers, na.rm = TRUE)
cat(sprintf("\n  Worker-weighted share with any PT to centre:      %.1f%%\n",
            100 * wt_any_pt))
cat(sprintf("  Worker-weighted share with full round-trip:       %.1f%%\n",
            100 * wt_feasible))

# ============================================================
section("4. DEMAND–SUPPLY GAP SUMMARY")
cat("  demand_supply_gap = share_to_center − return_feasible (binary)\n")
cat("  Positive = commuting demand exceeds round-trip PT supply\n\n")
# ============================================================

gap_df <- non_centre %>% filter(!is.na(demand_supply_gap))

cat(sprintf("  Localities with positive gap (unmet demand):   %s  (%s)\n",
            fmt(sum(gap_df$demand_supply_gap > 0, na.rm = TRUE)),
            pct(sum(gap_df$demand_supply_gap > 0, na.rm = TRUE), nrow(gap_df))))
cat(sprintf("  Localities with zero/negative gap:             %s  (%s)\n",
            fmt(sum(gap_df$demand_supply_gap <= 0, na.rm = TRUE)),
            pct(sum(gap_df$demand_supply_gap <= 0, na.rm = TRUE), nrow(gap_df))))
cat(sprintf("\n  Mean demand–supply gap (unweighted):           %.3f\n",
            mean(gap_df$demand_supply_gap, na.rm = TRUE)))
cat(sprintf("  Mean demand–supply gap (worker-weighted):      %.3f\n",
            weighted.mean(gap_df$demand_supply_gap, gap_df$workers, na.rm = TRUE)))
cat(sprintf("  Median demand–supply gap:                      %.3f\n",
            median(gap_df$demand_supply_gap, na.rm = TRUE)))

# Top mismatches
subsection("10 localities with largest unmet demand (highest gap)")
gap_df %>%
  arrange(desc(demand_supply_gap)) %>%
  slice_head(n = 10) %>%
  select(kant_name, keskus_name, population, workers,
         share_to_center, return_feasible, demand_supply_gap) %>%
  mutate(share_to_center = round(share_to_center, 1),
         demand_supply_gap = round(demand_supply_gap, 3)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# ============================================================
section("5. BREAKDOWN BY FUNCTIONAL REGION (commuting centre)")
# ============================================================

by_centre <- non_centre %>%
  group_by(keskus_name) %>%
  summarise(
    n_localities        = n(),
    pop_total           = sum(population,  na.rm = TRUE),
    workers_total       = sum(workers,     na.rm = TRUE),
    n_any_pt            = sum(has_pt_service,      na.rm = TRUE),
    n_top_match         = sum(pt_matches_commute,  na.rm = TRUE),
    n_return_feasible   = sum(return_feasible,     na.rm = TRUE),
    pop_any_pt          = sum(population[has_pt_service],     na.rm = TRUE),
    pop_top_match       = sum(population[pt_matches_commute], na.rm = TRUE),
    pop_feasible        = sum(population[return_feasible],    na.rm = TRUE),
    pct_any_pt          = round(100 * n_any_pt          / n_localities, 1),
    pct_top_match       = round(100 * n_top_match       / n_localities, 1),
    pct_feasible        = round(100 * n_return_feasible / n_localities, 1),
    pop_pct_any_pt      = round(100 * pop_any_pt    / pop_total, 1),
    pop_pct_top_match   = round(100 * pop_top_match / pop_total, 1),
    pop_pct_feasible    = round(100 * pop_feasible  / pop_total, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_feasible))

print_by_centre <- function(bc, N, P, pt_to_centre, pt_primary, feas_both) {
  cat("\n")
  cat(sprintf("  %-22s  %4s  %8s  %5s  %5s  %5s  %6s  %6s  %6s  %8s  %8s\n",
              "Centre", "Locs", "Pop",
              "%PT", "%Top", "%RT",
              "pop%PT", "pop%Top", "pop%RT",
              "res.Top", "res.RT"))
  cat(sprintf("  %s\n", paste(rep("-", 106), collapse = "")))
  for (i in seq_len(nrow(bc))) {
    r <- bc[i, ]
    cat(sprintf("  %-22s  %4d  %8s  %5.1f  %5.1f  %5.1f  %6.1f  %8.1f  %6.1f  %8s  %8s\n",
                substr(r$keskus_name, 1, 22),
                r$n_localities,
                format(r$pop_total,     big.mark = ",", width = 8),
                r$pct_any_pt,
                r$pct_top_match,
                r$pct_feasible,
                r$pop_pct_any_pt,
                r$pop_pct_top_match,
                r$pop_pct_feasible,
                format(r$pop_top_match, big.mark = ",", width = 8),
                format(r$pop_feasible,  big.mark = ",", width = 8)))
  }
  cat(sprintf("  %s\n", paste(rep("-", 106), collapse = "")))
  cat(sprintf("  %-22s  %4d  %8s  %5.1f  %5.1f  %5.1f  %6.1f  %8.1f  %6.1f  %8s  %8s\n",
              "TOTAL / MEAN", N,
              format(P, big.mark = ",", width = 8),
              100 * nrow(pt_to_centre) / N,
              100 * nrow(pt_primary)   / N,
              100 * nrow(feas_both)    / N,
              100 * sum(pt_to_centre$population, na.rm = TRUE) / P,
              100 * sum(pt_primary$population,   na.rm = TRUE) / P,
              100 * sum(feas_both$population,    na.rm = TRUE) / P,
              format(sum(pt_primary$population, na.rm = TRUE), big.mark = ",", width = 8),
              format(sum(feas_both$population,  na.rm = TRUE), big.mark = ",", width = 8)))
  cat("\n  Columns: %PT = any PT to centre  |  %Top = centre is #1 PT destination",
      " |  %RT = AM+PM round-trip\n")
  cat("           pop% = population-weighted  |  res. = residents meeting criterion\n")
}

print_by_centre(by_centre, N, P, pt_to_centre, pt_primary, feas_both)

# ============================================================
section("6. TRAVEL TIME TO COMMUTING CENTRE (PT)")
# ============================================================
print_travel_time_overview(travel_time_overview)

# ============================================================
# SAVE TO FILE
# ============================================================

output_file <- "output/summary_statistics.txt"
sink(output_file)

section("OVERVIEW")
cat(sprintf("  Total localities (kants):           %s\n", fmt(nrow(df))))
cat(sprintf("  Commuting centres:                  %s\n", fmt(Nc)))
cat(sprintf("  Non-centre localities:              %s\n", fmt(N)))
cat(sprintf("\n  Total population (all kants):       %s\n", fmt(sum(df$population, na.rm=TRUE))))
cat(sprintf("  Population in centres:              %s  (%s of total)\n", fmt(Pc), pct(Pc, Pc + P)))
cat(sprintf("  Population in non-centre localities:%s  (%s of total)\n", fmt(P),  pct(P,  Pc + P)))

section("1. PT CONNECTIVITY TO COMMUTING CENTRE")
cat(sprintf("  Base: %s non-centre localities  |  population: %s\n\n", fmt(N), fmt(P)))
row("No PT service at all",                 nrow(no_pt_any),       N, sum(no_pt_any$population, na.rm=TRUE),       P)
row("PT exists but does not reach centre",  nrow(pt_not_to_centre),N, sum(pt_not_to_centre$population, na.rm=TRUE),P)
row("At least one PT connection to centre", nrow(pt_to_centre),    N, sum(pt_to_centre$population, na.rm=TRUE),    P)
subsection("Among localities WITH PT to centre")
cat(sprintf("  Base: %s localities  |  population: %s\n\n", fmt(nrow(pt_to_centre)), fmt(sum(pt_to_centre$population, na.rm=TRUE))))
row("Centre is primary PT destination (#1)", nrow(pt_primary),  nrow(pt_to_centre), sum(pt_primary$population,  na.rm=TRUE), sum(pt_to_centre$population, na.rm=TRUE))
row("Centre is NOT primary PT destination",  nrow(pt_secondary),nrow(pt_to_centre), sum(pt_secondary$population,na.rm=TRUE), sum(pt_to_centre$population, na.rm=TRUE))
subsection("Service level summary (localities with PT to centre)")
cat(sprintf("  Median daily services to centre:    %.0f\n", med_daily))
cat(sprintf("  Median travel time to centre:       %.0f min\n", med_ttime))
cat(sprintf("  Median gap between services:        %.0f min\n", med_freq))

section("2. MINIMUM FEASIBLE ROUND-TRIP CONNECTIVITY")
cat("  Definition:\n    AM outbound: at least one arrival at centre 07:00–09:00\n    PM return:   at least one departure from centre 15:00–19:00\n")
cat(sprintf("\n  Base: %s non-centre localities  |  population: %s\n\n", fmt(N), fmt(P)))
row("Full round-trip feasible (AM + PM)",   nrow(feas_both), N, sum(feas_both$population, na.rm=TRUE), P)
row("AM service only (no PM return)",       nrow(feas_am),   N, sum(feas_am$population,   na.rm=TRUE), P)
row("PM return only (no AM outbound)",      nrow(feas_pm),   N, sum(feas_pm$population,   na.rm=TRUE), P)
row("PT to centre but outside peak windows",nrow(feas_none), N, sum(feas_none$population, na.rm=TRUE), P)
row("No PT connection to centre at all",    nrow(no_pt_any) + nrow(pt_not_to_centre), N, sum(c(no_pt_any$population, pt_not_to_centre$population), na.rm=TRUE), P)
subsection("Among localities WITH PT to centre")
cat(sprintf("  Base: %s localities  |  population: %s\n\n", fmt(nrow(pt_to_centre)), fmt(sum(pt_to_centre$population, na.rm=TRUE))))
row("Full round-trip feasible (AM + PM)",   nrow(feas_both), nrow(pt_to_centre), sum(feas_both$population, na.rm=TRUE), sum(pt_to_centre$population, na.rm=TRUE))
row("AM service only",                      nrow(feas_am),   nrow(pt_to_centre), sum(feas_am$population,   na.rm=TRUE), sum(pt_to_centre$population, na.rm=TRUE))
row("PM return only",                       nrow(feas_pm),   nrow(pt_to_centre), sum(feas_pm$population,   na.rm=TRUE), sum(pt_to_centre$population, na.rm=TRUE))
row("PT to centre but outside peak windows",nrow(feas_none), nrow(pt_to_centre), sum(feas_none$population, na.rm=TRUE), sum(pt_to_centre$population, na.rm=TRUE))

section("3. POPULATION-WEIGHTED FEASIBILITY SUMMARY")
cat(sprintf("  Total workers in non-centre localities: %s\n\n", fmt(W)))
wrow("No PT service at all",                no_pt_any)
wrow("PT but not to designated centre",     pt_not_to_centre)
wrow("PT to centre (any service)",          pt_to_centre)
wrow("  of which: centre is primary dest.", pt_primary)
wrow("  of which: full round-trip (AM+PM)", feas_both)
wrow("  of which: AM only",                 feas_am)
wrow("  of which: PM only",                 feas_pm)
wrow("  of which: off-peak PT only",        feas_none)
cat(sprintf("\n  Worker-weighted share with any PT to centre:      %.1f%%\n", 100 * wt_any_pt))
cat(sprintf("  Worker-weighted share with full round-trip:       %.1f%%\n",  100 * wt_feasible))

section("4. DEMAND–SUPPLY GAP SUMMARY")
cat(sprintf("  Localities with positive gap (unmet demand):   %s  (%s)\n", fmt(sum(gap_df$demand_supply_gap > 0, na.rm=TRUE)), pct(sum(gap_df$demand_supply_gap > 0, na.rm=TRUE), nrow(gap_df))))
cat(sprintf("  Localities with zero/negative gap:             %s  (%s)\n", fmt(sum(gap_df$demand_supply_gap <= 0, na.rm=TRUE)), pct(sum(gap_df$demand_supply_gap <= 0, na.rm=TRUE), nrow(gap_df))))
cat(sprintf("  Mean demand–supply gap (unweighted):           %.3f\n", mean(gap_df$demand_supply_gap, na.rm=TRUE)))
cat(sprintf("  Mean demand–supply gap (worker-weighted):      %.3f\n", weighted.mean(gap_df$demand_supply_gap, gap_df$workers, na.rm=TRUE)))
cat(sprintf("  Median demand–supply gap:                      %.3f\n", median(gap_df$demand_supply_gap, na.rm=TRUE)))

section("5. BREAKDOWN BY FUNCTIONAL REGION")
print_by_centre(by_centre, N, P, pt_to_centre, pt_primary, feas_both)

section("6. TRAVEL TIME TO COMMUTING CENTRE (PT)")
print_travel_time_overview(travel_time_overview)

sink()
cat("\nSummary statistics saved to", output_file, "\n")

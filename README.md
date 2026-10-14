# Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23365662.svg)](https://doi.org/10.5281/zenodo.23365662)

Code and outputs for the study by Martin Haamer and Anto Aasa on the alignment between Estonia’s public transport network and commuting-based functional regions.

Repository: [gitlab.ut.ee/ut-mobilitylab-public/transport-commuting-alignment-estonia](https://gitlab.ut.ee/ut-mobilitylab-public/transport-commuting-alignment-estonia) (moved from github.com/mahrtinh/transport-commuting-alignment-estonia)  
Archived release: [doi:10.5281/zenodo.23365662](https://doi.org/10.5281/zenodo.23365662)

## Overview

This repository documents the methodology of the paper, with the analysis code, public transport input data and derived outputs:

**Haamer, M. and Aasa, A.** (2026). *Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia.* AGILE GIScience Series, 7, 24. [https://doi.org/10.5194/agile-giss-7-24-2026](https://doi.org/10.5194/agile-giss-7-24-2026)

Article: [agile-giss.copernicus.org/articles/7/24/2026](https://agile-giss.copernicus.org/articles/7/24/2026/)

For one typical weekday (Tuesday 15 April 2025) the analysis detects direct public transport (PT) trips between Estonian localities (EE: *kant*), measures the service from each locality to its designated commuting centre (EE: *toimepiirkonna keskus*), and checks whether PT connects localities to the centre they commute to.

The functional regions and their centres come from the published dataset:

**Aasa, A.** (2026). *Commuting-based functional areas and their centres in Estonia, 2021* (Version 1.0) [Dataset]. Zenodo. [https://doi.org/10.5281/zenodo.23240154](https://doi.org/10.5281/zenodo.23240154)

## Example: direct PT connections between localities

![Direct public transport connections between Estonian localities](figures/pt_direct_connections_map.jpg)

Map made in QGIS from `results/pt_adjacent_links.gpkg` (layers `adjacent_links` and `non_adjacent_links`). Each line joins the representative stops of two localities that a bus, train or ferry travels between without stopping in another locality; line width shows the number of daily services (1–737), for all PT types (long-distance, city and county buses, trains, ferries). For example, 83 trips run between Tallinn and Tartu per day, 21 of them without an intermediate stop (the straight line between the two cities). The busiest links are Harkujärve–Tallinn (737 services per day) and Jõhvi–Kohtla-Järve (491).

## Repository structure

```
scripts/
  PT_commute_comparison.R   main analysis
  validate_locality.R       per-locality check of all trips to/from the centre
  as_published/             scripts as used for the published article
data/
  GTFS/gtfs_2025_04/gtfs/   national GTFS feed, April 2025
  toimepiirkonnad/          functional areas dataset, downloaded from Zenodo on first run (not tracked)
results/                    outputs of PT_commute_comparison.R
figures/                    example map
```

## Data

| File | Content | Source |
|---|---|---|
| `data/GTFS/gtfs_2025_04/gtfs/` | Public transport schedules (GTFS) | Estonian Transport Administration (Transpordiamet), open data |
| `data/toimepiirkonnad/toimepiirkonnad_2021.gpkg` (layer `kandid`) | 840 locality polygons (EPSG:3301) with code (`CODE`), name (`KANT_NI`), commuting centre (`keskus`, `keskus_CODE`), population (`RAHVAAR`), employed residents (`t66tajad_n`) and share of them working in the centre (`osak_t66tajatest`) | Aasa (2026), [doi:10.5281/zenodo.23240154](https://doi.org/10.5281/zenodo.23240154), CC BY 4.0; derived from Statistics Estonia 2021 data |

### Functional regions

The functional regions (21 functional areas and their centres) are delineated from the Statistics Estonia 2021 home-to-work commuting origin–destination matrix at locality level, with a territorial contiguity constraint and a minimum population of 5,000. They were created for the settlement development scenarios study of the Estonian national spatial plan (ÜRP 2050) and are published on Zenodo:

> Aasa, A. (2026). Commuting-based functional areas and their centres in Estonia, 2021 (Version 1.0) [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.23240154

The dataset is not redistributed in this repository. `scripts/PT_commute_comparison.R` downloads `toimepiirkonnad_2021.gpkg` from the Zenodo record into `data/toimepiirkonnad/` on its first run and reads the `kandid` layer. To download it manually, get the file from [the Zenodo record](https://zenodo.org/records/23240154) and save it at that path. The codebook (`codebook.csv`) and documentation of the dataset are in the same record.

This is the same functional regions data that was used for the published article.

## How to run

Requirements: R with the packages `tidyverse`, `sf` and `scales`, and an internet connection on the first run (to download the functional regions dataset from Zenodo).

Run from the repository root (all paths are relative):

```bash
Rscript scripts/PT_commute_comparison.R
```

This takes several minutes and writes the files in `results/`.

To inspect one locality in detail (all outbound and inbound trips, with QGIS layers):

```bash
Rscript scripts/validate_locality.R            # asks for a locality name or code
Rscript scripts/validate_locality.R K372       # or give codes/names directly
```

The validation script uses `results/pt_validation_inputs.rds`, which is created by the main script, so run that first.

## Method in brief

1. Keep GTFS services active on the target date and assign stops to localities.
2. For each trip, build the ordered sequence of localities it serves and all forward origin–destination pairs.
3. Count trips per locality pair; find each locality’s top PT destination and the number of trips departing from it.
4. For trips from a locality to its commuting centre: travel time, frequency, service span and arrivals in the AM peak (07:00–09:00).
5. Count PM return trips from the centre to the locality (departing 15:00–19:00).
6. Compare with the commuting-centre assignment: does the top PT destination match the centre, and is a round trip (AM arrival + PM return) possible?

Method choices:

- A trip counts once per locality pair, however many stops it makes in a locality.
- Stops with the same name within 250 m of each other are treated as one stop.
- Stops just outside all locality polygons (e.g. harbour piers) are assigned to the nearest locality within 50 m; stops abroad (e.g. Valka bus station, 99 m from Valga) are left out.
- Travel time to the centre is measured to the centre stop with the most incoming services from the centre’s localities (usually the bus station).
- On loop routes that pass the origin stop twice before reaching the centre, the last visit is used.
- Trips of 240 minutes or more are excluded from the centre metrics.
- Neighbouring localities are polygons within 1 m of each other.
- Lines in the spatial outputs connect representative stops: the stop of each locality with the most services on the target date (for a group of nearby same-name stops, their mean position).

## Outputs

| File | Content |
|---|---|
| `pt_commute_comparison.csv` / `.gpkg` | One row per locality: top PT destination, trips to the centre, share of departing trips reaching the centre, travel time and frequency statistics, AM/PM service and round-trip feasibility |
| `locality_stop_service_rankings.csv` | Stops of each locality ranked by number of services, with representative stops |
| `pt_connections_lines.gpkg` | Lines between the representative stops of every pair of localities connected by PT, with trip counts in both directions |
| `pt_adjacent_links.gpkg` | Lines between the representative stops of neighbouring localities with the number of services moving directly between them (style line width by `n_services_total` in QGIS) |

Main columns of `pt_commute_comparison.csv`:

| Column | Meaning |
|---|---|
| `kant_CODE`, `kant_name` | Locality code and name |
| `keskus_CODE`, `keskus_name` | Assigned commuting centre |
| `top_pt_dest`, `n_trips_to_top` | Locality with the most direct trips from this locality, and the number of trips |
| `total_outbound_trips` | Trips departing from the locality to any other locality |
| `n_trips_to_keskus`, `prop_to_keskus` | Trips to the centre, and their share of departing trips |
| `pt_matches_commute` | Top PT destination is the commuting centre |
| `median_travel_time_min` | Median travel time to the centre |
| `avg_frequency_min` | Average interval between departures to the centre |
| `am_peak_services` | Trips arriving at the centre 07:00–09:00 |
| `pm_return_services` | Trips from the centre to the locality departing 15:00–19:00 |
| `return_feasible` | Both an AM arrival and a PM return exist |
| `is_center` | Locality is itself a commuting centre |

## Differences from the published article

The code in `scripts/` has been revised since the article was published. The scripts that produced the published results are kept unchanged in `scripts/as_published/`.

The revision changes four things in how trips to the commuting centre are counted and timed:

- **Trips stopping more than once in the origin locality.** When a bus stopped at several stops within the same locality on its way to the centre, the published code counted it as several trips (one per stop). The revised code counts it as one trip.
- **Loop routes.** When a route passed the same origin stop twice before reaching the centre, the published code measured travel time from the first pass. The revised code measures it from the last pass, so the time spent going around the loop is not included.
- **Arrival stop in the centre.** The published code measured travel time to the last stop the trip made in the centre locality. The revised code measures it to the centre's main stop (the one with the most incoming services, usually the bus station).
- **Harbour stops.** Stops just outside the locality polygons (e.g. Heltermaa harbour) were dropped in the published code. The revised code assigns them to the nearest locality.

The alignment and minimum-service results are essentially unchanged. The travel-time results (Section 3.2, Table 3, Fig. 3) change:

| Result in the article | Published | Revised code |
|---|---|---|
| Localities with PT aligned to the commuting centre | 365 (44.5%) | 366 (44.7%) |
| Localities with minimum level of service | 566 (69.1%) | 568 (69.4%) |
| Non-centre residents without minimum service | 12.4% | 12.3% |
| Localities without direct PT to the centre | 170 (20.8%) | 169 (20.6%) |
| **Median travel time to the centre** | **43 min** | **34.5 min** |
| Residents within 15 min of their centre | 6.3% (55 localities) | 16.6% (108 localities) |
| Residents more than 60 min from their centre | 17.8% | 13.0% |

## Use of generative AI

AI tools (Claude) were used in developing this repository: for structuring the code of the original analysis, revision, implementing the corrections listed in [Differences from the published article](#differences-from-the-published-article), and drafting documentation. All methodological decisions were made by the authors, and the results were checked by the authors against the GTFS data.

## Citation

If you use this repository, please cite the associated paper and the archived repository release:

> Haamer, M. and Aasa, A.: Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia, AGILE GIScience Ser., 7, 24, https://doi.org/10.5194/agile-giss-7-24-2026, 2026.

> Haamer, M. and Aasa, A. (2026). transport-commuting-alignment-estonia (Version 1.1.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.23365662

If you use the functional regions, please also cite the dataset:

> Aasa, A. (2026). Commuting-based functional areas and their centres in Estonia, 2021 (Version 1.0) [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.23240154

See `CITATION.cff` for citation metadata.

## License

The code is released under the MIT License (see `LICENSE`). The input data are subject to the terms of their original providers (see [Data](#data)); the functional regions dataset is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Contact

Martin Haamer  
Mobility Lab, Department of Geography, University of Tartu, Tartu, Estonia

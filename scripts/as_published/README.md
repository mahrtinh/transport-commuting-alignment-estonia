# Scripts as published

These are the scripts, unchanged, that produced the results in:

Haamer, M. and Aasa, A.: Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia, AGILE GIScience Ser., 7, 24, https://doi.org/10.5194/agile-giss-7-24-2026, 2026.

- `PT_commute_comparison.R` — main analysis (writes `output/pt_commute_comparison.csv` and spatial outputs)
- `generate_summary_statistics.R` — Tables 2 and 3 and the regional statistics, from `output/pt_commute_comparison.csv`

They are kept for reproducibility of the published numbers. For new work use the revised scripts in `scripts/`; see "Differences from the published article" in the main README.

Note: these scripts read the functional regions dataset from `data/Toimepiirkonnad/pendelr6nde_toimepiirkonnad_3.gpkg` and write to `output/`. That file is the dataset now published on Zenodo (Aasa, A. (2026). Commuting-based functional areas and their centres in Estonia, 2021 (Version 1.0) [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.23240154). Its `kandid` layer has the same localities, centre assignments and attributes. To rerun these scripts, download `toimepiirkonnad_2021.gpkg` from the record and save it at that path (the `kandid` layer is the first layer, which `st_read` reads by default).

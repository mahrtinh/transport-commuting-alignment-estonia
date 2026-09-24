# Scripts as published

These are the scripts, unchanged, that produced the results in:

Haamer, M. and Aasa, A.: Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia, AGILE GIScience Ser., 7, 24, https://doi.org/10.5194/agile-giss-7-24-2026, 2026.

- `PT_commute_comparison.R` — main analysis (writes `output/pt_commute_comparison.csv` and spatial outputs)
- `generate_summary_statistics.R` — Tables 2 and 3 and the regional statistics, from `output/pt_commute_comparison.csv`

They are kept for reproducibility of the published numbers. For new work use the revised scripts in `scripts/`; see "Differences from the published article" in the main README.

Note: these scripts expect the full functional regions dataset (`data/Toimepiirkonnad/pendelr6nde_toimepiirkonnad_3.gpkg`, with population and employment attributes), which is not publicly available, and write to `output/`. They do not run with the public `data/localities/localities.gpkg`.

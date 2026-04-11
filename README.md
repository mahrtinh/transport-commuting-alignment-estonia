# Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia

Code and outputs for the study by Martin Haamer and Anto Aasa on the alignment between Estonia’s public transport network and commuting-based functional regions.

## Overview

This repository contains the analysis code, derived outputs, and documentation for the paper:

**Haamer, M. and Aasa, A.**  
*Linking Public Transport Provision to Functional Regions: Evaluating Transport–Commuting Alignment in Estonia*

## Repository structure

- `src/` — reusable functions
- `scripts/` — analysis scripts
- `data/` — input data notes and shareable data
- `results/` — output datasets and tables
- `figures/` — exported figures
- `docs/` — project documentation
- `manuscript/` — manuscript-related materials

## Data availability

Public transport schedule data in GTFS format are publicly available from the Estonian Transport Administration.

The functional regions dataset used in this study is part of the ongoing “Eesti 2050” national planning process and is currently not publicly available.

This repository provides:
- analysis code
- workflow documentation
- publicly shareable derived outputs
- reproduction guidance where possible

## Reproducibility

The analysis was conducted in R and QGIS.

General workflow:

1. Prepare GTFS data
2. Join stops to locality polygons
3. Generate origin–destination locality pairs from active trips
4. Aggregate trip counts by locality pair
5. Evaluate alignment with designated commuting centres
6. Calculate minimum service levels for daily commuting
7. Compute travel time indicators
8. Export figures and tables

## Citation

If you use this repository, please cite the associated paper and the archived repository release.

See `CITATION.cff` for citation metadata.

## License

See the `LICENSE` file for reuse terms.

## Contact

Martin Haamer  
Mobility Lab, Department of Geography, University of Tartu, Tartu, Estonia

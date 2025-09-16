# Dispersal across heterogeneous landscapes unlocks stable coexistence of competing species
_Code to reproduce the results in Allen-Perkins, Godoy & García-Callejas (2025)_

[![DOI](https://zenodo.org/badge/DOI/XXXXXXXXX.svg)](https://doi.org/XXXXXXXXX)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](#license)

## Overview
This repository contains all code used to reproduce the analyses and figures for the manuscript:

> **Allen-Perkins, A., Godoy, O., & García-Callejas, D. (2025)**  
> *Dispersal across heterogeneous landscapes unlocks stable coexistence of competing species.*

**What’s here**
- `R-scripts/` — R scripts to run the calculations and generate derived data, plus scripts to create the figures for the main text and appendices.  
- `Maple-scripts/` — Maple worksheets with equilibrium equations for the three interaction scenarios considered.
- `Figures/` — mages and videos used in the manuscript (main text and appendices).

> **Data note:** We do **not** deposit the large derived/output datasets due to file size. All outputs can be **regenerated** from the shared code (see below). Specific files can be provided on reasonable request.

## Requirements
- **R** ≥ 4.2 (tested on: 4.4.3)  
- **R packages:** `tidyverse`, `landscapemetrics`, `terra`, `parallel`, `ggplot2`, `patchwork`, `latex2exp`, `av`, `data.table`, `scales`
- **Maple** (optional, for symbolic derivations): Maple ≥ 2023.0.  
  Maple is not required to reproduce the paper’s figures and videos; the R pipeline uses closed-form expressions already exported to R or computed within R.

## License
This project is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
See the `LICENSE` file for full terms.

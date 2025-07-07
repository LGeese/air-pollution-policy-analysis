# Air Pollution Policy Analysis

This repository contains the materials for a pre-interview task for the Research Fellow in Computational Social Science position at the University of Birmingham.

## Project Overview

The goal of this project is to analyse how air pollution-related climate policies affect public health outcomes, specifically average annual exposure to PM2.5 air pollution. The project includes:

- Development of an air pollution policy indicator based on structural topic modelling (STM) of climate policy documents
- Panel data analysis linking the indicator to public health outcomes
- Validation steps and robustness analysis

## Repository Structure

- `data/`: Raw and processed datasets
- `scripts/`: R scripts for indicator development and analysis
- `output/`: Visualisations and tables
- `models/`: Saved ML models
- `report/`: Final report


## How to Reproduce

1. Clone or download the repository.
2. Open the R project (`.Rproj`) in RStudio.
3. Run the scripts in order from the `scripts/` directory.
   - Ensure internet connection for online data sources (e.g., V-Dem, World Bank).
   - Output will be stored in the `output/` directory.

## Requirements

The analysis requires the following R packages:

- `tidyverse`
- `stm`
- `plm`
- `broom`
- `cowplot`
- `sjmisc`
- `vdemdata`
- `here`
- `stringi`
- `quanteda`
- `rvest`
- `officer`
- `readxl`

You can install all missing packages using the first code blocks in each script.

## Author

Dr. Lucas Geese  
School of Environmental Sciences, University of East Anglia  

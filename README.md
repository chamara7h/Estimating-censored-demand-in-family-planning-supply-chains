# Estimating censored demand in family planning supply chains: forecast accuracy, inventory implications, and public health outcomes

This repository accompanies the article in *Production and Operations Management*. It contains the prepared study data, R scripts, and Quarto manuscript sources. Large generated forecasts and results are omitted to keep the repository small. The workflow below shows which files can be regenerated from the included data and which analyses require the original benchmark forecasts.

**DOI** [10.1177/10591478261511731](https://doi.org/10.1177/10591478261511731)

## Authors

- **Harsha Chamara Hewage** (corresponding author), Data Lab for Social Good Research Group, Cardiff Business School, Cardiff University, UK. [Email](mailto:HalgamuweHewageHR@cardiff.ac.uk)
- **Bahman Rostami-Tabar**, Data Lab for Social Good Research Group, Cardiff Business School, Cardiff University, UK.
- **Aris Syntetos**, PARC Institute of Manufacturing, Logistics & Inventory, Cardiff Business School, Cardiff University, UK.
- **Federico Liberatore**, Cardiff School of Computer Science & Informatics, Cardiff University, UK.
- **Glenn R. Milano**, formerly Bureau for Global Health, United States Agency for International Development, USA.

## Abstract

Stockouts and service interruptions can hide demand for contraceptive products. Forecasts based on recorded quantities distributed can underestimate demand and contribute to repeated stockouts. The study proposes the Truncated Conformal Kalman Filter (TCKF), which combines a Tobit Kalman Filter with conformal prediction. TCKF distinguishes partial censoring during stockouts from full censoring during service interruptions or periods when facilities receive no stock, adapts to prolonged uncertainty, and produces prediction intervals. The study evaluates the method using synthetic demand and family planning logistics data from Côte d’Ivoire. The results show improvements in point and probabilistic forecast accuracy and simulated inventory performance, with implications for product availability and public health.

## Keywords

Censored demand forecasting, Tobit Kalman Filter, conformal prediction, inventory planning, family planning supply chains.

## Repository structure

| Folder | Contents |
| --- | --- |
| [`data/`](data/) | Product and variable metadata and the prepared empirical, synthetic, and diagnostic datasets in `data/tidy_data/`. |
| [`scripts/data_preperation/`](scripts/data_preperation/) | Cleaning, comment clustering, and preprocessing for the empirical data. |
| [`scripts/synthetic_data/`](scripts/synthetic_data/) | Generation of the synthetic demand and censoring scenarios. |
| [`scripts/forecast_models/`](scripts/forecast_models/) | TCKF estimation, forecasting, and model diagnostics. |
| [`scripts/inventory_simulation/`](scripts/inventory_simulation/) | Numerical and empirical inventory simulations and lead-time sensitivity. |
| [`scripts/analysis/`](scripts/analysis/) | Forecast evaluation, inventory analysis, and diagnostic summaries. |
| [`writings/`](writings/) | Quarto sources, tables, figures, and the compiled submission PDFs. |

## Reproduction workflow

### 1. Set up the project

Clone the repository, then run the remaining commands from its root.

```sh
git clone https://github.com/chamara7h/Estimating-censored-demand-in-family-planning-supply-chains.git
cd Estimating-censored-demand-in-family-planning-supply-chains
```

Install the R packages and create the output folders in an R session opened at the repository root. The repository has no package lockfile.

```r
install.packages(c(
  "tidyverse", "tsibble", "feasts", "forecast", "doParallel", "foreach",
  "fastDummies", "ggthemes", "patchwork", "Metrics", "SpecsVerification",
  "tsutils", "scales"
))

dir.create("data/predictions", recursive = TRUE, showWarnings = FALSE)
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)
```

Several scripts use parallel workers and can require substantial memory and disk space. On a machine with limited memory, reduce the number of workers in the relevant script before running it.

### 2. Start with the prepared data

| Input in `data/tidy_data/` | Used for |
| --- | --- |
| `synthetic_data.rds` | The numerical experiment and TCKF diagnostics. |
| `stock_tsb_filtered.rds` | The empirical experiment and lead-time sensitivity. |
| `synthetic_regime_separation_data.rds` | The diagnostic comparing full and partial censoring. |

### 3. Run TCKF and its diagnostics

Run the first two commands for the numerical and empirical TCKF forecasts. Run the remaining commands for the synthetic diagnostics.

```sh
Rscript scripts/forecast_models/01-proposed_model_pom.R
Rscript scripts/forecast_models/04-proposed_model_empirical_pom.R

Rscript scripts/forecast_models/05-proposed_model_diagnostic.R
Rscript scripts/analysis/12-tckf_signal_recovery_diagnostic.R
Rscript scripts/forecast_models/06-tckf_regime_separation_diagnostic.R
Rscript scripts/analysis/11-tckf_regime_separation_diagnostic.R
```

The forecast scripts write `tckf_pred_pom.rds`, `tckf_pred_pom_emp_new.rds`, `tckf_diag.rds`, and `tckf_regime_separation_diag.rds` to `data/predictions/`. The regime-separation analysis saves its summary files in `data/results/`.

### 4. Run the forecast comparisons and inventory simulations

The benchmark forecasts and their generation scripts are absent from this checkout. To reproduce the full comparisons, first obtain the original benchmark `*_prob*.rds` files and place them in `data/predictions/`. The two forecast-accuracy scripts list the required filenames in their `read_rds()` calls.

1. Run `Rscript scripts/analysis/01-numerical_experiement_forecast_accuracy.R` and `Rscript scripts/analysis/04-empirical_experiement_forecast_accuracy.R`. They save the forecast accuracy results and create `data/predictions/pred_master.rds` and `data/predictions/pred_master_emp.rds` for the inventory simulations.
2. Run the scripts below to generate the inventory results in `data/results/`. The manuscript sources use outputs from both empirical simulation scripts.

   ```sh
   Rscript scripts/inventory_simulation/05-inventory_sim_num.R
   Rscript scripts/inventory_simulation/06-inventory_sim_emp.R
   Rscript scripts/inventory_simulation/06-inventory_sim_emp_revised.R
   Rscript scripts/inventory_simulation/07-inventory_sim_emp_leadtime_sensitivity.R
   ```

3. Use `scripts/analysis/07-inventory_value_emp.R`, `08-inventory_leadtime_sensitivity_emp.R`, and `09-inventory_value_num.R` in `scripts/analysis/` to analyse the inventory outputs. The Quarto files also read the generated results directly for the paper figures and tables.

## Manuscript and e-companion

The compiled [main paper](writings/submission/Main-Paper.pdf) and [e-companion](writings/submission/E-companion.pdf) are available in `writings/submission/`.

To rebuild the PDFs from [`00-main.qmd`](writings/00-main.qmd) and [`00-e-companion.qmd`](writings/00-e-companion.qmd), generate the outputs above, install Quarto and a LaTeX distribution, and install the additional R packages `knitr`, `rmarkdown`, `kableExtra`, `showtext`, `viridis`, and `furrr`. Before rendering, update `project_path()` in both entry points to resolve paths one level above `writings/` (add `file.path("..", path)` to its candidates). In `writings/r_scripts/paper/`, update the old `writings/pom/tables` and `writings/pom/figs` references to `writings/tables` and `writings/figs` respectively. Then run the following from the repository root.

```sh
quarto render writings/00-main.qmd --to elsevier-pdf
quarto render writings/00-e-companion.qmd --to elsevier-pdf
```

## Citation

Hewage, H. C., Rostami-Tabar, B., Syntetos, A., Liberatore, F., & Milano, G. R. (2026). EXPRESS: Estimating censored demand in family planning supply chains: forecast accuracy, inventory implications, and public health outcomes. *Production and Operations Management*, *0*(ja). [https://doi.org/10.1177/10591478261511731](https://doi.org/10.1177/10591478261511731)

## Contact

For questions about the study or the files needed for reproduction, contact Harsha Chamara Hewage. [HalgamuweHewageHR@cardiff.ac.uk](mailto:HalgamuweHewageHR@cardiff.ac.uk)

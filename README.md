# Uncertainty estimation code for density surface models

Currently this is hard-coded for the fin whale example, filenames etc are set in each of the files `0` through `4`, which should be run in order. Once these have been run the `fig_*` files can be run.


## General file/folder layout


- Files:
  - `0_format_aux_data.R` - do some additional data manicure.
  - `1_model_and_data.R` - setup the data and fit the spatial model.
  - `2_prop_that_var.R` - propagate the detection function uncertainty through to the GAM using the Bravington et al. method.
  - `3_big_uncertainty.R` - simulate from the model over multiple "days" (more generally time periods) with different environmental covariates.
  - `4_process_uncertainty.R` - take the outputted simulated surfaces and calculate summaries (mean/variance per cell etc) overall and per year.
  - `fig_Nhat_timeseries.R` - generate timeseries for abundance.
  - `fig_gam_terms.R` - plot the smooth terms in the GAM.
  - `fig_maps.R` - plot maps of mean and standard error overall and per year.
  - `fig_plot_df.R` - plot the fitted detection function.
  - `fig_plot_raw_data.R` - plot the segments with bathymetry background.
  - `X_investigate.R` - investigate the 2008 high uncertainty.
  - `support_scripts/get_covar_bnds.R` - find the max/min of the covariates.
  - `support_scripts/get_varprop_model.R` - cut-down version of `dsm::dsm_varprop`.
  - `support_scripts/obs_exp.R` - compare observed vs. expected values for the model vs. data.
  - `support_scripts/prepare_preds.R` - take ROMS grids and format them for predicting.


- Input/output file folders:
  - `RData/` - holds intermediate model outputs as RData files, this includes fitted spatial models, variance propagated models etc. (empty to begin with).
  - `data/` - holds the "raw" data including the segment data, the fitted detection function object, average group size table, CCE grid.
  - `data/roms_grids` - ROMS grids should be placed here, one for each time period that needs to be predicted.
  - `figures/` - output figures from `fig_*.R` scripts go here (empty to begin with).
  - `out/` output grids will be stored here. One file per time period, name format is as for the ROMS files. Timeseries are stored in `Nhat_ests.csv`.

## Required R packages

Worth running the following before running anything above!

```r
install.packages(c("mgcv", "Distance", "dsm", "data.table", "lubridate",
                   "patchwork", "ggplot2", "mapdata", "tidyr", "dplyr",
                   "mrds", "RColorBrewer", "cmocean"))
```


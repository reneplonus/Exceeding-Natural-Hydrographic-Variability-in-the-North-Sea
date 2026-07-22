# Overview of the R analysis scripts in this folder

This project examines dipole-like thermocline structures in the German Bight using gridded environmental data and R-based analysis. It investigates how these features vary with transect scale, seasonal context, and wind forcing, and compares them against natural variability to better understand their significance in oceanography.

## Script overview

### corNatVar1_100.R

Purpose: estimates a natural-variability corridor for thermocline depth
across different transect-length scales.

What it does: - defines functions to calculate variability over moving
windows of different ranges - loads ScanFish grid data from the data
folder - computes variability metrics for a range of window sizes (1–100
km) - prepares data used for later plots of natural variability versus
dipole signals

Typical outputs: intermediate data objects such as range1_100.rda.

### dipol_var.R

Purpose: analyzes specific transects for dipole-like structures and
creates figures for those cases.

What it does: - loads processed output files from data/output_AE -
derives temperature differences and stratification indicators per
section - computes thermocline depth and dipole-related metrics -
produces plot objects for several cruise/transect combinations,
including figures such as diHE466\_\*.rda

This script is focused on the detailed inspection of selected dipole
examples.

### era5.R

Purpose: prepares ERA5 wind data from NetCDF files.

What it does: - reads wind component fields (u10 and v10) - converts
them into wind magnitude and direction - reshapes the data into a
tabular format - writes the results to an output text file
(era5_05_09.txt)

This is mainly a data-import and preprocessing script for wind
information.

### findDipoleInSF.R

Purpose: searches ScanFish data for dipole-like patterns by testing
thermocline-depth windows.

What it does: - loads ScanFish grid files - derives thermocline depth
and temperature differences - applies a rule-based detection routine for
possible dipoles - creates a diagnostic data frame that marks where
dipole-like patterns were found

Typical outputs: dipoCheck.rda and related intermediate objects.

### loadMatlab.r

Purpose: imports MATLAB-based gridded data into R.

What it does: - reads a MATLAB file using the R.matlab package -
reshapes the result into a long-format data frame - adds columns such as
latitude, longitude, depth, temperature, salinity, and density - saves
the converted object as an .rda file in the data/gridSFrolf folder

This script is a data-conversion utility rather than a full analysis
workflow.

### map_reference_area.R

Purpose: creates maps of the study region and reference area.

What it does: - loads OWF positions and ScanFish grid data - summarizes
sampling density across the region - overlays bathymetry and the study
area boundaries - adds offshore wind farm positions to the map -
produces map-based figures for the manuscript

Typical outputs: map-related figure objects in the figs folder.

### transectMapDipoles.R

Purpose: maps the spatial locations of selected transects for the main
dipole cases.

What it does: - loads output_AE data for cruises HE466, HE490, and
HE496 - identifies the relevant transect positions for each case - plots
their positions relative to the offshore wind farm locations - saves map
objects for the transect overview figures

Typical outputs: transectsHE466.rda, transectsHE490.rda, and
transectsHE496.rda.

### viz_dipoles.R

Purpose: visualizes dipole signals and compares them with natural
variability.

What it does: - loads dipole and natural-variability results from the
data folder - builds plots showing dipole strength across scales -
compares dipole variability with the background variability corridor -
creates probability and reference-area plots for the interpretation of
dipole events

This is one of the central visualization scripts in the project.

### wind_plot.R

Purpose: visualizes wind-related patterns and their relation to
dipole-induced mixing.

What it does: - loads wind summary data for different months and years -
creates raster maps of dipole-related mixing probability - produces
faceted plots for monthly and yearly wind patterns - saves figure
objects for use in the manuscript

Typical outputs: wind.rda and wind_july.rda in the figs folder.

## Notes

-   The scripts in this folder rely heavily on data files stored in the
    data/ directory.
-   Some scripts are still partly exploratory and contain commented-out
    code or older alternatives.

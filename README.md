# Replication of "Measuring Economic Growth From Outer Space"

Author: Gideon Grinberg • [Read the paper](https://github.com/gideongrinberg/measuring-growth-from-space/paper/paper.pdf?raw=true) (PDF)

Official GDP figures are unreliable in many countries, especially those with weak statistical agencies. Henderson, Storeygard, and Weil (2012) showed that satellite images of night lights can serve as an independent measure of economic activity. Errors in lights data have no reason to be related to errors in national accounts, so the two can be combined into a better estimate of true growth than either gives alone.

This repository reproduces the paper's main analyses from public data sources by rebuilding the country-level lights data from about 8 GB of raw satellite rasters and re-estimating the main models in R. The pipeline is orchestrated using the `{targets}` package.

## Replication Instructions

Running the pipeline requires R v4.6.2 and a computer with around 12gb of free storage. On my MacBook Pro with 24gb RAM and an M4 processor, the entire pipeline runs in around 10-15 minutes.

1.  **Install dependencies:** Download or clone the repository and open it in R or RStudio. If you do not have `renv` installed, run `install.packages("renv")`. Then run `renv::restore()` to install all the dependencies.

2.  **Dataverse Setup:** Generate an API token from Harvard Dataverse. Make a copy of `.Renviron.example`, rename it to `.Renviron`, and replace the placeholder value with your API token.

3.  **Download the DMSP-OLS image data:** Navigate to the [EOG website](https://eogdata.mines.edu/products/dmsp/#v4_dmsp_download) and login. You can manually download the files ending in `stable_lights.avg_vis.tif` and `cf_cvg.tif` for each satellite-year you wish to analyze and save them to `./data/raw/dmsp`, or you can automate the process by pasting the contents of `./source/download_dmsp.js` into the console of your browser and clicking the button that appears on the top right.

4.  **Run the analysis:** The project uses `targets` to manage its analysis pipeline. Run `targets::tar_make()` to execute the full pipeline. If you change any of the code or data, run `tar_make()`; only affected steps will re-run. You can access the output files in `data`, or using `targets::tar_read()`.

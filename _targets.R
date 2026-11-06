# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sf", "terra", "exactextractr", "httr2"),
  # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  controller = crew::crew_controller_local(workers = 3, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("source/")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
# list(
#   tar_target(gpw_zip, "data/raw/gl_gpwv3_ntlbnd_shp.zip", format = "file"),
#   tar_target(grump_zip, "data/raw/gl_grumpv1_area_ascii_30.zip", format = "file"),
#   tar_target(dmsp, list_dmsp_files("data/raw/dmsp"), cue = tar_cue("always")),
#
#   # Preprocess spatial data
#   tar_target(countries, load_country_borders(gpw_zip)),
#   tar_target(template_file, dmsp$lights[1], format = "file"),
#   tar_target(land_file, build_land_area(grump_zip, template_file), format = "file"),
#   tar_target(country_land_area, calculate_country_land_area(land_file, countries)),
#
#   # Process each satellite-year
#   tar_target(lights_file, dmsp$lights, pattern = map(dmsp), format = "file"),
#   tar_target(cloud_file, dmsp$cloud_cov, pattern = map(dmsp), format = "file"),
#   tar_target(stats,
#     process_dmsp_sat_year(lights_file, cloud_file, countries, land_file, country_land_area) |>
#       mutate(sat = dmsp$sat, year = dmsp$year, .before = 1),
#     pattern = map(dmsp, lights_file, cloud_file)
#   )
# )

tar_assign({
  # Preprocess data
  gpw_zip <- download_dataverse_file(
    "10.7910/DVN/BTCYL4",
    2.0,
    "gl_gpwv3_ntlbnd_shp.zip",
    "data/raw",
    "8cfc46980987a8b483d018a456a40055"
  ) |> tar_target(format = "file")
  grump_zip <- download_dataverse_file(
    "10.7910/DVN/USMCOR",
    2.0,
    "gl_grumpv1_area_ascii_30.zip",
    "data/raw/",
    "54e7f3fa2def04e3ac090b1f61f45e2c",
  ) |> tar_target(format = "file")
  
  dmsp <- list_dmsp_files("data/raw/dmsp") |> tar_target(cue = tar_cue(mode = "always"))
  template_tif <- dmsp$lights[dmsp$sat == "10" &
                                dmsp$year == 1992] |>
    tar_target(format = "file")
  countries <- load_country_borders(gpw_zip) |> tar_target()
  land_file <- build_land_weights_tif(grump_zip, template_tif) |>
    tar_target(format = "file")
  country_land_area <- calculate_country_land_area(land_file, countries) |>
    tar_target()
  
  # Process each satellite year into country-level stats
  sat_year_data <- process_dmsp_sat_year(dmsp, countries, land_file, country_land_area) |>
    tar_target(pattern = map(dmsp))
  lights_data <- sat_year_data |> tar_target()
})

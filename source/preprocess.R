ensure_unzipped <- function(zip) {
  dir <- tools::file_path_sans_ext(zip)
  if (!dir.exists(dir))
    unzip(zip, exdir = dir)
  dir
}

# List all of the satellite-years and corresponding image files
list_dmsp_files <- function(dir) {
  parse <- \(pattern, col) {
    tibble(path = list.files(dir, pattern, full.names = T)) |>
      transmute(sat = substr(basename(path), 2, 3),
                year = as.integer(substr(basename(path), 4, 7)),
                !!col := path)
  }
  
  inner_join(
    parse("\\.stable_lights\\.avg_vis\\.tif$", "lights"),
    parse("\\.cf_cvg\\.tif$", "cloud_cov")
  ) |>
    filter(year <= 2008)
}

# Load the GPWv3 country borders and make the geometry valid
load_country_borders <- function(gpw_zip) {
  sf_use_s2(FALSE)
  dir <- ensure_unzipped(gpw_zip)
  st_read(file.path(dir, "bnds/ntblnds.shp")) |>
    st_make_valid() |>
    group_by(Iso3v10) |>
    summarise() |>
    st_wrap_dateline() |>
    st_crop(
      xmin = -180,
      ymin = -65,
      xmax = 180,
      ymax = 66.56
    )
}

# Compute the land area per pixel using the GRUMP data and a template DMSP-OLS image
build_land_weights_tif <- function(grump_zip, template, out = "data/interim/land_area_km2.tif") {
  dir <- ensure_unzipped(grump_zip)
  grump <- rast(file.path(dir, "gluareag.asc"))
  crs(grump) <- "EPSG:4326"
  grump <- min(grump, cellSize(grump, unit = "km"))
  grump <- classify(grump, cbind(NA, 0))
  land <- resample(grump, rast(template), method = "bilinear")
  dir.create(dirname(out),
             recursive = TRUE,
             showWarnings = FALSE)
  writeRaster(land, out, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  out
}

# Compute the land area of each country by merging GRUMPS and GPW.
calculate_country_land_area <- function(land_file, countries) {
  sf_use_s2(FALSE)
  exact_extract(rast(land_file), countries, "sum", append_cols = "Iso3v10") |>
    rename(land_km2 = sum)
}

process_dmsp_sat_year <- function(dmsp,
                                  countries,
                                  land_file,
                                  country_land_area) {
  lights.raster <- rast(dmsp$lights)
  cloud_cov.raster <- rast(dmsp$cloud_cov)
  lights.raster <- mask(lights.raster,
                        cloud_cov.raster,
                        maskvalues = 0,
                        updatevalue = 255)
  
  # Compute the share of pixels (frac) and land area (weighted frac) at each DN value per country
  df <- exact_extract(
    lights.raster,
    countries,
    c("count", "frac", "weighted_frac"),
    weights = rast(land_file),
    append_cols = "Iso3v10",
    max_cells_in_memory = 5e7
  )
  
  # Summarize country-level stats.
  df |>
    pivot_longer(contains("frac"),
                 names_pattern = "^(weighted_frac|frac)_(\\d+)$",
                 names_to = c(".value", "dn")) |>
    rename(total_pixels = count) |>
    mutate(dn = as.integer(dn), across(c(frac, weighted_frac), \(x) coalesce(x, 0))) |>
    group_by(Iso3v10) |>
    summarize(
      missing_share = sum(weighted_frac[dn == 255]),
      mean_dn = sum((dn * weighted_frac)[dn <= 63]) / sum(weighted_frac[dn <= 63]),
      n_topcoded = first(total_pixels) * sum(frac[dn == 63]),
      n_unlit = first(total_pixels) * sum(frac[dn == 0])
    ) |>
    left_join(country_land_area) |>
    mutate(missing_km2 = missing_share * land_km2) |> 
    mutate(year = dmsp$year, sat = dmsp$sat, .before = 1)
}

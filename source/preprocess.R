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
                year = as.integer(substr(basename(path), 4, 7)),!!col := path)
  }
  
  inner_join(
    parse("\\.stable_lights\\.avg_vis\\.tif$", "lights"),
    parse("\\.cf_cvg\\.tif$", "cloud_free_cvg")
  ) |>
    filter(year <= 2008)
}

# Open a DMSP file and crop out the artic
make_template <- function(dmsp_file, n_arctic = 1016) {
  r <- rast(dmsp_file)
  rast(
    ext(xmin(r), xmax(r), ymin(r), ymax(r) - n_arctic * yres(r)),
    ncols = ncol(r),
    nrows = nrow(r) - n_arctic,
    crs = crs(r)
  )
}

# Compute the land area per pixel using the GRUMP data and a template DMSP-OLS image
build_land_weights_tif <- function(grump_zip, template, out = "data/interim/land_area_km2.tif") {
  dir <- ensure_unzipped(grump_zip)
  grump <- rast(file.path(dir, "gluareag.asc"))
  crs(grump) <- "EPSG:4326"
  
  tmpl <- make_template(template)
  grump <- min(grump, cellSize(grump, unit = "km"))
  grump <- classify(grump, cbind(NA, 0))
  land <- grump |>
    classify(cbind(NA, 0)) |>
    shift(dx = 1 / 240, dy = -1 / 240) |>
    crop(tmpl, snap = "near") |>
    extend(tmpl, fill = 0)
  
  stopifnot(nrow(land) == nrow(tmpl), ncol(land) == ncol(tmpl))
  ext(land) <- ext(tmpl)
  
  dir.create(dirname(out),
             recursive = TRUE,
             showWarnings = FALSE)
  writeRaster(land, out, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  out
}


build_countries_tif <- function(gpw_zip, template, land_file, out = "data/interim/countries.tif") {
  dir <- ensure_unzipped(gpw_zip)
  tmpl <- make_template(template)
  carto <- rasterize(vect(file.path(dir, "bnds/ntlbndscarto.shp")), tmpl, field = "Iso3v10", touches = F)
  base <- rasterize(vect(file.path(dir, "bnds/ntblnds.shp")), tmpl, touches = F)
  open_water <- !is.na(base) & rast(land_file) == 0
  r <- mask(carto, open_water, maskvalues = 1)
  writeRaster(r, out, datatype = "INT2U", overwrite = T)
  out
}

# Compute the land area of each country by merging GRUMPS and GPW.
calculate_country_land_area <- function(land_file, countries_file) {
  zonal(rast(land_file),
        rast(countries_file[1]),
        fun = "sum",
        na.rm = TRUE) |>
    setNames(c("Iso3v10", "land_km2")) |>
    mutate(Iso3v10 = as.character(Iso3v10))
}

gini_hist <- function(dn, n) {
  N  <- sum(n)
  mu <- sum(dn * n) / N
  if (mu == 0)
    return(NA_real_)
  o <- order(dn)
  dn <- dn[o]
  n <- n[o]
  below <- cumsum(n) - n
  above <- N - cumsum(n)
  sum(n * dn * (below - above)) / (N^2 * mu)
}
process_dmsp_sat_year <- function(dmsp, countries_file, land_file) {
  countries <- rast(countries_file)
  land <- rast(land_file)
  lights <- rast(dmsp$lights)
  cf_cov <- rast(dmsp$cloud_free_cvg)
  
  window(lights) <- ext(countries)
  window(cf_cov) <- ext(countries)
  sums <- zonal(c(lights * land, (cf_cov == 0) * land),
                countries,
                fun = "sum",
                na.rm = T) |>
    setNames(c("Iso3v10", "dn_area_sum", "missing_km2")) |>
    mutate(
      Iso3v10 = as.character(Iso3v10),
      year = dmsp$year,
      sat = dmsp$sat,
      .before = 1
    )
  
  hist <- crosstab(c(countries, lights), long = TRUE) |>
    setNames(c("Iso3v10", "dn", "n")) |>
    mutate(dn = as.integer(as.character(dn)))
  
  counts <- hist |>
    summarise(
      n_topcoded = sum(n[dn == 63]),
      n_unlit = sum(n[dn == 0]),
      gini = gini_hist(dn, n),
      hist = list(tibble(dn, n)),
      .by = Iso3v10
    )
  
  sums |>
    left_join(counts, by = "Iso3v10") |>
    mutate(
      Iso3v10 = as.character(Iso3v10),
      year = dmsp$year,
      sat = dmsp$sat,
      .before = 1
    )
  
}

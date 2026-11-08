tar_analysis <- tar_assign({
  df.clean <- clean_data(lights_data, gdp_df, electricity_df, country_land_area) |> 
    tar_target()
  main_models <- make_main_models(df.clean) |> 
    tar_target()
})

clean_data <- function(lights_data,
                       gdp_df,
                       electricity_df,
                       country_land_area) {
  drop_iso <- c("SCG", "MNE", "KSV", "HKG", "MAC", "GNQ", "BHR", "SGP")
  lights.df <- lights_data |>
    inner_join(country_land_area, by = "Iso3v10") |>
    rename(iso3 = Iso3v10) |>
    filter(!(iso3 %in% drop_iso)) |>
    # merge channel islands
    mutate(iso3 = case_match(iso3, c("JEY", "GGY") ~ "CHI", .default = iso3)) |>
    summarise(
      across(
        c(dn_area_sum, n_topcoded, n_unlit, land_km2, missing_km2),
        sum
      ),
      hist = list(bind_rows(hist) |> summarise(n = sum(n), .by = dn)),
      .by = c(iso3, year, sat)
    ) |>
    mutate(
      gini = map_dbl(hist, \(h) gini_hist(h$dn, h$n)),
      flag = land_km2 > 0 & missing_km2 / land_km2 > 0.05
    ) |>
    # merge all sats per year
    summarise(
      lights = mean(dn_area_sum),
      topcoded = mean(n_topcoded),
      unlit = mean(n_unlit),
      gini = mean(gini, na.rm = TRUE),
      area = first(land_km2),
      flag = any(flag),
      .by = c(iso3, year)
    ) |>
    filter(!flag) |>
    rename(country = iso3)
  
  df <- lights.df |>
    inner_join(gdp_df, by = c("country", "year")) |>
    inner_join(electricity_df |> rename(country = iso3),
               by = c("country", "year"))
}

make_main_models <- function(df.clean) {
  ln <<- \(x) log(x, base = exp(1))
  specs.main <- c(
    ln(gdp) ~ ln(lights / area) + i(year) | country,
    ln(gdp) ~ ln(lights / area) + ln(lights / area)^2 + i(year) |
      country,
    ln(gdp) ~ ln(lights / area) + ln(topcoded + 1) + ln(unlit + 1) + i(year) |
      country,
    ln(gdp) ~ ln(lights / area) + gini + i(year) | country
  )
  
  specs.elec <- c(
    ln(gdp) ~ ln(lights / area) + i(year) | country,
    ln(gdp) ~ ln(kwh) + i(year) | country,
    ln(gdp) ~ ln(lights / area) + ln(kwh) + i(year) | country
  )
  
  m.main <- lapply(specs.main, \(s) feols(s, data = df.clean, cluster = ~ country))
  m.elec <- lapply(specs.elec, \(s) feols(s, data = df.clean |> filter(!is.na(kwh)), cluster = ~ country))
  c(m.main, m.elec)
}
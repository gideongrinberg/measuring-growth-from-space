tar_analysis <- tar_assign({
  df.clean <- clean_data(lights_data, gdp_df, electricity_df, country_land_area) |>
    tar_target()
  main_models <- make_main_models(df.clean) |>
    tar_target()
  
  secondary_models <- make_secondary_models(df.clean) |>
    tar_target()
  
  rated_models <- make_rated_models(df.clean, wb_quality.df) |>
    tar_target()
  
  solved_model <- solve_model(df.clean, wb_quality.df) |> 
    tar_target()
  
  growth_rates <- calculate_growth_rates(df.clean, wb_quality.df, solved_model) |> 
    tar_target()
})

ln <- \(x) log(x, base = exp(1))

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
    left_join(gdp_df, by = c("country", "year")) |>
    left_join(electricity_df |> rename(country = iso3),
              by = c("country", "year"))
}

calculate_demeaned_lights <- function(df.clean) {
  residuals.df <- df.clean |>
    mutate(
      ln_gdp = log(gdp),
      ln_lights = log(lights / area),
      ln_unlit = ln(unlit + 1),
      ln_topcoded = ln(topcoded + 1)
    ) |>
    filter(is.finite(ln_gdp) & is.finite(ln_lights))
  
  residuals.df$resid_gdp <- resid(feols(ln_gdp ~ 1 |
                                          country + year, data = residuals.df))
  residuals.df$resid_lights <- resid(feols(ln_lights ~ 1 |
                                             country + year, data = residuals.df))
  
  residuals.df |>
    mutate(lights_up = pmax(resid_lights, 0),
           lights_down = abs(pmin(resid_lights, 0)))
  
}

calculate_long_diffs <- function(demeaned.df) {
  demeaned.df |>
    summarize(across(
      c(ln_gdp, ln_lights, ln_unlit, ln_topcoded),
      \(x) mean(x[year %in% 2005:2006]) - mean(x[year %in% 1992:1993]),
      .names = "d_{.col}"
    ), .by = country) |>
    filter(if_all(starts_with("d_"), is.finite))
}

make_main_models <- function(df.clean) {
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
  m.elec <- lapply(specs.elec,
                   \(s) feols(
                     s,
                     data = df.clean |> filter(!is.na(kwh)),
                     cluster = ~ country
                   ))
  c(m.main, m.elec)
}

make_secondary_models <- function(df.clean) {
  demeaned.df <- calculate_demeaned_lights(df.clean)
  long_diffs.df <- calculate_long_diffs(demeaned.df)
  
  models <- list(
    "Fixed Effects (Baseline)" = feols(
      ln(gdp) ~ ln(lights / area) + i(year) | country,
      data = df.clean,
      cluster = ~ country
    ),
    "Country Time Trend" = feols(
      ln(gdp) ~ ln(lights / area) + i(year) + i(country, year),
      data = df.clean,
      cluster = ~ country
    ),
    "Demeaned Plus/Minus" = feols(
      resid_gdp ~ lights_up + lights_down,
      data = demeaned.df,
      cluster = ~ country
    ),
    "Long Difference" = feols(
      d_ln_gdp ~ d_ln_lights,
      data = long_diffs.df,
      cluster = ~ country
    ),
    "Long Difference" = feols(
      d_ln_gdp ~ d_ln_lights + d_ln_unlit + d_ln_topcoded,
      data = long_diffs.df,
      cluster = ~ country
    )
  )
  
  models
}

make_ratings <- function(wb_quality.df) {
  wb_quality.df |>
    filter(!country %in% c("PRI", "SAU", "TUN")) |>
    transmute(country, good = as.integer(rating > 3))
}

make_rated_models <- function(df.clean, wb_quality.df) {
  ratings <- make_ratings(wb_quality.df)
  rated.df <- df.clean |>
    inner_join(ratings, by = "country") |>
    mutate(t = year - 1992)
  
  long_diffs.df <- df.clean |>
    calculate_demeaned_lights() |>
    calculate_long_diffs() |>
    inner_join(ratings, by = "country")
  
  a <- list(
    feols(
      ln(gdp) ~ ln(lights / area) + i(year) |
        country,
      data = rated.df,
      cluster = ~ country
    ),
    feols(
      ln(gdp) ~ ln(lights / area) + i(year) + i(country, t) |
        country,
      data = rated.df,
      cluster = ~ country
    ),
    feols(
      d_ln_gdp ~ d_ln_lights,
      data = long_diffs.df,
      cluster = ~ country
    )
  )
  
  b <- list(
    feols(
      ln(gdp) ~ ln(lights / area) + ln(lights / area):good + i(year) |
        country,
      data = rated.df,
      cluster = ~ country
    ),
    feols(
      ln(gdp) ~ ln(lights / area) + ln(lights / area):good + i(year) + i(country, t) |
        country,
      data = rated.df,
      cluster = ~ country
    ),
    feols(
      d_ln_gdp ~ good * d_ln_lights,
      data = long_diffs.df,
      cluster = ~ country
    )
  )
  
  sq <- map2(a, list(rated.df, rated.df, long_diffs.df), \(m, d) {
    d <- d[fixest::obs(m), ] |> mutate(e2 = resid(m)^2)
    feols(e2 ~ good, data = d, vcov = "iid")
  })
  
  list(a = a, b = b, sq = sq)
}

solve_model <- function(df.clean, wb_quality.df,
                        phi_g = c(1, 0.9, 0.8, 0.7, 0.6)) {
  
  ratings <- make_ratings(wb_quality.df)
  long_diffs.df <- df.clean |> 
    calculate_demeaned_lights() |> 
    calculate_long_diffs() |> 
    inner_join(ratings, by = "country")
  
  var_zg <- var(long_diffs.df$d_ln_gdp[long_diffs.df$good == 1])
  var_zb <- var(long_diffs.df$d_ln_gdp[long_diffs.df$good == 0])
  var_x <- var(long_diffs.df$d_ln_lights)
  cov_xz <- cov(long_diffs.df$d_ln_lights, long_diffs.df$d_ln_gdp)
  weight <- \(s2z, s2y, s2x) s2x * s2y / (s2z * var_x + s2x * s2y)
  
  tibble(phi_g = phi_g) |>
    mutate(
      s2y = phi_g * var_zg,
      s2zg = var_zg - s2y,
      s2zb = var_zb - s2y,
      phi_b = s2y / var_zb,
      beta = cov_xz / s2y,
      s2x = var_x - beta^2 * s2y,
      lambda_g = weight(s2zg, s2y, s2x),
      lambda_b = weight(s2zb, s2y, s2x)
    )
}

calculate_growth_rates <- function(df.clean, wb_quality.df, solved_model) {
  ratings <- make_ratings(wb_quality.df)
  lambda <- solved_model |> 
    filter(phi_g == 0.9) |> 
    pull(lambda_b)
  
  bad.df <- calculate_demeaned_lights(df.clean) |> 
    calculate_long_diffs() |> 
    inner_join(ratings) |> 
    filter(!good)
  
  fit <- feols(d_ln_gdp ~ d_ln_lights, data = bad.df, vcov = "hetero")
  bad.df$fitted_lights <- fitted(fit)
  bad.df |> 
    transmute(
      iso = country,
      wdi = d_ln_gdp,
      fitted_lights = fitted(fit),
      optimal = (1-lambda)*(fitted_lights) + lambda*(wdi),
      diff = optimal - wdi
    ) |> 
    mutate(across(where(is.numeric), ~ (exp(.x / 13) - 1)))|> 
    arrange(diff)
}  

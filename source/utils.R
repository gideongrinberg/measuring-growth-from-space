regtab <- function (models,
                    coefs = c(),
                    drop = "year",
                    hide_fe = TRUE,
                    fitstats = c(n = "Observations", wr2 = "Within R$^2$"),
                    count_fes = c(country = "Countries"),
                    counts_after = "n",
                    ...) {
  purrr::iwalk(
    count_fes,
    \(label, fe) fixest::fitstat_register(
      type = paste0("n_", fe),
      alias = label,
      fun = \(x) if (fe %in% names(x$fixef_sizes))
        x$fixef_sizes[[fe]]
      else
        NA
    )
  )
  
  types <- names(fitstats)
  types <- types |>
    append(paste0("n_", names(count_fes), recycle0 = T),
           after = match(counts_after, types, nomatch = length(types)))
  
  dict <- c(
    coefs,
    "country" = "country",
    "ln(gdp)" = "ln(GDP)",
    "ln(kwh)" = "ln(KWH)",
    "gini" = "Spatial Gini",
    fitstats
  )
  
  codes <- c("***" = 0.01,
             "**" = 0.05,
             "*" = 0.10)
  foot <- paste("Signif. codes:", paste0(names(codes), ": ", codes, collapse = ", "))
  
  rlang::exec(
    fixest::etable,
    models,
    drop = drop,
    drop.section = c("fixef", "slope"),
    dict = dict,
    tex = !interactive(),
    fitstat = types,
    style.tex = fixest::style.tex(
      "aer",
      signif.code = codes,
      tablefoot = T,
      tablefoot.value = "default"
    ),
    ...
  )
}

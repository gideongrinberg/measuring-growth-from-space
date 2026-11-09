regtab <- function(models,
                   coef_map = NULL,
                   col_names = NULL,
                   rows = NULL,
                   notes = "Clustered (country) standard errors in parentheses.",
                   ...) {
  if (!is.null(col_names))
    names(models) <- col_names
  else if (is.null(names(models)))
    names(models) <- paste0("(", seq_along(models), ")")
  
  gof_extra <- function(model, ...) {
    data.frame(
      countries = fixest::fitstat(model, "g")[[1]],
      r2_any = if (length(model$fixef_vars))
        fixest::fitstat(model, "wr2")[[1]]
      else
        fixest::fitstat(model, "r2")[[1]]
    )
  }
  
  gof_map <- tibble::tribble(
    ~ raw,
    ~ clean,
    ~ fmt,
    "nobs",
    "Observations",
    0,
    "countries",
    "Countries",
    0,
    "r2_any",
    "(Within) $R^2$",
    3
  )
  
  add <- NULL
  if (!is.null(rows)) {
    add <- data.frame(term = names(rows),
                      do.call(rbind, rows),
                      check.names = F)
    names(add) <- c("term", names(models))
    present <- intersect(names(coef_map), unlist(lapply(models, \(m) names(coef(
      m
    )))))
    attr(add, "position") <- 2 * length(unique(coef_map[present])) + seq_len(nrow(add))
  }
  
  defaults <- list(
    coef_map = coef_map,
    coef_omit = if (is.null(coef_map))
      "^year|^country|Intercept",
    gof_map = gof_map,
    gof_function = gof_extra,
    add_rows = add,
    stars = c(
      "*" = 0.1,
      "**" = 0.05,
      "***" = 0.01
    ),
    notes = notes,
    escape = F,
    output = "tinytable"
  )
  args <- utils::modifyList(Filter(Negate(is.null), defaults), list(...))
  
  rlang::exec(modelsummary::modelsummary, models, !!!args)
}
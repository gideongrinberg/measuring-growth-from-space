download_dataverse_file <- function(doi,
                                    version,
                                    filename,
                                    dest_dir,
                                    md5 = NULL,
                                    unzip = FALSE) {
  dest <- fs::path(dest_dir, filename)
  if (!is.null(md5) &&
      file.exists(dest) && unname(tools::md5sum(dest)) == md5)
    return(dest)
  
  key <- Sys.getenv("DATAVERSE_KEY")
  if (key == "")
    stop("Set DATAVERSE_KEY in .Renviron")
  
  meta <- request("https://dataverse.harvard.edu") |>
    req_url_path("api/datasets/:persistentId/versions", version, "files") |>
    req_url_query(persistentId = paste0("doi:", doi),
                  searchText = filename) |>
    req_perform() |>
    resp_body_json()
  
  stopifnot("File not found on Dataverse." = length(meta$data) == 1)
  stopifnot("Dataverse MD5 does not match." = meta$data[[1]]$dataFile$md5 == md5)
  file_id <- meta$data[[1]]$dataFile$id
  
  resp <- request("https://dataverse.harvard.edu") |>
    req_url_path("api/access/datafile", as.character(file_id)) |>
    req_headers("X-Dataverse-key" = key, .redact = "X-Dataverse-key") |>
    req_body_json(list(guestbookResponse = list(position = "Researcher"))) |>
    req_perform() |>
    resp_body_json()
  
  signed_url <- resp$data$signedUrl
  if (is.null(signed_url))
    stop("No signed URL found; inspect the response for its field name")
  
  dir.create(dirname(dest),
             recursive = TRUE,
             showWarnings = FALSE)
  request(signed_url) |> req_perform(path = dest)
  stopifnot("MD5 hash matches." = unname(tools::md5sum(dest)) == md5)
  
  if (unzip) {
    unzip(dest,
          exdir = tools::file_path_sans_ext(dest),
          overwrite = T)
  }
  
  dest
}

fetch_wdi_vintage <- function(series, version = "200910") {
  resp <- request("https://api.worldbank.org/v2/sources/57") |>
    req_url_path_append("country",
                        "all",
                        "series",
                        series,
                        "time",
                        "all",
                        "version",
                        version) |>
    req_url_query(format = "json", per_page = 20000) |>
    req_perform() |>
    resp_body_json()
  
  map(resp$source$data, \(r) {
    ids <- set_names(map_chr(r$variable, "id"), map_chr(r$variable, "concept"))
    tibble(
      iso3  = ids[["Country"]],
      year  = as.integer(sub("YR", "", ids[["Time"]])),
      value = r$value %||% NA_real_
    )
  }) |>
    list_rbind()
}

# Parse World Bank PDF and compute the data quality ratings
parse_wb2002 <- function(pdf) {
  cols <- c(
    "natacct",
    "bop",
    "exdebt",
    "eximprice",
    "popcensus",
    "vrsys",
    "agcensus",
    "cpi",
    "prodindex",
    "sdds"
  )
  pages <- pdftools::pdf_text(pdf)
  pages <- pages[str_detect(pages, "SCB Paper, Appendix A")]
  map(pages, \(p) {
    lines <- str_split_1(p, "\n")
    leg <- str_which(lines, "Best possible score")
    centers <- str_locate_all(lines[leg], "1991 or|BPM5 is 1|Actual /|Yes is 1")[[1]][, "start"] + 4
    body <- lines[(leg + 3):length(lines)]
    body <- body[str_detect(body, "^[A-Z][^*]*\\S\\s{2,}\\S")]
    map(body, \(l) {
      split <- str_locate(l, "\\s{2,}")[1, "start"]
      toks <- str_locate_all(l, "\\S+")[[1]]
      toks <- toks[toks[, "start"] > split, , drop = F]
      vals <- str_sub(l, toks[, "start"], toks[, "end"])
      col <- map_int((toks[, "start"] + toks[, "end"]) / 2, \(c) which.min(abs(centers - c)))
      tibble(country = str_trim(str_sub(l, 1, split)),
             col = cols[col],
             value = vals)
    }) |> list_rbind()
  }) |>
    list_rbind() |>
    pivot_wider(names_from = col, values_from = value)
}

score_wb2002 <- function(raw) {
  recent <- \(x) coalesce(x == "A" |
                            as.integer(str_sub(x, 1, 4)) >= 1991, F)
  yes <- \(x) coalesce(x == "Yes", F)
  raw |>
    transmute(
      country,
      natacct = recent(natacct),
      bop = coalesce(bop == "BPM5", F),
      exdebt = coalesce(exdebt %in% c("Actual", "Preliminary"), F),
      eximprice = yes(eximprice),
      popcensus = recent(popcensus),
      vrsys = yes(vrsys),
      agcensus = recent(agcensus),
      cpi = recent(cpi),
      prodindex = yes(prodindex),
      sdds = yes(sdds)
    ) |>
    mutate(rating = rowSums(pick(-country))) |>
    mutate(
      country = countrycode::countrycode(
        country,
        origin = "country.name",
        destination = "iso3c",
        custom_match = c("Yugoslavia, Fed. Rep." = "SCG")
      )
    )
}

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
    req_url_path_append("country", "all", "series", series,
                        "time", "all", "version", version) |>
    req_url_query(format = "json", per_page = 20000) |>
    req_perform() |>
    resp_body_json()
  
  map(resp$source$data, \(r) {
    ids <- set_names(map_chr(r$variable, "id"), map_chr(r$variable, "concept"))
    tibble(iso3  = ids[["Country"]],
           year  = as.integer(sub("YR", "", ids[["Time"]])),
           value = r$value %||% NA_real_)
  }) |>
    list_rbind()
}

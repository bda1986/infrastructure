library("utils")


urls <- c(
  "Kreise" = "https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/vg2500_12-31.utm32s.gpkg.zip",
  "Gemeinden" = "https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_1231/aktuell/vg250_12-31.utm32s.gpkg.ebenen.zip"
)

lapply(seq_along(urls), function(u) {
  url = urls[u]
  file = here::here("data-raw", names(url), basename(url))

  dir.create(path = dirname(file), showWarnings = FALSE, recursive = TRUE)
  
  print(paste("filename is", file))
  if (!file.exists(file)) {
    download.file(url = url, destfile = file)
    utils::unzip(
      zipfile = file,
      exdir = here::here("data-raw", names(url))
    )
  }
})

traffic <- list.files("data-raw/BASt_Traffic_Annual", pattern = ".csv$", full.names = TRUE) |> 
  readr::read_delim(delim = ";", id = "from_file") |> 
  dplyr::mutate(year = stringr::str_extract(from_file, "\\d{4}"))



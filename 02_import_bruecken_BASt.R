library("tidyverse")
library("sf")


# Input Files -------------------------------------------------------------

## BAST --------------------------------------------------------------------
# Read Excel files
bast <- list.files("./data-raw/BASt_Liste", pattern = ".xlsx$", full.names = TRUE)
bast <- bast |> 
  lapply(readxl::read_xlsx) |>
  dplyr::bind_rows() 

# Filter out missing coordinates
bast_na <- bast |> 
  dplyr::filter(is.na(X) | is.na(Y))

# Convert to sf
bast_sf <- bast |> 
  dplyr::filter(!is.na(X), !is.na(Y)) |> 
  sf::st_as_sf(coords = c("X", "Y"), crs = 25832) |> 
  dplyr::mutate(
    nichtunterverkehr = stringr::str_detect(teil_bw_stadium, "nicht unter Verkehr")
  )


## Merge with Geodata ------------------------------------------------------
gemeinde <- "./data-raw/Gemeinden/vg250_12-31.utm32s.gpkg.ebenen/vg250_ebenen_1231/DE_VG250.gpkg" |> 
  sf::st_read(layer = "vg250_vwg")

ggplot()+
  geom_sf(data = gemeinde)

bast_gemeinde <- gemeinde |> 
  sf::st_join(bast_sf)


agg <- bast_gemeinde |>
  sf::st_drop_geometry() |>
  dplyr::group_by(OBJID) |>
  dplyr::summarise(
    n = n(),
    mean_zn = mean(ZN, na.rm = TRUE),
    sd_zn = sd(ZN, na.rm = TRUE),
    #max_zn = max(ZN, na.rm = TRUE),
    #min_zn = min(ZN, na.rm = TRUE),
    nuv = sum(nichtunterverkehr, na.rm = TRUE)
  )

gemeinde2 <- gemeinde |> 
  dplyr::left_join(agg, by = "OBJID")

ggplot()+
  geom_sf(data = gemeinde2, aes(fill = mean_zn)) |> 
  scale_fill_viridis_b()

# Combine -----------------------------------------------------------------
source("./elections.R")

join <- gemeinde2 |> 
  dplyr::left_join(data_latest, by = c("ARS" = "ags"))




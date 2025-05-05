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

bast_bl <- bast_sf |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(kreis) |> 
  dplyr::summarise(n = n(),
                   mean_zn = mean(ZN, na.rm = TRUE),
                   sd_zn = sd(ZN, na.rm = TRUE),
                   max_zn = max(ZN, na.rm = TRUE),
                   min_zn = min(ZN, na.rm = TRUE))


## Kreise ------------------------------------------------------------------
url_kreise <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/vg2500_12-31.utm32s.gpkg.zip"
file_kreise <- here::here("data-raw/Kreise", basename(url_kreise))
gpkg_kreise <- here::here("data-raw/Kreise/vg2500_12-31.utm32s.gpkg/vg2500/DE_VG2500.gpkg")
if (!file.exists(file_kreise)) {
  download.file(url_kreise, file_kreise)
  utils::unzip(file_kreise, exdir = here::here("data-raw/Kreise"))
}

gpkg_kreise |>
  sf::st_layers()
kreise_sf <- gpkg_kreise |> 
  sf::st_read(layer = "vg2500_krs") |> 
  sf::st_transform(25832)



# Plot --------------------------------------------------------------------
ggplot()+
  geom_sf(data = kreise_sf)+
  geom_sf(data = bast_sf, size = 0.5)

join <- sf::st_join(kreise_sf, bast_sf) |> 
  dplyr::group_by(AGS) |> 
  dplyr::summarise(n = n(),
                   mean_zn = mean(ZN, na.rm = TRUE),
                   sd_zn = sd(ZN, na.rm = TRUE),
                   max_zn = max(ZN, na.rm = TRUE),
                   min_zn = min(ZN, na.rm = TRUE),
                   nuv = sum(nichtunterverkehr, na.rm = TRUE))


ggplot()+
  geom_sf(data = join, aes(fill = mean_zn))+
  scale_fill_viridis_b(name = "Mittlere\nZustandsnote")


ggplot()+
  geom_sf(data = join, aes(fill = sd_zn))+
  scale_fill_viridis_b(name ="StAbw\nZustandsnote")

ggplot()+
  geom_sf(data = join, aes(fill = as.factor(nuv)))+
  scale_fill_viridis_d(name ="Nicht\nunter\nVerkehr")

ggplot()+
  geom_sf(data = join, aes(fill = n))+
  scale_fill_viridis_c(name ="Anzahl\nBrücken")+
  theme(legend.position = "bottom")+
  labs(title = "Anzahl Brücken pro Landkreis",
       subtitle = "Datenquelle: BAST")


# Combine -----------------------------------------------------------------
source("./elections.R")

join <- join |> 
  dplyr::left_join(data_cty_latest, by = c("AGS"))


## Plots of individual party results ---------------------------------------
# AfD
ggplot()+
  geom_sf(data = join, aes(fill = afd))+
  scale_fill_gradient(low = "#ccccff", high = "#0000cc", name = "AfD\nStimmenanteil")

# CDU
ggplot()+
  geom_sf(data = join, aes(fill = cdu_csu))+
  scale_fill_gradient(low = "#cccccc", high = "#000000", name = "CDU/CSU\nStimmenanteil")

# SPD
ggplot()+
  geom_sf(data = join, aes(fill = spd))+
  scale_fill_gradient(low = "#ffcccc", high = "#cc0000", name = "SPD\nStimmenanteil")

# Gruene
ggplot()+
  geom_sf(data = join, aes(fill = gruene))+
  scale_fill_gradient(low = "#ccffcc", high = "#00cc00", name = "Grüne\nStimmenanteil")



## Plot strongest party ----------------------------------------------------
join <- join |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    strongest = c("AfD", "CDU/CSU", "SPD", "Gruene")[which.max(c(afd, cdu_csu, spd, gruene))]
  )

ggplot()+
  geom_sf(data = join, aes(fill = strongest, color = nuv>0, linewidth = nuv>0))+
  scale_fill_manual(
    values = c("AfD" = "#0000cc", "CDU/CSU" = "#202020", "SPD" = "#cc0000", "Gruene" = "#00cc00"),
    name = "Stärkste\nPartei"
  )+
  scale_color_manual(
    values = c("TRUE" = alpha("#cccccc", 1), "FALSE" = alpha("#000000", 0.5)),
    name = "Nicht\nunter\nVerkehr"
  )+
  scale_linewidth_manual(
    values = c("TRUE" = 0.5, "FALSE" = 0.1),
    name = "Nicht\nunter\nVerkehr"
  )


ggplot()+
  geom_sf(data = join, aes(fill = afd, color = nuv>0, linewidth = nuv>0))+
  scale_fill_gradient(low = "#ccccff", high = "#0000cc", name = "AfD\nStimmenanteil")+
  scale_color_manual(
    values = c("TRUE" = alpha("#ff0000", 1), "FALSE" = alpha("#000000", 0.5)),
    name = "Nicht\nunter\nVerkehr"
  )+
  scale_linewidth_manual(
    values = c("TRUE" = 0.5, "FALSE" = 0.1),
    name = "Nicht\nunter\nVerkehr"
  )

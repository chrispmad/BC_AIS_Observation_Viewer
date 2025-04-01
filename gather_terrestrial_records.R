library(readxl)
library(bcdata)
library(tidyverse)
library(bcinvadeR)

proj_wd = getwd()
terr_lan_root = "//SFP.IDIR.BCGOV/S140/S40203/Ecosystems/Conservation Science/Invasive Species/SPECIES/5_Incidental Observations/"
aq_lan_root = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/"

# Priority species list
pr_sp = read_excel(paste0(aq_lan_root,"/provincial_priority_is_list_final2019_dec18_2020.xlsx"))
names(pr_sp) = c("group","status","name","genus","species")

terr_pr_sp = pr_sp |>
  dplyr::filter(!group %in% c("Plant","Fish")) |>
  dplyr::mutate(name = stringr::str_to_title(name)) |>
  dplyr::mutate(name = dplyr::case_when(
    name == "White Nose Syndrome - Bats" ~ "White Nose Syndrome",
    name == "" ~ "White Nose Syndrome",
    name == "White Nose Syndrome - Bats" ~ "White Nose Syndrome",
  )) |>
  dplyr::filter(!name %in% c("Zebra & Quagga Mussels"))

# Terrestral incident reports
tir = read_excel(paste0(terr_lan_root,"Master Terrestrial Incidence Report Records.xlsx"), sheet = 'Terrestrial Reports')
# Filter for confirmed reports!
tir = tir |> dplyr::filter(ID_Confirmation == "Confirmed")

species_search_string = paste0(terr_pr_sp$genus," ",terr_pr_sp$species)

# BC data catalogue layer
terr_occs = bcdc_query_geodata('7d5a14c4-3b6e-4c15-980b-68ee68796dbe') |>
  filter(SCIENTIFIC_NAME %in% species_search_string) |>
  collect()

# Combine the data!
tir_sf = tir |>
  dplyr::mutate(lat = as.numeric(Latitude),
                lng = as.numeric(Longitude)) |>
  dplyr::filter(!is.na(lat),
                !is.na(lng)) |>
  sf::st_as_sf(coords = c("lng","lat"), crs = 4326) |>
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) |>
  dplyr::mutate(Species = dplyr::coalesce(Confirmed_Common_Name,Submitted_Common_Name)) |>
  dplyr::select(Date, Species)

terr_occs = terr_occs |>
  sf::st_transform(4326) |>
  dplyr::select(Date = OBSERVATION_DATE, Species = SPECIES_ENGLISH_NAME)

terr_obvs = dplyr::bind_rows(tir_sf, terr_occs)

saveRDS(terr_obvs, "data/terr_occ_dat.rds")

library(readxl)
library(bcdata)
library(tidyverse)
library(bcinvadeR)
library(tabulapdf)

proj_wd = getwd()
terr_lan_root = "//SFP.IDIR.BCGOV/S140/S40203/Ecosystems/Conservation Science/Invasive Species/SPECIES/5_Incidental Observations/"
aq_lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/"

# Read in priority species from updated PDF document.
pr_sp_tbls = tabulapdf::extract_tables(file = "https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/invasive-species/publications/provincial_priority_is_list.pdf")
# Combine table items from each page into a single table.
pr_sp_tbl = pr_sp_tbls[[2]] |>
  purrr::set_names(c("group","status","name","genus","species")) |>
  dplyr::bind_rows(
    pr_sp_tbls[3:length(pr_sp_tbls)] |>
      purrr::map( ~ {
        first_row = names(.x)
        .x |>
          purrr::set_names(c("group","status","name","genus","species")) |>
          dplyr::bind_rows(data.frame(group = first_row[1],
                                      status = first_row[2],
                                      name = first_row[3],
                                      genus = first_row[4],
                                      species = first_row[5]))
      })
  ) |>
  dplyr::filter(!is.na(status))

terr_pr_sp = pr_sp_tbl |>
  dplyr::filter(group %in% c("Insects and spiders","Amphibian","Birds","Reptile","Mammal"))

# # Priority species list
# pr_sp = read_excel(paste0(aq_lan_root,"/provincial_priority_is_list_final2019_dec18_2020.xlsx"))
# names(pr_sp) = c("group","status","name","genus","species")
# terr_pr_sp = pr_sp |> dplyr::filter(!group %in% c("Plant","Fish")) |>
#   dplyr::filter(!is.na(group))

# # Capitalize common names.
# terr_pr_sp$name = stringr::str_to_title(terr_pr_sp$name)
# terr_pr_sp = terr_pr_sp |>
#   dplyr::mutate(genus = ifelse(name == 'Roof/Black Rat',"Rattus",genus)) |>
#   dplyr::mutate(species = ifelse(name == 'Roof/Black Rat',"rattus",species)) |>
#   dplyr::mutate(name = ifelse(name == 'Roof/Black Rat','Black Rat',name)) |>
#   mutate(species = ifelse(name == 'Red-Eared Slider', 'scripta elegans', species)) |>
#   dplyr::mutate(name = ifelse(name == 'European Rabbits', 'European Rabbit', name)) |>
#   dplyr::mutate(name = ifelse(name == 'American Bull Frogs','American Bullfrog',name)) |>
#   dplyr::mutate(name = ifelse(name == 'Italian Wall Lizard','Common Wall Lizard',name)) |>
#   dplyr::mutate(name = ifelse(name == 'European Fire Ant & Other Invasive Ants','European Fire Ant',name))

# # Add additional species to the terrestrial priority species list.
# terr_pr_sp = dplyr::bind_rows(
#   terr_pr_sp,
#   data.frame(group = "Invertebrate", status = "Prevent", name = "Asian Needle Ant",
#              genus = 'Pachycondyla', species = 'chinensis')
# )

# Terrestrial incident reports
tir = read_excel(paste0(terr_lan_root,"Master Terrestrial Incidence Report Records.xlsx"), sheet = 'Terrestrial Reports') |>
  dplyr::filter(ID_Confirmation == 'Confirmed') |>
  dplyr::mutate(Confirmed_Common_Name = ifelse(is.na(Confirmed_Common_Name),Submitted_Common_Name,Confirmed_Common_Name)) |>
  dplyr::mutate(Submitted_Scientific_Name = stringr::str_to_sentence(Submitted_Scientific_Name),
                Confirmed_Scientific_Name = stringr::str_to_sentence(Confirmed_Scientific_Name)) |>
  dplyr::mutate(Date = openxlsx::convertToDate(Date))

tir_sf = tir |>
  dplyr::mutate(lat = as.numeric(Latitude),
                lng = as.numeric(Longitude)) |>
  dplyr::filter(!is.na(lat),
                !is.na(lng)) |>
  sf::st_as_sf(coords = c("lng","lat"), crs = 4326) |>
  dplyr::mutate(Date = as.Date(Date, "%Y-%m-%d")) |>
  dplyr::mutate(Species = dplyr::coalesce(Confirmed_Common_Name,Submitted_Common_Name)) |>
  dplyr::mutate(scientific_name = coalesce(Confirmed_Scientific_Name,Submitted_Scientific_Name)) |>
  dplyr::select(Date, Species, scientific_name) |>
  dplyr::filter(scientific_name %in% paste0(terr_pr_sp$genus," ",terr_pr_sp$species))

# Ensure we have the right names for these incident reports
tir_sf = tir_sf |>
  dplyr::left_join(terr_pr_sp |>
                     dplyr::mutate(scientific_name = paste0(genus,' ',species)) |>
                     dplyr::select(scientific_name, name)) |>
  dplyr::mutate(Species = name) |>
  dplyr::select(-name)

# Also query the BC Data catalogue for the terrestrial priority species!
species_search_string = paste0(terr_pr_sp$genus," ",terr_pr_sp$species)
species_search_string = species_search_string[species_search_string != "NA NA"]
species_search_string = unique(species_search_string)

# BC data catalogue layer
terr_occs = bcdata::bcdc_query_geodata('7d5a14c4-3b6e-4c15-980b-68ee68796dbe') |>
  bcdata::filter(SCIENTIFIC_NAME %in% species_search_string) |>
  bcdata::collect()

# Ensure we have the right names for the haul of data from the
# 'wildlife-species-inventory-incidental-observations-publicly-available'
# layer.
terr_occs = terr_occs |>
  sf::st_transform(4326) |>
  dplyr::select(Date = OBSERVATION_DATE,
                Species = SPECIES_ENGLISH_NAME,
                scientific_name = SCIENTIFIC_NAME) |>
  dplyr::left_join(terr_pr_sp |>
                     dplyr::mutate(scientific_name = paste0(genus,' ',species)) |>
                     dplyr::select(scientific_name, name)) |>
  dplyr::mutate(Species = name) |>
  dplyr::select(-name)

# Combine the data!
terr_obvs = dplyr::bind_rows(tir_sf, terr_occs)

# unique(terr_obvs$Species)[!unique(terr_obvs$Species) %in% unique(terr_pr_sp$name)]

saveRDS(terr_obvs, "data/terr_occ_dat.rds")

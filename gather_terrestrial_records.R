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
terr_pr_sp = pr_sp |> dplyr::filter(!group %in% c("Plant","Fish")) |>
  dplyr::filter(!is.na(group))

# Capitalize common names.
terr_pr_sp$name = stringr::str_to_title(terr_pr_sp$name)
terr_pr_sp = terr_pr_sp |>
  dplyr::mutate(genus = ifelse(name == 'Roof/Black Rat',"Rattus",genus)) |>
  dplyr::mutate(species = ifelse(name == 'Roof/Black Rat',"rattus",species)) |>
  dplyr::mutate(name = ifelse(name == 'Roof/Black Rat','Black Rat',name)) |>
  mutate(species = ifelse(name == 'Red-Eared Slider', 'scripta elegans', species)) |>
  dplyr::mutate(name = ifelse(name == 'European Rabbits', 'European Rabbit', name)) |>
  dplyr::mutate(name = ifelse(name == 'American Bull Frogs','American Bullfrog',name)) |>
  dplyr::mutate(name = ifelse(name == 'Italian Wall Lizard','Common Wall Lizard',name)) |>
  dplyr::mutate(name = ifelse(name == 'European Fire Ant & Other Invasive Ants','European Fire Ant',name))

# Add additional species to the terrestrial priority species list.
terr_pr_sp = dplyr::bind_rows(
  terr_pr_sp,
  data.frame(group = "Invertebrate", status = "Prevent", name = "Asian Needle Ant",
             genus = 'Pachycondyla', species = 'chinensis')
)

# Terrestrial incident reports
tir = read_excel(paste0(terr_lan_root,"Master Terrestrial Incidence Report Records.xlsx"), sheet = 'Terrestrial Reports') |>
  dplyr::filter(ID_Confirmation == 'Confirmed') |>
  dplyr::mutate(Confirmed_Common_Name = ifelse(is.na(Confirmed_Common_Name),Submitted_Common_Name,Confirmed_Common_Name)) |>
  dplyr::mutate(Submitted_Scientific_Name = stringr::str_to_sentence(Submitted_Scientific_Name),
                Confirmed_Scientific_Name = stringr::str_to_sentence(Confirmed_Scientific_Name))

# Also query the BC Data catalogue for the terrestrial priority species!
species_search_string = paste0(terr_pr_sp$genus," ",terr_pr_sp$species)
species_search_string = species_search_string[species_search_string != "NA NA"]
species_search_string = unique(species_search_string)

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
  dplyr::mutate(scientific_name = coalesce(Confirmed_Scientific_Name,Submitted_Scientific_Name)) |>
  dplyr::select(Date, Species, scientific_name)

terr_occs = terr_occs |>
  sf::st_transform(4326) |>
  dplyr::select(Date = OBSERVATION_DATE,
                Species = SPECIES_ENGLISH_NAME,
                scientific_name = SCIENTIFIC_NAME)

terr_obvs = dplyr::bind_rows(tir_sf, terr_occs)

# Use the scientific names to replace whichever common name has
# been reported in the incidence sheet; when scientific name doesn't match
# one from the terrestrial priority invasive species, keep the name
# in the Confirmed_Common_Name column.
terr_obvs = terr_obvs |>
  dplyr::left_join(terr_pr_sp |>
                     dplyr::mutate(scientific_name = paste0(genus," ",species)) |>
                     dplyr::select(scientific_name,
                                   name) |>
                     dplyr::distinct()) |>
  dplyr::mutate(common_name = ifelse(!is.na(name),name,Species))

# Also, filter for just priority species.
terr_obvs = terr_obvs |>
  dplyr::filter(scientific_name %in% paste0(terr_pr_sp$genus," ",terr_pr_sp$species))

terr_obvs = terr_obvs |>
  dplyr::mutate(Species = common_name) |>
  dplyr::select(Date, Species)

saveRDS(terr_obvs, "data/terr_occ_dat.rds")

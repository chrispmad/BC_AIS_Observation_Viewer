cat("This script updates the public-facing BC Invasives Dashboard (https://bcgov-env.shinyapps.io/BC_IS_Occ_Viewer/)\n")

invisible(library(tidyverse))

# Check to see if a previous attempt to automatically
# publish this app has failed; if so, don't attempt any of the stuff below,
# as that will very likely fail again.

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/"
proj_wd = getwd()
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")

setwd(here::here())

if(file.exists('app/www/Master Incidence Report Records.xlsx')) {
  file.remove('app/www/Master Incidence Report Records.xlsx')
  print('Removed old version of incident report excel document.')
}

tryCatch(
  file.copy(
    from = paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx"),
    to = 'app/www/Master Incidence Report Records.xlsx'
  ),
  error = function(e) {
    publishing_results$error_at = 'excel_copying_master_incident'
    publishing_results$error = TRUE
  }
)

occ_dat_res_b = sf::read_sf(paste0(lan_folder,"2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/AIS_occurrences_all_sources.gpkg"))

# Source data-gathering scripts.
source("gather_terrestrial_records.R")
source("preprocess_occ_recs.R")

# Remove records from occ_dat object where species are actually native to that
# part of the province ("Native Range") or have been eradicated ("Eradicated")
# or resampled and proven to be
# no longer found there ("Anecdotal")

# 1) Native Range
## Ecological Drainage Units
np_native_range = bcdc_query_geodata("eaubc-ecological-drainage-units") |>
  filter(ECO_DRAINAGE_UNIT %in% c("Alsek","North Coastal","Lewes","Nakina",
                                  "Teslin","Upper Stikine","Upper Liard","Taku",
                                  "Lower Liard","Lower Peace","Hay")) |>
  collect() |>
  sf::st_transform(4326)

walleye_native_range = bcdc_query_geodata("eaubc-ecological-drainage-units") |>
  filter(ECO_DRAINAGE_UNIT %in% c("Upper Liard","Lower Liard","Hay","Lower Peace")) |>
  collect() |>
  sf::st_transform(4326)

native_range_occs = dplyr::bind_rows(
  occ_dat_res_b |>
    dplyr::filter(Species == 'Northern pike') |>
    sf::st_filter(np_native_range),
  occ_dat_res_b |>
    dplyr::filter(Species == 'Walleye') |>
    sf::st_filter(walleye_native_range)
) |>
  dplyr::mutate(range_label = "native")

print(paste0("Looking for native ranges of Northern Pike and Walleye, we found ",nrow(native_range_occs)," rows that describe native occurences."))
print(paste0("Before removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))
# Drop records that are now in native range
occ_dat_res_b = occ_dat_res_b |>
  dplyr::anti_join(native_range_occs |>
                     sf::st_drop_geometry())
print(paste0("After removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

# 2) Eradicated
eradicated_occs = occ_dat_res_b |>
  # Only one eradicated record currently, for Northern Pike!
  dplyr::filter((Date == '2009' & Location == 'Haha Lake') |
                  (Date == '2006' & Location == 'Haha Lake'))

print(paste0("Looking for records that have been flagged as eradicated, we found ",nrow(eradicated_occs)," rows."))
print(paste0("Before removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))
# Drop records that are now in native range
occ_dat_res_b = occ_dat_res_b |>
  dplyr::anti_join(eradicated_occs |>
                     sf::st_drop_geometry())
print(paste0("After removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

# 3) Anecdotal
anecdotal_occs = occ_dat_res_b |>
  dplyr::filter((Location == 'SUMMIT LAKE') |
                  (Date == '2001-01-01' & Location == "AID LAKE"))

print(paste0("Looking for records that have been flagged as eradicated, we found ",nrow(eradicated_occs)," rows."))
print(paste0("Before removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))
# Drop records that are now in native range
occ_dat_res_b = occ_dat_res_b |>
  dplyr::anti_join(anecdotal_occs |>
                     sf::st_drop_geometry())
print(paste0("After removing those rows from the AIS data file, there are ",nrow(occ_dat_res_b)," rows."))

sf::write_sf(native_range_occs,"app/www/native_range_occs.gpkg")
sf::write_sf(eradicated_occs,"app/www/eradicated_occs.gpkg")
sf::write_sf(anecdotal_occs,"app/www/anecdotal_occs.gpkg")
sf::write_sf(occ_dat_res_b, "data/occ_dat.gpkg")

rsconnect::deployApp(
  appDir = 'app/',
  appTitle = 'BC_IS_Occ_Viewer',
  account = 'bcgov-env',
  forceUpdate = T
)

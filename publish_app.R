cat("This script updates the BC Invasives Dashboard (https://chrispmadsen.shinyapps.io/bc_invasives_dashboard/)\n")

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
sf::write_sf(occ_dat_res_b, "data/occ_dat.gpkg")

# Source data-gathering scripts.
source("gather_terrestrial_records.R")
source("preprocess_occ_recs.R")

rsconnect::deployApp(
  appDir = 'app/',
  appTitle = 'BC_IS_Occ_Viewer',
  account = 'bcgov-env',
  forceUpdate = T
)

library(sf)
library(bcdata)
library(leaflet)

# This file comes from the SAR_prioritization_model R project.
named_wbs = readRDS("../SAR_prioritization_model/data/named_lakes_and_rivers.rds")

named_streams = bcdc_query_geodata('freshwater-atlas-stream-network') |>
  filter(!is.na(GNIS_NAME)) |>
  collect() |>
  sf::st_zm()

# Since occurrence records can be shifted from waterbody locations by
# small errors in GPS / typing etc., buffer named waterbodies by a kilometer.
named_wbs = sf::st_buffer(named_wbs, dist = 1000)

# All occurrences of AIS from multiple sources
ais_occ = sf::read_sf("app/www/occ_dat.gpkg")

ais_occ_w_wbs = ais_occ |>
  sf::st_join(named_wbs)

# Filter for named waterbodies - these are likely waterbodies on public land / not privately owned.
ais_occ_w_wbs_pub = ais_occ_w_wbs |>
  dplyr::filter(!is.na(waterbody))

ais_occ_w_wbs_no_name = ais_occ_w_wbs |>
  dplyr::filter(is.na(waterbody))

ais_occ_w_wbs_no_name = ais_occ_w_wbs_no_name |>
  dplyr::filter(is.na(Location))

leaflet() |>
  addTiles() |>
  addCircleMarkers(
    data = ais_occ_w_wbs_no_name
  )



# Overlay aquatic and terrestrial occurrence records over the natural resource regions
# and jitter points and buffer them.
library(tidyverse)

# 1. Overlay
set.seed(9876)

ais_occ = sf::read_sf("data/occ_dat.gpkg") |>
  dplyr::filter(is.na(iNat_user))

terr_occ = readr::read_rds("data/terr_occ_dat.rds")

nr_regs = sf::read_sf("app/www/nr_regions.gpkg") |>
  dplyr::mutate(reg_name = stringr::str_remove(ORG_UNIT_NAME, " Natural.*")) |>
  dplyr::select(reg_name)

# Find which natural region each point is in.
ais_occ = ais_occ |>
  sf::st_join(nr_regs |> dplyr::select(reg_name))

terr_occ = terr_occ |>
  sf::st_join(nr_regs |> dplyr::select(reg_name))

# Create record ID
ais_occ = ais_occ |>
  dplyr::mutate(record_id = paste(
    snakecase::to_snake_case(Species),
    reg_name,
    "lng",
    round(sf::st_coordinates(geom)[,2],3),
    "lat",
    round(sf::st_coordinates(geom)[,1],3),
    sep = '_')) |>
  dplyr::select(Date, Species, reg_name, record_id)

terr_occ = terr_occ |>
  dplyr::mutate(record_id = paste(
    snakecase::to_snake_case(Species),
    reg_name,
    "lng",
    round(sf::st_coordinates(geometry)[,2],3),
    "lat",
    round(sf::st_coordinates(geometry)[,1],3),
    sep = '_'))

# Jitter
ais_occ_j = sf::st_jitter(ais_occ, amount = 0.01) |>
  sf::st_buffer(dist = 1000)

ais_occ_jsq = ais_occ_j |>
  dplyr::distinct() |>
  dplyr::mutate(unique_row_id = dplyr::row_number()) |>
  dplyr::group_by(Date, Species, reg_name, record_id, unique_row_id) |>
  dplyr::group_split() |>
  purrr::map( ~ {

    new_geom = .x |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_sf()

    new_geom |>
      cbind(.x |> sf::st_drop_geometry() |> dplyr::distinct()) |>
      dplyr::select(-unique_row_id)
  }, .progress = T) |>
  dplyr::bind_rows()

terr_occ_j = sf::st_jitter(terr_occ, amount = 0.01) |>
  sf::st_buffer(dist = 1000)

terr_occ_jsq = terr_occ_j |>
  dplyr::distinct() |>
  dplyr::mutate(unique_row_id = dplyr::row_number()) |>
  dplyr::group_by(Date, Species, reg_name, record_id, unique_row_id) |>
  dplyr::group_split() |>
  purrr::map( ~ {

    new_geom = .x |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_sf()

    new_geom |>
      cbind(.x |> sf::st_drop_geometry() |> dplyr::distinct()) |>
      dplyr::select(-unique_row_id)
  }, .progress = T) |>
  dplyr::bind_rows()

# leaflet() |>
#   addTiles() |>
#   addPolygons(
#     data = ais_occ_jsq
#   )

# Update the record id!
ais_occ_new_id = ais_occ_jsq |>
  dplyr::rowwise() |>
  dplyr::mutate(new_coord = paste0(str_remove(lapply(sf::st_coordinates(x)[1],round,4),"^-"), collapse = 'foo')) |>
  dplyr::mutate(record_id = paste0(stringr::str_to_lower(Species),"_",snakecase::to_snake_case(reg_name),"_",new_coord)) |>
  dplyr::select(-new_coord) |>
  dplyr::ungroup()

terr_occ_new_id = terr_occ_jsq |>
  rowwise() |>
  dplyr::mutate(new_coord = paste0(str_remove(lapply(sf::st_coordinates(x)[1],round,4),"^-"), collapse = 'foo')) |>
  dplyr::mutate(record_id = paste0(stringr::str_to_lower(Species),"_",snakecase::to_snake_case(reg_name),"_",new_coord)) |>
  dplyr::select(-new_coord) |>
  dplyr::ungroup()

# Make sure all dates are correct...
ais_occ_new_id_date = ais_occ_new_id |>
  dplyr::mutate(
    Date = dplyr::case_when(
      stringr::str_detect(Date,"^[0-9]{4}$") ~ lubridate::ymd(paste0(Date,"-01-01")),
      stringr::str_detect(Date,"[0-9]{4}-[0-9]{2}-[0-9]{2}") ~ lubridate::ymd(Date),
      T ~ NA
    ))

# Bring the squares back to centroid... for facilitating mapping.
ais_occ_new_id_date = ais_occ_new_id_date |>
  sf::st_centroid()

terr_occ_new_id = terr_occ_new_id |>
  sf::st_centroid()

# saveRDS(ais_occ_new_id_date,"app/www/occ_ais.rds")
# saveRDS(terr_occ_new_id,"app/www/occ_terr.rds")
sf::write_sf(ais_occ_new_id_date,"app/www/occ_ais.gpkg")
sf::write_sf(terr_occ_new_id,"app/www/occ_terr.gpkg")

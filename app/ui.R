library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(leafpop)
library(shinyWidgets)

csv_dl_but = downloadButton('csv_dl_but',label = "CSV",icon = shiny::icon('file-csv'))

geopackage_dl_but = downloadButton('gpkg_dl_but',"Geopackage",icon = shiny::icon("shapes"))

summary_bits = layout_column_wrap(
  width = 1/2,
  card(
    h3("Aquatic"),
    uiOutput("number_AIS")
  ),
  card(
    h3("Terrestrial"),
    uiOutput("number_terr")
  )
)

proviso_dialogue = modalDialog(
    title = span(shiny::icon('circle-exclamation'),"Data Disclaimer"),
    "Please note that this dashboard presents BC provincial occurrence data; these data may be incomplete, such that the lack of species occurrence data in an area does not necessarily imply its absence.",
    easyClose = T
  )

the_sidebar = bslib::sidebar(
  width = '25%',
  div(
    radioButtons(
      'species_or_region_select',
      "Select By...",
      choices = c("Species","Region"),
      selected = "Species",
      inline = T),
    style = 'margin-top:-2rem;'),
  uiOutput('sp_or_reg_filter'),
  sliderInput('date_filter',"Date Filter",
              value = c(lubridate::ymd("1939-01-01"),
                        lubridate::ymd(Sys.Date())),
              min = lubridate::ymd("1939-01-01"),
              max = lubridate::ymd(Sys.Date())),
  summary_bits,
  card(
    h5("Download Occurrence Data"),
    layout_column_wrap(
      width = 1/2,
      csv_dl_but,
      geopackage_dl_but
    )
  ),
  div(
    capture::capture(
      selector = 'body',
      filename = paste0("BC_IS_Occ_Viewer_",Sys.Date(),"_screenshot.png"),
      shiny::icon("camera"),
      "Screen Capture",
      class = 'btn-info'
    ),
    style = 'display:grid;'
  )
)

main_page = card(
  leafletOutput('leafmap')
)

tooltip = bslib::card(
  p("GOOP GOOP!", id = 'tooltip_content'),
  class = 'custom-tooltip', id = 'tooltip'
)

bcinv_theme = bslib::bs_theme(
  bootswatch = 'cerulean',
  # primary = "#168eb075",
  primary = "#a4c497",
  # secondary = "#48DAC6",
  # secondary = "#daabf5",
  secondary = "#a4c497",
  "font-size-base" = "0.8rem"
)

ui = bslib::page_navbar(
  shinyjs::useShinyjs(),
  shiny::includeCSS('www/styles/my_styles.css'),
  shiny::includeScript('www/js/tooltip_generator.js'),
  theme = bcinv_theme,
  title = h3('BC Invasive Species Occurrence Viewer',id = 'app_title',class='title-text'),
  bslib::nav_panel(
    title = "Map",
    layout_sidebar(
      main_page,
      sidebar = the_sidebar,
      proviso_dialogue,
      tooltip#,
      # mouse_mirage
    )
  )
)

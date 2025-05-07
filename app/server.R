server <- function(input, output, session) {
  # if(!stringr::str_detect(getwd(),"www/$")) setwd(paste0(getwd(),"/www/"))

  ais_occ = sf::read_sf("www/occ_ais.gpkg")

  terr_occ = sf::read_sf("www/occ_terr.gpkg")

  nr_regs = sf::read_sf("www/nr_regions.gpkg") |>
    dplyr::mutate(reg_name = stringr::str_remove(ORG_UNIT_NAME, " Natural.*")) |>
    dplyr::select(reg_name)

  ais_species_list = unique(ais_occ$Species)[order(unique(ais_occ$Species))]
  terr_species_list = unique(terr_occ$Species)[order(unique(terr_occ$Species))]

  output$sp_or_reg_filter = renderUI({
    req(!is.null(ais_occ))
    if(input$species_or_region_select == 'Species'){
      ui_output = tagList(
        # h3("Select Species",style="margin-bottom:-0.75rem;"),
        div(
          # width = 1/2,
          pickerInput(
            'aq_species_select',"Aquatic",
            choices = ais_species_list,
            selected = ais_species_list[1],
            multiple = TRUE,
            options = pickerOptions(
              container = "body",
              liveSearch = TRUE,
              selectedTextFormat = "count > 3",
              actionsBox = TRUE)
          ),
          pickerInput(
            'terr_species_select',"Terrestrial",
            choices = terr_species_list,
            selected = terr_species_list[1],
            multiple = TRUE,
            options = pickerOptions(
              container = "body",
              liveSearch = TRUE,
              selectedTextFormat = "count > 3",
              actionsBox = TRUE)
          ),
          style = 'margin-top:-2rem;'
        )
      )
    }
    if(input$species_or_region_select == 'Region'){
      ui_output = pickerInput('region_select',"Select Region",
                              choices = unique(nr_regs$reg_name),
                              selected = unique(nr_regs$reg_name)[1],
                              multiple = TRUE,
                              options = pickerOptions(
                                container = "body",
                                liveSearch = TRUE,
                                selectedTextFormat = "count > 3",
                                actionsBox = TRUE)
      )
    }
    ui_output
  })

  # Reactives that are basically just the species names inputs.
  aq_sp_f = reactive({
    input$aq_species_select
  })
  terr_sp_f = reactive({
    input$terr_species_select
  })

  # Filtered form of data.
  ais_f = reactive({
    req(!is.null(ais_occ))
    # req(nrow(ais_occ) > 0)

    if(input$species_or_region_select == 'Species'){
      # req(!is.null(input$aq_species_select))
      dat_output = ais_occ |>
        dplyr::filter(Species %in% input$aq_species_select)
    }
    if(input$species_or_region_select == 'Region'){
      req(!is.null(input$region_select))
      dat_output = ais_occ |>
        dplyr::filter(reg_name %in% input$region_select)
    }

    # Filter records based on date filter.
    dat_output = dat_output |>
      dplyr::filter(is.na(Date) | (Date >= input$date_filter[1] & Date <= input$date_filter[2]))

    # Order rows based on species and record ID column.
    dat_output = dat_output |>
      dplyr::arrange(Species,record_id)
    return(dat_output)
  })

  terr_f = reactive({
    req(!is.null(terr_occ))
    # req(nrow(terr_occ) > 0)

    if(input$species_or_region_select == 'Species'){
      req(!is.null(input$terr_species_select))
      dat_output = terr_occ |>
        dplyr::filter(Species %in% input$terr_species_select)
    }
    if(input$species_or_region_select == 'Region'){
      req(!is.null(input$region_select))
      dat_output = terr_occ |>
        dplyr::filter(reg_name %in% input$region_select)
    }

    # Filter records based on date filter.
    dat_output = dat_output |>
      # Apply date filter.
      dplyr::filter(is.na(Date) | (Date >= input$date_filter[1] & Date <= input$date_filter[2]))

    # Order rows based on species and record ID column.
    dat_output = dat_output |>
      dplyr::arrange(Species,record_id)
    return(dat_output)
  })

  # Generate row sum widget.
  output$number_AIS = renderUI({
    h5(paste0(nrow(ais_f()), " AIS occurrences in selection"))
  })

  output$number_terr = renderUI({
    h5(paste0(nrow(terr_f()), " Terrestrial invasive occurrences in selection"))
  })

  aq_marker_tbls = reactive({
    leafpop::popupTable(
      ais_f() |>
        sf::st_drop_geometry()
    )
  })

  terr_marker_tbls = reactive({
    leafpop::popupTable(
      terr_f() |>
        sf::st_drop_geometry()
    )
  })

  output$leafmap = renderLeaflet({
    leaflet() |>
      addTiles(group = 'OpenStreetMaps') |>
      addProviderTiles(providers$CartoDB, group = 'cartoDB') |>
      addLayersControl(position = 'bottomleft',
                       baseGroups = c('cartoDB','OpenStreetMaps'),
                       options = layersControlOptions(collapsed = FALSE)) |>
      leaflet::addScaleBar('bottomright') |>
      leaflet.extras::addResetMapButton() |>
      addPolygons(
        data = nr_regs,
        label = ~paste0(reg_name," Natural Resource Region"),
        color = 'black',
        weight = 1,
        fillColor = 'transparent'
      )
  })

  aq_species_pal = reactive({
    leaflet::colorFactor('Set3', domain = unique(ais_f()$Species))
  })

  terr_species_pal = reactive({
    leaflet::colorFactor('Spectral', domain = unique(terr_f()$Species))
  })

  # Catch state where we have absolutely 0 species to add to the map.
  blank_map = reactive({
    if(is.null(aq_sp_f()) & is.null(terr_sp_f())){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  observe({

    l = leafletProxy('leafmap') |>
      leaflet::removeControl('aq_legend') |>
      leaflet::removeControl('terr_legend') |>
      clearGroup('aq_occurrence_markers') |>
      clearGroup('terr_occurrence_markers')

    if(nrow(ais_f()) > 0){
      l = l |>
        addCircleMarkers(
          data = ais_f(),
          label = lapply(aq_marker_tbls(), htmltools::HTML),
          popup = lapply(aq_marker_tbls(), htmltools::HTML),
          fillColor = ~aq_species_pal()(Species),
          fillOpacity = 0.8,
          radius = 6,
          color = 'black',
          weight = 1,
          group = 'aq_occurrence_markers'
        ) |>
        addLegend(layerId = 'aq_legend',
                  title = "Aquatic",
                  pal = aq_species_pal(), values = ais_f()$Species)
    }
    if(nrow(terr_f()) > 0){
      l = l |>
        addCircleMarkers(
          data = terr_f(),
          label = lapply(terr_marker_tbls(), htmltools::HTML),
          popup = lapply(terr_marker_tbls(), htmltools::HTML),
          fillColor = ~terr_species_pal()(Species),
          fillOpacity = 0.8,
          radius = 6,
          color = 'black',
          weight = 1,
          group = 'terr_occurrence_markers'
        ) |>
        addLegend(layerId = 'terr_legend',
                  title = "Terrestrial",
                  position = 'topleft',
                  # className = 'scrollable leaflet legend',
                  pal = terr_species_pal(), values = terr_f()$Species)
    }
    # Run javascript code to add scroll bars and minimizing buttons to leaflet legends.
    shinyjs::runjs(code = paste0(readLines('www/js/legend_extension.js'), collapse = "\n"))
    return(l)
  })


  # # # Download Buttons # # #

  # CSV download.
  output$csv_dl_but = downloadHandler(
    filename = function() {
      paste0("BC_AIS_occurrence_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(x = dplyr::bind_rows(ais_f(),terr_f()), file = file)
    }
  )

  # Geopackage download.
  output$gpkg_dl_but = downloadHandler(
    filename = function() {
      paste0("BC_AIS_occurrence_data_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      sf::write_sf(dplyr::bind_rows(ais_f(),terr_f()), file)
    }
  )

}

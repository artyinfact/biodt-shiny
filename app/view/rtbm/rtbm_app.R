box::use(
  # HTML structure (htmltools)
  htmltools[
    a, div, em, HTML, img, p, renderTags,
    strong, tagList, tags, tagQuery, span, hr
  ],

  # Reactive components (Shiny)
  shiny[
    NS, uiOutput, actionButton, dateRangeInput,
    moduleServer, observe, observeEvent,
    reactive, req, renderUI, eventReactive,
    invalidateLater, reactiveVal, sliderInput,
    icon, textOutput, renderText, isTruthy,
    selectInput, isolate, withProgress, incProgress
  ],

  # UI enhancement
  shinyjs,

  # Bootstrap components
  bslib[
    card, card_body, card_footer, card_header
  ],

  # Leaflet map components
  leaflet[
    leaflet, leafletOutput, leafletProxy, renderLeaflet,
    addProviderTiles, addTiles, setView, addControl,
    addLegend, addRasterImage, clearControls,
    clearImages, colorNumeric, clearShapes, addMarkers,
    clearGroup, makeIcon, addCircleMarkers, addRectangles
  ],

  # Data manipulation
  dplyr[arrange, filter, mutate, pull, select, slice],
  stringr[str_detect],
  sf[st_crs, st_as_sf, st_sfc, st_sf, st_transform, st_coordinates],
  tibble[as_tibble],
  jsonlite[fromJSON],
  tidyjson[spread_all],
  lubridate[today, as_date, interval],
  httr2[request, req_perform, req_url_path, req_url_query, resp_status, resp_body_json],

  # File and data handling
  terra[rast],
  utils[download.file],
  stats[na.omit, quantile],
  raster[crs, projectRaster, raster, values, projection, xyFromCell],
  tools[file_ext],
  grDevices[colorRampPalette],
  memoise[memoise, forget],

  # UI widgets
  shinyWidgets[pickerInput, sliderTextInput, updateSliderTextInput],

  # Color palettes
  viridisLite[magma, inferno],

  # JavaScript interface
  htmlwidgets,
)

#' Load and prepare bird species information
#' @noRd
load_bird_species <- function() {
  bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
  bird_info <- fromJSON(bird_info_url)

  bird_info |>
    spread_all() |>
    as_tibble() |>
    select(-document.id) |>
    arrange(common_name) |>
    mutate(
      scientific_name = stringr::str_replace(
        string = scientific_name,
        pattern = " ",
        replacement = "_"
      )
    )
}

# Initialize bird species data
bird_spp_info <- load_bird_species()
species_choices <- bird_spp_info$common_name

#' Calculate seconds until midnight for cache refresh
#' @noRd
calc_secd_til_midnight <- function() {
  current_time <- Sys.time()
  midnight <- as.POSIXct(format(current_time + 86400, "%Y-%m-%d 00:00:00"))
  as.numeric(difftime(midnight, current_time, units = "secs"))
}

#' Real-time Bird Monitoring UI Module
#'
#' @param id The module ID
#' @param i18n Internationalization function
#'
#' @return A Shiny UI definition
#' @export
rtbm_app_ui <- function(id, i18n) {
  ns <- NS(id)

  # Create base layout using htmltools
  base_layout <- div(
    class = "rtbm-container container-fluid p-3",
    div(
      class = "row g-3",
      # Sidebar Toggle Button (visible on mobile)
      div(
        class = "col-12 d-md-none mb-2",
        actionButton(
          inputId = ns("toggleSidebar"),
          label = "Toggle Controls",
          icon = icon("bars"),
          class = "btn btn-secondary w-100"
        )
      ),
      # Control Panel Sidebar
      div(
        id = ns("sidebarCol"),
        class = "col-md-3 sidebar-column",
        div(
          class = "control-panel card h-100",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            span("Bird Observation Controls"),
            # Desktop toggle button (smaller version)
            actionButton(
              inputId = ns("collapseSidebar"),
              label = NULL,
              icon = icon("chevron-left"),
              class = "btn btn-sm btn-outline-secondary collapse-sidebar-btn"
            )
          ),
          div(
            class = "card-body overflow-auto",
            # Current date display
            uiOutput(ns("currentDateDisplay")),
            # Date range picker
            dateRangeInput(
              inputId = ns("dateRange"),
              label = "Select Date Range",
              start = Sys.Date() - 30,
              end = Sys.Date(),
              min = "2025-01-16",
              max = Sys.Date(),
              format = "yyyy-mm-dd",
              startview = "month",
              weekstart = 1,
              separator = " to ",
              language = "en"
            ),
            # Species picker
            pickerInput(
              inputId = ns("speciesPicker"),
              label = "Select Species",
              choices = bird_spp_info$common_name,
              selected = bird_spp_info$common_name[1],
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                size = 10,
                `actions-box` = TRUE
              )
            ),
            # Actions
            actionButton(
              inputId = ns("loadData"),
              label = "Load Data",
              icon = icon("refresh"),
              class = "btn btn-primary btn-block mt-3 mb-3 w-100"
            ),
            # Status message
            uiOutput(ns("statusMsg")),
            hr(),
            # Date slider with more height for better display
            div(
              class = "date-slider-container",
              uiOutput(ns("dateSlider"))
            )
          )
        )
      ),
      # Collapsed sidebar state - only shows expand button
      div(
        id = ns("collapsedSidebar"),
        class = "col-auto sidebar-collapsed d-none",
        actionButton(
          inputId = ns("expandSidebar"),
          label = NULL,
          icon = icon("chevron-right"),
          class = "btn btn-secondary expand-sidebar-btn"
        )
      ),
      # Map Column - will expand when sidebar collapses
      div(
        id = ns("mapCol"),
        class = "col-md-9 map-column",
        leafletOutput(ns("rasterMap"), height = "800px")
      )
    )
  )

  # Wrap everything in tagList with styles
  tagList(
    # Include CSS resources
    tags$head(
      # Import global styles first
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css"),
      # Then module-specific styles that only contain necessary overrides
      tags$link(rel = "stylesheet", type = "text/css", href = "view/rtbm/styles.css")
    ),
    shinyjs::useShinyjs(), # Initialize shinyjs
    base_layout
  )
}

#' Real-time Bird Monitoring Server Module
#'
#' @param id The module ID
#' @param tab_selected Reactive expression for tab selection
#'
#' @return A Shiny server function
#' @export
rtbm_app_server <- function(id, tab_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for data storage
    available_dates <- reactiveVal(NULL)
    frames_data <- reactiveVal(list())
    current_date <- reactiveVal(NULL)
    current_frame <- reactiveVal(1)

    # Animation controls
    animation_active <- reactiveVal(FALSE)

    # Sidebar state management
    sidebar_expanded <- reactiveVal(TRUE) # Start expanded

    # Handle sidebar toggle on mobile
    observeEvent(input$toggleSidebar, {
      shinyjs::toggleClass(id = "sidebarCol", class = "d-none")
    })

    # Handle sidebar collapse on desktop
    observeEvent(input$collapseSidebar, {
      sidebar_expanded(FALSE)
      shinyjs::addClass(id = "sidebarCol", class = "d-none")
      shinyjs::removeClass(id = "collapsedSidebar", class = "d-none")
      shinyjs::removeClass(id = "mapCol", class = "col-md-9")
      shinyjs::addClass(id = "mapCol", class = "col-md-11")
    })

    # Handle sidebar expand on desktop
    observeEvent(input$expandSidebar, {
      sidebar_expanded(TRUE)
      shinyjs::removeClass(id = "sidebarCol", class = "d-none")
      shinyjs::addClass(id = "collapsedSidebar", class = "d-none")
      shinyjs::removeClass(id = "mapCol", class = "col-md-11")
      shinyjs::addClass(id = "mapCol", class = "col-md-9")
    })

    # Create flags to track if legend and info card are already added
    legend_added <- reactiveVal(FALSE)
    info_card_added <- reactiveVal(FALSE)

    # Generate date sequence based on selected range
    date_sequence <- reactive({
      req(input$dateRange)
      start_date <- as_date(input$dateRange[1])
      end_date <- as_date(input$dateRange[2])

      # Create date sequence
      seq.Date(
        from = start_date,
        to = end_date,
        by = "day"
      )
    })

    # Display current date in header
    output$currentDateDisplay <- renderUI({
      if (!is.null(current_date())) {
        span(
          class = "current-date",
          format(current_date(), "%B %d, %Y")
        )
      }
    })

    # Date slider for manual navigation
    output$dateSlider <- renderUI({
      req(available_dates())
      dates <- available_dates()

      if (length(dates) == 0) {
        return(NULL)
      }

      # Get the number of dates to determine display format
      num_dates <- length(dates)

      # Always use the same format for the values (YYYY-MM-DD)
      # This prevents parsing issues elsewhere in the code
      date_values <- format(dates, "%Y-%m-%d")

      # Format labels based on screen size and number of dates
      # For small screens or many dates, use a highly abbreviated format
      date_labels <- if (num_dates > 10) {
        # For many dates, use the month-day format
        format(dates, "%m-%d")
      } else {
        # For fewer dates, use the full format
        format(dates, "%Y-%m-%d")
      }

      # Create named vector where names are display labels and values are full dates
      names(date_values) <- date_labels

      # Ensure first date is selected
      initial_date <- date_values[1]

      # For many dates, limit the number of values shown
      # This helps reduce visual clutter on small screens
      if (num_dates > 20) {
        # If we have many dates, sample a subset for display
        # but always include first and last dates
        sample_indices <- unique(c(
          1, # First date
          sort(sample(2:(num_dates - 1), min(8, num_dates - 2))), # Sampled middle dates
          num_dates # Last date
        ))

        # Only show these sampled dates - this reduces visual clutter
        # Full dates are still accessible via slider movement
        date_values <- date_values[sample_indices]
      }

      tagList(
        div(
          class = "date-slider-container",
          # Add clearer heading for the slider
          p(class = "mb-1", "Select Date:"),
          # Use sliderTextInput with improved formatting
          sliderTextInput(
            inputId = ns("date_slider"),
            label = NULL, # Remove default label since we added our own
            choices = date_values,
            selected = initial_date,
            grid = TRUE,
            force_edges = TRUE,
            hide_min_max = FALSE,
            animate = FALSE,
            width = "100%"
          )
        ),
        # Animation controls - only shown after data is loaded
        div(
          class = "animation-controls mt-3",
          # Play/Pause Button (centered)
          div(
            class = "d-flex justify-content-center",
            div(
              class = "animation-button-container",
              uiOutput(ns("playPauseButton"))
            )
          )
        )
      )
    })

    # Play/Pause button UI
    output$playPauseButton <- renderUI({
      if (animation_active()) {
        # Show pause button when animation is running
        actionButton(
          inputId = ns("pause_animation"),
          label = "Pause",
          icon = icon("pause"),
          class = "btn btn-secondary"
        )
      } else {
        # Show play button when animation is stopped
        actionButton(
          inputId = ns("play_animation"),
          label = "Play",
          icon = icon("play"),
          class = "btn btn-primary"
        )
      }
    })

    # Handle play button
    observeEvent(input$play_animation, {
      animation_active(TRUE)
    })

    # Handle pause button
    observeEvent(input$pause_animation, {
      animation_active(FALSE)
    })

    # Animation loop
    observe({
      # Only run when animation is active
      req(animation_active())
      req(available_dates())
      req(input$date_slider)

      # Get dates and current index
      all_dates <- available_dates()
      all_date_values <- format(all_dates, "%Y-%m-%d")

      # Find the full index from all available dates
      current_idx <- which(all_date_values == input$date_slider)

      # Check if current index is valid (might be empty if date not found)
      if (length(current_idx) == 0) {
        # If current date not found, start from the beginning
        current_idx <- 1
      }

      # Move to next date in the full dataset (or cycle back to beginning)
      next_idx <- if (current_idx < length(all_dates)) current_idx + 1 else 1
      next_date <- all_date_values[next_idx]

      # Update the date slider input (which will trigger map update)
      updateSliderTextInput(
        session = session,
        inputId = "date_slider",
        selected = next_date
      )

      # Fixed delay of 1 second between animations
      invalidateLater(1000)
    })

    # Handle manual date slider change
    observeEvent(input$date_slider, {
      req(input$date_slider, frames_data())
      frames <- frames_data()

      if (length(frames) == 0) {
        return()
      }

      # No need to parse the date - slider value is already in YYYY-MM-DD format
      selected_date_str <- input$date_slider

      # Find the frame index for the selected date
      frame_index <- which(names(frames) == selected_date_str)

      if (length(frame_index) == 1) {
        # Update current_frame and current_date
        current_frame(frame_index)
        current_date(as.Date(selected_date_str))

        # Update the map
        update_map_with_frame(frame_index)
      }
    })

    # Observe date slider changes and update map
    observeEvent(input$date_slider, {
      req(input$date_slider, frames_data())
      frames <- frames_data()

      if (length(frames) == 0) {
        return()
      }

      # Find the frame index for the selected date
      selected_date_str <- input$date_slider
      frame_index <- which(names(frames) == selected_date_str)

      if (length(frame_index) == 1) {
        # Update current_frame and current_date
        current_frame(frame_index)
        current_date(as.Date(selected_date_str))

        # Update the map
        update_map_with_frame(frame_index)
      }
    })

    # Get Finnish name for URL construction
    finnish_name <- reactive({
      req(input$speciesPicker)
      fn <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(finnish_name)
      print(fn)
      if (length(fn) == 0) {
        return(NULL)
      }
      fn
    })

    # Get photo URL for display
    photo_url <- reactive({
      req(input$speciesPicker)
      url <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(photo_url)
      if (length(url) == 0 || url == "") {
        return(NULL)
      }
      url
    })

    # 3) Common name (English) - directly from input
    common_name <- reactive({
      req(input$speciesPicker)
      input$speciesPicker
    })

    # 4) Scientific name
    scientific_name <- reactive({
      req(input$speciesPicker)
      sn <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(scientific_name)
      if (length(sn) == 0) {
        return(NULL)
      }
      sn
    })

    # 4b) Wiki link (for the hyperlink on the scientific name)
    wiki_link <- reactive({
      req(input$speciesPicker)
      wl <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(wiki_link)
      if (length(wl) == 0 || wl == "") {
        return(NULL)
      }
      wl
    })

    # 5) Song URL
    song_url <- reactive({
      req(input$speciesPicker)
      s_url <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(song_url)
      if (length(s_url) == 0 || s_url == "") {
        return(NULL)
      }
      s_url
    })

    # Cache bird data to prevent repeated API calls - Modified for batch loading
    cached_get_bird_data_batch <- memoise(function(date_range, species) {
      req(species)
      scientific <- bird_spp_info |>
        filter(common_name == species) |>
        pull(scientific_name)

      if (length(scientific) == 0) {
        return(list())
      }

      # Generate all dates in the range
      all_dates <- seq(from = date_range[1], to = date_range[2], by = "day")
      result_list <- list()

      # Show loading message
      output$statusMsg <- renderUI({
        div(
          class = "alert alert-info",
          role = "alert",
          paste0("Loading ", length(all_dates), " dates for ", species, "...")
        )
      })

      # Process all dates in batch
      withProgress(
        message = "Loading bird observation data...",
        value = 0,
        {
          for (i in seq_along(all_dates)) {
            date <- all_dates[i]
            formatted_date <- format(date, "%Y-%m-%d")

            # Update progress
            incProgress(
              1 / length(all_dates),
              detail = paste("Processing", formatted_date)
            )

            # Build URL for the tif file
            url_tif <- paste0(
              "https://2007581-webportal.a3s.fi/daily/",
              formatted_date, "/",
              scientific,
              "_occurrences.tif"
            )

            # Try to download and process
            tryCatch(
              {
                # Check if file exists
                resp <- request(url_tif) |> req_perform()
                if (resp$status != 200) next

                # Download the file
                tmp_file <- tempfile(fileext = ".tif")
                download_result <- download.file(url_tif, tmp_file, mode = "wb", quiet = TRUE)

                if (download_result != 0 || !file.exists(tmp_file) || file.size(tmp_file) == 0) {
                  next
                }

                # Process the raster
                r <- terra::rast(tmp_file)
                r[r == 0] <- NA

                # Check for invalid data
                if (any(terra::values(r) == -1, na.rm = TRUE)) {
                  next
                }

                # Add to results
                result_list[[formatted_date]] <- r
              },
              error = function(e) {
                # Skip on error and continue with other dates
              }
            )
          }
        }
      )

      # Update status message with results
      if (length(result_list) > 0) {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-success",
            role = "alert",
            paste0("Successfully loaded data for ", length(result_list), " dates.")
          )
        })
      } else {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-warning",
            role = "alert",
            "No data found for the selected date range and species."
          )
        })
      }

      result_list
    })

    # Reactive for batch loading data
    frames_data <- eventReactive(input$loadData, {
      req(input$dateRange, input$speciesPicker)

      # Reset animation when loading new data
      animation_active(FALSE)
      current_frame(1)

      # Get data for all dates in range
      data_batch <- cached_get_bird_data_batch(input$dateRange, input$speciesPicker)

      if (length(data_batch) == 0) {
        return(list())
      }

      # Process all loaded rasters into frames
      frames <- list()

      for (date_str in names(data_batch)) {
        r <- data_batch[[date_str]]

        # Convert to raster format for leaflet
        rt <- raster::raster(r)

        # Project if needed
        crs_rt <- raster::crs(rt)
        if (!is.na(crs_rt) && sf::st_crs(crs_rt) != sf::st_crs(3857)) {
          rt <- raster::projectRaster(rt, crs = sf::st_crs(3857))
        }

        # Get values for color scaling
        vals <- na.omit(values(rt))

        # Create frame data
        frames[[date_str]] <- list(
          date = as.Date(date_str),
          raster = rt,
          values = vals
        )
      }

      # Sort frames by date
      frame_dates <- as.Date(names(frames))
      frames <- frames[order(frame_dates)]

      # Update available dates for slider
      available_dates(as.Date(names(frames)))

      # Reset to first frame
      if (length(frames) > 0) {
        current_date(available_dates()[1])
      }

      return(frames)
    })

    # Function to process and update map with a specific frame
    update_map_with_frame <- function(frame_index) {
      frames <- frames_data()

      # Check if frames data exists and frame index is valid
      if (length(frames) == 0 || frame_index > length(frames) || frame_index < 1) {
        return(NULL)
      }

      frame_data <- frames[[frame_index]]

      if (is.null(frame_data) || is.null(frame_data$raster)) {
        return(NULL)
      }

      rt <- frame_data$raster
      vals <- frame_data$values

      # Use standard color palette as requested
      pal <- colorNumeric(
        palette = "magma",
        domain = global_min_max(),
        na.color = "#00000000"
      )

      # Start with a leaflet proxy that only clears the raster images
      # Use layerId for raster image to enable targeted updates
      map_update <- leafletProxy(ns("rasterMap")) |>
        clearImages() |>
        addRasterImage(
          rt,
          colors = pal,
          opacity = 0.8,
          project = FALSE,
          layerId = paste0("raster_", frame_index)
        )

      # Convert raster data to point markers for interactivity
      # Extract points with values above threshold for markers
      if (length(vals) > 0) {
        # Convert raster to points where values are above minimum threshold
        # This creates interactive markers at observation hotspots
        threshold <- quantile(vals, 0.75, na.rm = TRUE) # Use top 25% as hotspots
        significant_cells <- which(values(rt) > threshold & !is.na(values(rt)))

        if (length(significant_cells) > 0) {
          # Convert cell indices to spatial coordinates
          cell_coords <- xyFromCell(rt, significant_cells)
          cell_values <- values(rt)[significant_cells]

          # Create sf points
          points_df <- data.frame(
            x = cell_coords[, 1],
            y = cell_coords[, 2],
            value = cell_values,
            id = seq_along(cell_values)
          )

          # Convert to sf object
          points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = projection(rt))

          # Transform to WGS84 (EPSG:4326) for Leaflet compatibility
          points_sf <- st_transform(points_sf, 4326)

          # Clear previous markers but leave existing squares
          map_update <- map_update |>
            clearGroup("observation_markers")

          # No need to add new shapes - existing squares are already displayed
        }
      }

      # Update only the date display
      map_update <- map_update |>
        addControl(
          html = paste(
            "<div class='map-date-display'>",
            "<strong>Date:</strong> ", format(current_date(), "%Y-%m-%d"),
            "</div>"
          ),
          position = "bottomleft",
          layerId = "date-display" # Add layerId for easy replacement
        )

      # Only add the legend once when data is first loaded
      if (!legend_added()) {
        # Create info card with species info (only once)
        info_card_components <- isolate(create_info_card())
        info_card_html <- div(
          class = "leaflet-info-card",
          style = paste(
            "background-color: rgba(255, 255, 255, 0.9);",
            "padding: 15px;",
            "border-radius: 4px;",
            "border: 1px solid rgba(0,0,0,0.1);",
            "width: 220px;",
            "box-shadow: 0 2px 5px rgba(0,0,0,0.2);"
          ),
          info_card_components
        )

        # Convert htmltools tags to HTML for leaflet
        info_card_html_str <- renderTags(info_card_html)$html

        # Add the legend and info card
        map_update <- map_update |>
          addLegend(
            position = "bottomright",
            pal = pal,
            values = global_min_max(),
            title = "Observations",
            opacity = 0.8,
            layerId = "legend"
          ) |>
          addControl(
            html = info_card_html_str,
            position = "topleft",
            layerId = "info-card"
          )

        # Set flags to indicate legend and info card have been added
        legend_added(TRUE)
        info_card_added(TRUE)
      }

      # Execute the map update
      map_update
    }

    # Create the info card with species details
    create_info_card <- function() {
      list(
        # Photo section
        if (!is.null(photo_url())) {
          div(
            class = "info-card-photo mb-3",
            img(
              src = photo_url(),
              alt = paste("Photo of", common_name()),
              class = "img-fluid rounded"
            )
          )
        } else {
          div(
            class = "info-card-photo mb-3 text-muted",
            em("No image available")
          )
        },

        # Bird information section
        div(
          class = "info-card-details",
          # Common name
          if (!is.null(common_name())) {
            div(
              class = "mb-2",
              strong("Common Name: "),
              span(common_name())
            )
          },

          # Scientific name with optional wiki link
          if (!is.null(scientific_name())) {
            div(
              class = "mb-2 scientific-name",
              if (!is.null(wiki_link())) {
                a(
                  href = wiki_link(),
                  target = "_blank",
                  rel = "noopener",
                  em(scientific_name()),
                  class = "text-decoration-none"
                )
              } else {
                em(scientific_name())
              }
            )
          },

          # Audio player
          if (!is.null(song_url())) {
            div(
              class = "mt-3",
              tags$audio(
                class = "w-100",
                controls = NA,
                tags$source(
                  src = song_url(),
                  type = "audio/mpeg"
                ),
                "Your browser does not support the audio element."
              )
            )
          }
        )
      )
    }

    # Calculate global min/max values for legend scale
    global_min_max <- reactive({
      req(frames_data())
      frames <- frames_data()
      if (length(frames) == 0) {
        return(c(0, 1))
      }

      # Collect all values across all frames
      all_values <- numeric(0)
      for (frame in frames) {
        if (!is.null(frame$values) && length(frame$values) > 0) {
          all_values <- c(all_values, frame$values)
        }
      }

      if (length(all_values) == 0) {
        return(c(0, 1))
      }

      # Return min and max values
      c(min(all_values, na.rm = TRUE), max(all_values, na.rm = TRUE))
    })

    # Base leaflet map
    output$rasterMap <- renderLeaflet({
      leaflet() |>
        addProviderTiles("CartoDB.Positron", layerId = "base_map") |>
        setView(lng = 25, lat = 65.5, zoom = 5) |>
        # Add empty controls that will be filled later
        addControl(
          html = "<div id='hover-info' class='map-hover-display d-none'></div>",
          position = "topright",
          layerId = "hover-info-control"
        )
    })

    # Initial status message prompting user action
    output$statusMsg <- renderUI({
      div(
        class = "alert alert-info",
        role = "alert",
        "Select a date range and species, then click 'Load Data' to view the distribution map. Use the date slider to navigate through time."
      )
    })

    # Observe button click to trigger initial data load
    observeEvent(input$loadData, {
      raster_data <- frames_data()
      if (length(raster_data) > 0) {
        update_map_with_frame(1)
      }
    })

    # Track map view changes
    observeEvent(input$rasterMap_zoom, {
      # Store current zoom level
      current_zoom <- input$rasterMap_zoom
      print(paste("Zoom level changed to:", current_zoom))

      # Just track zoom level but don't try to manipulate markers directly
      # Let the CSS solution handle consistent appearance
    })

    # Track map bounds changes
    observeEvent(input$rasterMap_bounds, {
      # Store current bounds
      current_bounds <- input$rasterMap_bounds
      print(paste("Map bounds changed"))

      # This could be used to load more detailed data for the visible area
      # or adjust data display based on the current view
    })

    # Handle marker clicks
    observeEvent(input$rasterMap_marker_click, {
      click_data <- input$rasterMap_marker_click
      print(paste("Marker clicked:", click_data$id))

      # Extract marker ID from the layerId
      marker_id <- sub("marker_", "", click_data$id)

      # Additional click handling can be implemented here
      # For example, showing detailed information about the specific observation
    })

    # Handle map shape hover events
    observeEvent(input$rasterMap_shape_mouseover, {
      hover_data <- input$rasterMap_shape_mouseover

      if (!is.null(hover_data)) {
        # Show hover info with the observation value
        intensity_value <- if (is.numeric(hover_data$value)) {
          round(hover_data$value, 2)
        } else {
          "N/A" # Fallback for non-numeric values
        }

        leafletProxy(ns("rasterMap")) |>
          addControl(
            html = paste0(
              "<div class='map-hover-display'>",
              "<strong>Intensity:</strong> ", intensity_value,
              "</div>"
            ),
            position = "topright",
            layerId = "hover-info-control"
          )
      }
    })

    observeEvent(input$rasterMap_shape_mouseout, {
      # Hide or clear hover information
      leafletProxy(ns("rasterMap")) |>
        addControl(
          html = "<div class='map-hover-display d-none'></div>",
          position = "topright",
          layerId = "hover-info-control"
        )
    })
  })
}

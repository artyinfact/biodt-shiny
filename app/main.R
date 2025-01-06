box::use(
  shiny,
  bslib[nav_select, bs_theme, page_navbar, nav_menu, nav_spacer, nav_item, nav_panel, page],
  shinyjs[useShinyjs],
  waiter[useWaiter, useHostess, waiterShowOnLoad, waiter_hide, spin_loaders],
  cicerone[use_cicerone],
  stringi[stri_rand_strings],
  htmltools[includeScript],
  config,
  shiny.i18n[Translator, usei18n, update_lang],
  shiny.router[router_ui, router_server, route, route_link],
)

box::use(
  app/view/info[mod_info_ui, mod_info_server],
  app/view/acknowledgements[mod_acknowledgements_ui],
  app/view/honeybee/honeybee_main[honeybee_ui, honeybee_server],
  app/view/grassland/grassland_main[grassland_main_ui, grassland_main_server],
  app/view/ces/ces_main[ces_ui, ces_server],
  app/view/disease_outbreaks/disease_outbreaks_main[
    disease_outbreaks_main_ui,
    disease_outbreaks_main_server
  ],
  app/view/cwr/cwr_main[mod_cwr_server, mod_cwr_ui],
  app/view/page404[page404_ui]
)

shiny$enableBookmarking("server")
# App theme ----
#' @export
biodt_theme <- bs_theme(
  version = 5,
  primary = "#A86200",
  secondary = "#414f2f",
  info = "#DDA15E",
  warning = "#6E3E18",
  success = "#f8f2e4",
  bg = "#fff",
  fg = "#414f2f",
  bootswatch = "bootstrap"
)

env_active <- Sys.getenv("R_CONFIG_ACTIVE")

i18n <- Translator$new(translation_json_path = "app/translations/translations.json")
i18n$set_translation_language("en")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  page(
  #shiny$bootstrapPage(
    theme = biodt_theme,
    # Head ----
    shiny$tags$head(
      shiny$tags$link(rel = "shortcut icon", href = "static/favicon.ico"),
      useShinyjs(),
      useWaiter(),
      useHostess(),
      use_cicerone(),
      usei18n(i18n),
      includeScript("app/js/tab-index.js") 
    ),
    waiterShowOnLoad(
      html = spin_loaders(
        id = 19,
        color = "#414f2f"
      ),
      color = "rgba(256,256,256,0.9)"
    ),
    includeScript("app/js/popover.js"),
    shiny::a("Acknowledgements", href = route_link("acknowledgements")),
    shiny$tags$nav(
      class = "navbar navbar-default navbar-static-top",
      role = "navigation",
      shiny$tags$div(
        class = "container-fluid",
        shiny$tags$div(
          class = "navbar-header",
          shiny$actionLink(
            inputId = ns("biodt_logo"),
            shiny$img(
              src = "static/logo.svg",
              height = "70px",
              style = "padding-right: 20px",
              alt = "Biodiversity Digital Twin",
            ),
          ),
        ),
        shiny$tags$div(
          class = "collapse navbar-collapse",
          shiny$tags$ul(
            class="navbar-nav me-auto mb-2 mb-lg-0",
            shiny$tags$li(
              class="nav-item dropdown",
              shiny::a(
                class="nav-link dropdown-toggle",
                href="#",
                role="button",
                "Digital Twin"
              ),
              shiny$tags$ul(
                class="dropdown-menu",
                shiny$tags$li(
                  class="dropdown-item",
                  shiny::a("Info", href = route_link("/")),
                ),
              ),
              shiny$tags$ul(
                class="dropdown-menu",
                shiny$tags$li(
                  class="dropdown-item",
                  shiny::a("Ces", href = route_link("ces")),
                ),
              ),
              shiny$tags$ul(
                class="dropdown-menu",
                shiny$tags$li(
                  class="dropdown-item",
                  shiny::a("Grassland", href = route_link("grassland")),
                ),
              ),
            )
          )
        )
      )
    ),
    # Body ----
    # Main navbar----
    page_navbar(
      window_title = "BioDT",
      id = ns("navbar"),
      theme = biodt_theme,
      bg = "#fff",
      fillable = TRUE,
      # must be true
      collapsible = TRUE,
      fluid = TRUE,
      # Digital Twins - main menu item ----
       nav_menu(
         title = i18n$translate("Digital Twin"),
         align = "left",
         icon = shiny$icon("people-group", `aria-hidden` = "true"),
           nav_item(
             ## Species response to environment - menu subitem ----
             shiny$tags$div(
               class = "p-2",
               shiny$icon("temperature-arrow-up", `aria-hidden` = "true"),
               shiny$tags$strong(i18n$translate("Species response to environmental change"))
             )
           ),
         if (env_active == "dev") {
           nav_panel(
             class = "p-0",
             title = i18n$translate("Grassland Dynamics"),
             value = shiny::a("Grassland", href = route_link("grassland")),                     
           )
         },
           nav_panel(
             class = "p-0",
             title = i18n$translate("Cultural Ecosystem Services"),
             value = "CES",
             shiny::a("ces", href = route_link("ces"))
           ),
         if (env_active == "dev") {
           nav_item(
             ## Species response to environment - menu subitem ----
             shiny$tags$div(
               class = "p-2",
               shiny$icon("temperature-arrow-up"),
               shiny$tags$strong(i18n$translate("Species response to environmental change"))
             )
           )
         },
         if (env_active == "dev") {
           nav_panel(
             class = "p-0",
             title = i18n$translate("Crop wild relatives and genetic resources for food security"),
             mod_cwr_ui(
               ns("cwr_main"),
               i18n
             )
           )
         },
         ## Species interactions (themselves, human) - menu subitem ----
         nav_item(
           shiny$div(
             class = "p-2",
             shiny$div(
               shiny$icon("bugs", `aria-hidden` = "true"),
               shiny$strong(i18n$translate("Species interactions with each other and with humans")),
               style = "width: 450px"
             ),
           )
         ),
         nav_panel(
           title = i18n$translate("Honeybee"),
           value = "Honeybee",
           class = "p-0",
           shiny::a("honeybee", href = route_link("honeybee"))
         ),
         if (env_active == "dev") {
           nav_panel(
             title = i18n$translate("Disease Outbreaks"),
             class = "p-0",
             disease_outbreaks_main_ui(ns("disease_outbreaks_main"), i18n)
           )
         },
       ),
       nav_spacer(),
       ## Acknowledgements - main menu item ----
       nav_panel(
         title = i18n$translate("Acknowledgements"),
         value = "acknowledgements",
         icon = shiny$icon("users-gear", `aria-hidden` = "true"),
         class = "container-fluid index-info",
         shiny::a("acknowledgements", href = route_link("acknowledgements"))        
       ),
       if (env_active == "dev") {
         nav_item(
           shiny$bookmarkButton()
         )
       },
       if (env_active == "dev") {
         nav_item(
           shiny$selectInput(
             ns("selected_language"),
             shiny$span(), # shiny$p(i18n$translate("Language:")),
             choices = i18n$get_languages(),
             selected = i18n$get_key_translation(),
             width = "75px"
           )
         )
       }
    ),
    router_ui(
      route("/", mod_info_ui(ns("info"), i18n)),
      route("grassland", grassland_main_ui(
          ns("grassland_main"),
          i18n
        )
      ),
      route("ces", ces_ui(
          ns("ces_main")
        )
      ),
      route("honeybee", honeybee_ui(
          ns("honeybee_main"),
          theme = biodt_theme,
          i18n
        )
      ),
      route("acknowledgements",
        mod_acknowledgements_ui("info")
      ),
      page_404 = page404_ui(ns("page404"))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    router_server("/")

    ns <- session$ns

    base_path <- Sys.getenv("BASE_PATH")

    session_dir <- file.path(
      paste0(base_path, "shared"),
      paste0(
        Sys.time() |> format(format = "%Y-%m-%d_%H-%M-%S"),
        "_",
        stri_rand_strings(1, 8)
      )
    )

    r <- shiny$reactiveValues(
      biodt_theme = biodt_theme
    )
    
    # Language change support see shiny.i18n
    shiny$observeEvent(input$selected_language, {
      update_lang(input$selected_language)
    })
    
    # Info page ----
    mod_info_server(
      "info",
      main_session = session
    )
    # CWR pDT ----
    mod_cwr_server(
      "cwr_main",
      i18n
    )
    
    # Honeybee pDT ----
    honeybee_server(
      "honeybee_main",
      session_dir
    )
    # Grassland pDT ----
    grassland_main_server(
      "grassland_main"
    )

    ces_server(
      "ces_main"
    )

    disease_outbreaks_main_server("disease_outbreaks_main")

    shiny$observeEvent(input$biodt_logo, {
      nav_select(
        id = "navbar",
        selected = "info",
        session = session
      )
    })

    waiter_hide()
  })
}

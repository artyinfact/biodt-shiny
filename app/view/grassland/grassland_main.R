box::use(
  shiny[moduleServer, icon, NS, tagList, tags],
  bslib[navset_tab, nav_panel],
  shiny.router[router_ui, route, route_link],
)

box::use(
  app/view/grassland/grassland_dynamics/grassland_dynamics[
    grassland_dynamics_ui,
    grassland_dynamics_server
  ],
  app/view/grassland/info/grassland_info[grassland_info_ui],
)

#' @export
grassland_main_ui <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    tags$ul(
      inputId = "app-disease_outbreaks_main-tab",
      class = "nav nav-tabs",
      tags$li(
        class = "nav-item",
        tags$a("Info", href = route_link("grassland"))
      ),
      tags$li(
        class = "nav-item",
        tags$a(i18n$translate("Grassland Dynamics"), href = route_link("grasslandpdt")
        )
      ),
    
      router_ui(
        route("grasslandpdt", grassland_dynamics_ui(
            ns("grassland_app"),
            i18n
          )
        )
      )
    )
  )
}

#' @export
grassland_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {    
    ns <- session$ns    

    grassland_dynamics_server("grassland_app")
  })
}

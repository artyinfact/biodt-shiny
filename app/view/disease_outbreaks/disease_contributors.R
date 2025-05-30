box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)

#' @export
disease_contributors_ui <- function(id, i18n) {
  # nolint
  ns <- NS(id)
  fluidRow(
    class = "align-items-center justify-content-center m-0 p-0",
    style = "overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5",
        tags$h2(i18n$translate("CONTRIBUTORS"), style = "greeting display-4 font-weight-bold"),
        tags$p("Kate Ingenloff, GBIF Secretariat Universitetsparken, 15 DK-2100, Copenhagen Ø, Denmark"),
        tags$p(
          "Radek Halfar and Tomas Martinovic, IT4Innovations, VSB – Technical University of Ostrava, 17. listopadu 2172/15, 708 00 Ostrava-Poruba, Czech Republic"
        ),
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/Alexis-Lours-Sus-scrofa-Linnaeus.gif",
          alt = "Video of wild boar pack",
        )
      )
    )
  )
}

#' @export
disease_contributors_server <- function(id, r) {
  # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

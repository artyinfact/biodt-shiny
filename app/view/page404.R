box::use(
  shiny[a, div, h1, moduleServer, NS],
  shiny.router[route_link],
)

#' @export
page404_ui <- function(id) {
  ns <- NS(id)

  div(
    class="align-middle",
    h1("Whoops! Something went wrong!"),
    a("Back to home page", href = route_link("/"), class = "btn btn-primary btn-lg")
  )
}
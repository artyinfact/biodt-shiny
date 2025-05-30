box::use(
  readr[read_delim],
  utils[str]
)

# loads and restructure GRASSLAND data ----
#' @export
read_grass_simulations <- function(
  filename,
  column_types = list(
    Date = "D",
    DayCount = "i",
    PFT = "i",
    Fraction = "d",
    NumberPlants = "i"
  ),
  plot_type,
  colors,
  stack,
  file_nr
) {
  input_data <- read_delim(
    file = filename,
    skip = 0,
    trim_ws = TRUE,
    delim = "\t",
    escape_double = FALSE,
    col_names = TRUE,
    col_types = column_types
  )

  series_list <- list()
  pft_list <- sort(unique(input_data$PFT))

  for (i in seq_along(pft_list)) {
    series_list[[length(series_list) + 1]] <-
      list(
        name = ifelse(is.null(file_nr), paste0("PFT ", pft_list[i]), paste0("PFT ", pft_list[i], " file #", file_nr)),
        type = plot_type,
        stack = stack,
        color = colors[i],
        symbol = "none",
        showSymbol = FALSE,
        emphasis = list(disabled = TRUE),
        data = unname(as.list(unlist(input_data[input_data$PFT == pft_list[i], "Fraction"])))
      )
  }

  return(series_list)
}

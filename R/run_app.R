#' Run the ESBase Shiny application
#'
#' Launches the ESBase Shiny app with all required data, configuration,
#' and UI components initialised.
#'
#' This is the main entry point for users of the package.
#'
#' @return Invisibly returns the Shiny app object.
#'
#' @import shiny
#'
#' @export
run_app <- function() {

  ## ---- Packages ---------------------------------------------------
  # (imported via DESCRIPTION, attached here for clarity if needed)
  # library(shiny)

  ## ---- Load data --------------------------------------------------
  load(system.file("extdata", "all_tabs.Rdata", package = "esbaseShiny"))

  ## ---- Theme ------------------------------------------------------
  mytheme <- fresh::create_theme(
    fresh::adminlte_color(
      light_blue = "#144836"
    ),
    fresh::adminlte_sidebar(
      width = "120px",
      dark_bg = "#1D634A",
      dark_hover_bg = "#00892F",
      dark_color = "#FFF"
    ),
    fresh::adminlte_global(
      content_bg = "#FFF",
      box_bg = "#00892F",
      info_box_bg = "#00892F"
    )
  )

  ## ---- Derived configuration -------------------------------------
  accession_columns <- names(accession_tab)

  material_columns <- names(material_tab)

  plot_columns <- accession_tab |>
    dplyr::select(
      dplyr::where(is.numeric) |
        dplyr::where(~ dplyr::n_distinct(.) <= 100) |
        dplyr::where(~ inherits(.x, "Date"))
    ) |>
    names()

  ## ---- UI ---------------------------------------------------------
  ui <- app_ui(
    mytheme           = mytheme,
    accession_columns = accession_columns,
    plot_columns      = plot_columns,
    material_columns  = material_columns
  )

  ## ---- Server -----------------------------------------------------
  server <- function(input, output, session) {
    server_app(
      input        = input,
      output       = output,
      session      = session,
      accession_tab = accession_tab,
      material_tab  = material_tab
    )
  }

  ## ---- Run app ----------------------------------------------------
  shiny::shinyApp(ui = ui, server = server)
}

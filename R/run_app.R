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



  load_esbase_data <- function() {
    path <- system.file("extdata", "all_tabs.Rdata", package = "esbaseShiny")

    if (!nzchar(path)) {
      path <- "/var/lib/esbaseShiny/all_tabs.Rdata"
    }

    e <- new.env(parent = emptyenv())
    load(path, envir = e)

    list(
      accession_tab = e$accession_tab,
      material_tab  = e$material_tab
    )
  }

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

  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = "esbaseShiny")
  )

  ## ---- Derived configuration -------------------------------------
  tmp <- load_esbase_data()

  accession_columns <- names(tmp$accession_tab)

  material_columns <- names(tmp$material_tab)

  plot_columns <- tmp$accession_tab |>
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
    dat <- load_esbase_data()

    server_app(
      input         = input,
      output        = output,
      session       = session,
      accession_tab = dat$accession_tab,
      material_tab  = dat$material_tab
    )
  }

  ## ---- Run app ----------------------------------------------------
  shiny::shinyApp(ui = ui, server = server)
}

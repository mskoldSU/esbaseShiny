#' Server logic for the ESBase Shiny application
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param accession_tab Data frame of accessions.
#' @param material_tab Data frame of materials.
#'
#' @importFrom shiny reactive observeEvent renderTable downloadHandler req
#' @importFrom DT renderDT datatable
#' @importFrom leaflet renderLeaflet leaflet addTiles addMarkers markerClusterOptions
#' @importFrom plotly renderPlotly event_data
#' @importFrom shinyjs runjs
#'
#' @keywords internal
server_app <- function(
    input,
    output,
    session,
    accession_tab,
    material_tab
) {

  # ---- Reactives --------------------------------------------------

  acctbl_data <- reactive({
    accession_tab |>
      dplyr::select(dplyr::all_of(input$accession_columns))
  })

  filtered_accessions <- reactive({
    req(input$acctbl_rows_all)
    accession_tab[input$acctbl_rows_all, , drop = FALSE]
  })

  xy_data <- reactive({
    filtered_accessions() |>
      dplyr::select(
        esbase_id,
        accnr,
        dplyr::all_of(input$PlotX),
        dplyr::all_of(input$PlotY)
      ) |>
      dplyr::mutate(
        url = substr(esbase_id, 10, 52)
      ) |>
      tidyr::drop_na()
  })

  map_data <- reactive({
    filtered_accessions() |>
      dplyr::select(longitude, latitude, map_label) |>
      dplyr::distinct()
  })

  plot_data <- reactive({
    tibble::tibble(
      x = filtered_accessions() |>
        dplyr::select(
          dplyr::where(is.numeric) |
            dplyr::where(~ dplyr::n_distinct(.) <= 100) |
            dplyr::where(~ inherits(.x, "Date"))
        ) |>
        dplyr::pull(input$PlotColumn)
    )
  })

  mattbl_data <- reactive({
    material_tab |>
      dplyr::select(dplyr::all_of(input$material_columns))
  })

  summary_data <- reactive({
    req(plot_data())
    summarise_variable(plot_data()$x)
  })

  # ---- Downloads --------------------------------------------------

  output$download_acctbl_csv <- downloadHandler(
    filename = function() paste0("ESBase_data_", Sys.Date(), ".csv"),
    content = function(file) {
      idx <- input$acctbl_rows_all
      write_esbase_csv(acctbl_data()[idx, , drop = FALSE], file)
    }
  )

  output$download_mattbl_csv <- downloadHandler(
    filename = function() paste0("ESBase_data_", Sys.Date(), ".csv"),
    content = function(file) {
      idx <- input$mattbl_rows_all
      write_esbase_csv(mattbl_data()[idx, , drop = FALSE], file)
    }
  )

  # ---- Tables -----------------------------------------------------

  output$acctbl <- renderDT(
    datatable(
      acctbl_data(),
      filter = "top",
      escape = FALSE,
      selection = "none",
      options = list(dom = "ftip", pageLength = 25)
    ),
    server = TRUE,
    class = "compact stripe hover"
  )

  output$mattbl <- renderDT(
    datatable(
      mattbl_data(),
      filter = "top",
      escape = FALSE,
      selection = "none"
    ),
    server = TRUE
  )

  # ---- Map --------------------------------------------------------

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addMarkers(
        lng = map_data()$longitude,
        lat = map_data()$latitude,
        label = lapply(map_data()$map_label, htmltools::HTML),
        clusterOptions = markerClusterOptions()
      )
  })

  # ---- XY Plot ----------------------------------------------------

  output$plot_xy <- renderPlotly({
    req(input$PlotX, input$PlotY)

    plot_xy(
      df     = xy_data(),
      x      = input$PlotX,
      y      = input$PlotY,
      smooth = isTRUE(input$AddSmooth),
      linear = isTRUE(input$AddLine)
    )
  })

  observeEvent(
    plotly::event_data("plotly_click", source = "plot_xy"),
    {
      d <- plotly::event_data("plotly_click", source = "plot_xy")

      if (!is.null(d$customdata)) {
        shinyjs::runjs(
          sprintf("window.open('%s', '_blank')", d$customdata)
        )
      }
    }
  )


  # ---- Statistics -------------------------------------------------

  output$summary_stats <- renderTable({
    summary_data()
  }, align = "lr")

  output$plot <- renderPlotly({
    req(plot_data())
    plot_univariate(
      x    = plot_data()$x,
      xlab = input$PlotColumn
    )
  })
}

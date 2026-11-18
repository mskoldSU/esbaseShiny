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
      req(input$acctbl_rows_all)
      idx <- input$acctbl_rows_all
      data <- acctbl_data()[idx, , drop = FALSE]
      data <- reorder_export_columns(data, input$acctbl_col_order)
      write_esbase_csv(data, file)
    }
  )

  output$download_mattbl_csv <- downloadHandler(
    filename = function() paste0("ESBase_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$mattbl_rows_all)
      idx <- input$mattbl_rows_all
      data <- mattbl_data()[idx, , drop = FALSE]
      data <- reorder_export_columns(data, input$mattbl_col_order)
      write_esbase_csv(data, file)
    }
  )

  # ---- Tables -----------------------------------------------------

  # Send both the visual order and the original column names to Shiny.
  # The names identify the selected-column configuration, preventing stale
  # reorder events from being applied after columns are added or removed.
  col_reorder_callback <- function(input_id, column_names) {
    DT::JS(sprintf("
      var originalColumns = %s;
      function sendOrder() {
        Shiny.setInputValue('%s', {
          columns: originalColumns,
          order: table.colReorder.order()
        }, {priority: 'event'});
      }
      table.on('column-reorder.dt', sendOrder);
      sendOrder();
    ", jsonlite::toJSON(column_names, auto_unbox = FALSE), input_id))
  }

  output$acctbl <- renderDT(
    datatable(
      acctbl_data(),
      extensions = "ColReorder",
      filter = "top",
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(dom = "ftip", pageLength = 25, colReorder = TRUE),
      callback = col_reorder_callback(
        "acctbl_col_order", names(acctbl_data())
      )
    ),
    server = TRUE,
    class = "compact stripe hover"
  )

  output$mattbl <- renderDT(
    datatable(
      mattbl_data(),
      extensions = "ColReorder",
      filter = "top",
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(colReorder = TRUE),
      callback = col_reorder_callback(
        "mattbl_col_order", names(mattbl_data())
      )
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

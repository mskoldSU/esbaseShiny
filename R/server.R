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

  accession_defaults <- c(
    "esbase_id", "accnr", "discovery_year", "collection_project_name",
    "species_swe", "locality_name"
  )
  material_defaults <- c(
    accession_defaults, "material_type", "storage_location", "storage_tray"
  )

  acctbl_data <- reactive(accession_tab)

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

  mattbl_data <- reactive(material_tab)

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
      data <- export_columns(data, input$acctbl_col_order, accession_defaults)
      write_esbase_csv(data, file)
    }
  )

  output$download_mattbl_csv <- downloadHandler(
    filename = function() paste0("ESBase_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$mattbl_rows_all)
      idx <- input$mattbl_rows_all
      data <- mattbl_data()[idx, , drop = FALSE]
      data <- export_columns(data, input$mattbl_col_order, material_defaults)
      write_esbase_csv(data, file)
    }
  )

  # ---- Tables -----------------------------------------------------

  # Use the initial visibility until the browser reports its first state.
  export_columns <- function(data, state, defaults) {
    if (is.null(state)) {
      return(data[, names(data) %in% defaults, drop = FALSE])
    }
    reorder_export_columns(data, state)
  }

  table_options <- function(data, defaults) {
    list(
      dom = "Bftip",
      pageLength = 25,
      colReorder = TRUE,
      buttons = list(list(extend = "colvis", text = "Columns")),
      columnDefs = list(list(
        targets = which(!names(data) %in% defaults) - 1L,
        visible = FALSE
      ))
    )
  }

  # Order uses original indices; visibility uses the current visual positions.
  # Send both together so exports can follow the visible table after dragging.
  col_reorder_callback <- function(input_id, column_names) {
    DT::JS(sprintf("
      var originalColumns = %s;
      function sendOrder() {
        Shiny.setInputValue('%s', {
          columns: originalColumns,
          order: table.colReorder.order(),
          visible: table.columns().visible().toArray()
        }, {priority: 'event'});
      }
      table.on('column-reorder.dt column-visibility.dt', sendOrder);
      sendOrder();
    ", jsonlite::toJSON(column_names, auto_unbox = FALSE), input_id))
  }

  output$acctbl <- renderDT(
    datatable(
      acctbl_data(),
      extensions = c("ColReorder", "Buttons"),
      filter = "top",
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = table_options(acctbl_data(), accession_defaults),
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
      extensions = c("ColReorder", "Buttons"),
      filter = "top",
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = table_options(mattbl_data(), material_defaults),
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

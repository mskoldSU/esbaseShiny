#' Application UI
#'
#' Constructs the main user interface for the ESBase Shiny application.
#'
#' @param mytheme A \code{fresh} theme object created with
#'   \code{\link[fresh]{create_theme}}.
#' @param accession_columns Character vector of selectable accession columns.
#' @param plot_columns Character vector of variables available for plotting.
#' @param material_columns Character vector of selectable material columns.
#'
#' @return A shiny UI object.
#'
#' @importFrom shiny tags
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody tabItems sidebarMenu menuItem
#' @importFrom fresh use_theme
#'
#' @export
app_ui <- function(
    mytheme,
    accession_columns,
    plot_columns,
    material_columns
) {

  shinydashboard::dashboardPage(
    title = "ESBase – Search & Export",

    dashboardHeader(
      title = shiny::tags$div(
        style = "text-align: center; padding: 0px;",
        shiny::tags$img(src = "www/nrm2.png", width = "80px")
      ),
      shiny::tags$li(
        class = "dropdown",
        style = "padding: 15px; font-size: 15px; color: white;",
        "ESBase search & export"
      )
    ),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Accessions", tabName = "accessions", icon = icon("table")),
        menuItem("Materials",  tabName = "materials",  icon = icon("vials"))
      )
    ),

    dashboardBody(
      fresh::use_theme(mytheme),
      tabItems(
        ui_accessions(accession_columns, plot_columns),
        ui_materials(material_columns)
      )
    )
  )
}


#' Materials tab UI
#'
#' Constructs the UI for the Materials tab.
#'
#' @param material_columns Character vector of selectable material columns.
#'
#' @return A \code{shinydashboard::tabItem}.
#'
#' @importFrom shiny fluidRow column selectInput downloadButton br
#' @importFrom shinydashboard tabItem
#' @importFrom DT DTOutput
#'
#' @export
ui_materials <- function(material_columns) {

  tabItem(
    tabName = "materials",

    fluidRow(
      column(
        12,

        selectInput(
          "material_columns",
          "Columns",
          choices = material_columns,
          selected = c(
            "esbase_id", "accnr", "discovery_year",
            "collection_project_name", "species_swe",
            "locality_name", "material_type",
            "storage_location", "storage_tray"
          ),
          multiple = TRUE
        ),

        DT::DTOutput("mattbl"),
        shiny::br(),
        downloadButton("download_mattbl_csv", "Download CSV")
      )
    )
  )
}



#' Accessions tab UI
#'
#' Constructs the UI for the Accessions tab, including table, map,
#' statistics, and XY plots.
#'
#' @param accession_columns Character vector of selectable accession columns.
#' @param plot_columns Character vector of variables available for plotting.
#'
#' @return A \code{shinydashboard::tabItem}.
#'
#' @importFrom shiny fluidRow column selectInput checkboxInput tableOutput div tabPanel
#' @importFrom shinydashboard tabItem tabBox
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinyjs useShinyjs
#'
#' @export
ui_accessions <- function(accession_columns, plot_columns) {

  tabItem(
    tabName = "accessions",

    tabBox(
      width = 12,

      # ---- TABLE ---------------------------------------------------
      tabPanel(
        title = "Table",

        selectInput(
          "accession_columns",
          "Columns",
          choices = accession_columns,
          selected = c(
            "esbase_id", "accnr", "discovery_year",
            "collection_project_name",
            "species_swe", "locality_name"
          ),
          multiple = TRUE
        ),

        DT::DTOutput("acctbl"),
        downloadButton("download_acctbl_csv", "Download CSV")
      ),

      # ---- MAP -----------------------------------------------------
      tabPanel(
        title = "Map",
        shiny::div(
          style = "height: 80vh;",
          leaflet::leafletOutput("map", height = "100%")
        )
      ),

      # ---- STATISTICS ----------------------------------------------
      tabPanel(
        title = "Statistics",

        selectInput(
          "PlotColumn",
          "Variable",
          choices = plot_columns,
          selected = plot_columns[1]
        ),

        fluidRow(
          column(8, plotly::plotlyOutput("plot", height = "70vh")),
          column(4, tableOutput("summary_stats"))
        )
      ),

      # ---- XY PLOT -------------------------------------------------
      tabPanel(
        title = "XY plot",

        shinyjs::useShinyjs(),

        fluidRow(
          column(
            4,
            selectInput(
              "PlotX",
              "X variable",
              choices = plot_columns,
              selected = plot_columns[1]
            )
          ),
          column(
            4,
            selectInput(
              "PlotY",
              "Y variable",
              choices = plot_columns,
              selected = plot_columns[2]
            )
          )
        ),

        fluidRow(
          column(
            12,
            plotly::plotlyOutput("plot_xy", height = "70vh")
          )
        ),

        fluidRow(
          column(4, checkboxInput("AddSmooth", "Add smoothing line")),
          column(4, checkboxInput("AddLine", "Add linear trend"))
        )
      )
    )
  )
}



#' Interactive XY scatter plot with optional smoothing
#'
#' Creates an interactive Plotly scatter plot with optional
#' GAM smoothing and/or linear regression trend lines.
#'
#' @param df A data frame containing the data.
#' @param x Character scalar. Name of the x-axis column.
#' @param y Character scalar. Name of the y-axis column.
#' @param smooth Logical. If TRUE, add a GAM smoothing curve.
#' @param linear Logical. If TRUE, add a linear regression line.
#' @param n Integer. Number of points used for fitted lines.
#'
#' @return A \code{plotly} object.
#'
#' @details
#' The smoothing curve is fitted using \code{mgcv::gam()} with a
#' univariate smooth term \code{s(x)}. Fitted lines have hover
#' disabled by default to avoid interference with point hover.
#'
#' @examples
#' \dontrun{
#' plot_xy(mtcars, "wt", "mpg", smooth = TRUE, linear = TRUE)
#' }
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom mgcv gam
#' @export
plot_xy <- function(df,
                    x,
                    y,
                    smooth = FALSE,
                    linear = FALSE,
                    n = 50) {

  stopifnot(is.data.frame(df))
  stopifnot(x %in% names(df), y %in% names(df))

  p <- plotly::plot_ly(source = "plot_xy") |>
    plotly::add_trace(
      x = df[[x]],
      y = df[[y]],
      type = "scatter",
      mode = "markers",
      name = "Data",
      marker = list(opacity = 0.6),
      customdata = df[["url"]],
      hovertemplate = paste0(
        df[["accnr"]], "<br>",
        "<b>", x, ":</b> %{x}<br>",
        "<b>", y, ":</b> %{y}",
        "<extra></extra>"
      )
    )

  x_seq <- seq(
    min(df[[x]], na.rm = TRUE),
    max(df[[x]], na.rm = TRUE),
    length.out = n
  )

  # GAM smooth
  if (isTRUE(smooth)) {
    gam_fit <- mgcv::gam(
      stats::reformulate(paste0("s(", x, ")"), y),
      data = df
    )

    pred <- stats::setNames(data.frame(x_seq), x)
    yhat <- stats::predict(gam_fit, newdata = pred)

    p <- p |>
      plotly::add_trace(
        x = x_seq,
        y = yhat,
        type = "scatter",
        mode = "lines",
        name = "GAM smooth",
        hovertemplate = NULL)
  }

  # Linear regression
  if (isTRUE(linear)) {
    lm_fit <- stats::lm(stats::reformulate(x, y), data = df)

    pred <- stats::setNames(data.frame(x_seq), x)
    yhat <- stats::predict(lm_fit, newdata = pred)

    p <- p |>
      plotly::add_trace(
        x = x_seq,
        y = yhat,
        type = "scatter",
        mode = "lines",
        name = "Linear fit",
        hovertemplate = NULL,
        line = list(dash = "dash")
      )
  }

  p |>
    plotly::layout(
      xaxis = list(title = x),
      yaxis = list(title = y),
      hovermode = "closest",
      hoverlabel = list(
        bgcolor = "rgba(255,255,255,0.95)",
        font = list(color = "black"),
        bordercolor = "rgba(0,0,0,0.2)"
      ),
      plot_bgcolor='rgb(250,250,250)'
    )
}

#' Interactive univariate plot
#'
#' Creates an interactive Plotly plot for a single variable:
#' a density plot with rug for numeric data, or a bar plot
#' for categorical data with limited cardinality.
#'
#' @param x A vector. Numeric, Date, or categorical.
#' @param xlab Character. Label for the x-axis.
#' @param max_levels Integer. Maximum number of categories allowed
#'   for bar plots.
#'
#' @return A \code{plotly} object, or NULL if input is unsupported.
#'
#' @details
#' Numeric variables are visualized using a kernel density estimate
#' computed via \code{stats::density()}. Categorical variables are
#' summarized using counts.
#'
#' @examples
#' \dontrun{
#' plot_univariate(mtcars$mpg, "Miles per gallon")
#' plot_univariate(iris$Species, "Species")
#' }
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @export
plot_univariate <- function(x,
                            xlab = deparse(substitute(x)),
                            max_levels = 100) {

  if (is.numeric(x)) {

    dens <- stats::density(x, na.rm = TRUE)

    p <- plotly::plot_ly() |>
      plotly::add_trace(
        x = dens$x,
        y = dens$y,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        name = "Density",
        fillcolor = "lightgrey",
        line = list(color = "black"),
        hovertemplate = "x=%{x}<br>density=%{y}<extra></extra>"
      ) |>
      plotly::add_trace(
        x = x,
        y = rep(0, length(x)),
        type = "scatter",
        mode = "markers",
        name = "Rug",
        marker = list(
          symbol = "line-ns-open",
          size = 10,
          color = "black"
        ),
        hoverinfo = "skip"
      )

  } else {
    x_chr <- as.character(x)
    n_levels <- length(unique(x_chr))

    if (n_levels > max_levels) {
      return(NULL)
    }

    tab <- as.data.frame(table(x_chr))
    names(tab) <- c("x", "n")
    tab <- dplyr::mutate(tab, x = as.factor(x) |> forcats::fct_reorder(n))

    p <- plotly::plot_ly(
      data = tab,
      x = ~x,
      y = ~n,
      type = "bar",
      name = "Count",
      hovertemplate = "x=%{x}<br>count=%{y}<extra></extra>"
    )
  }

  p |>
    plotly::layout(
      xaxis = list(title = xlab),
      yaxis = list(title = ""),
      hovermode = "closest",
      plot_bgcolor='rgb(250,250,250)'
    )
}

#' Summarise a vector for display in Shiny
#'
#' Computes a concise summary of a vector depending on its type.
#'
#' * **Date** vectors are summarised by earliest, latest, median, count, and missing values.
#' * **Numeric** vectors are summarised by mean, median, standard deviation, range, count, and missing values.
#' * **Other** vectors (factor, character, logical) are summarised by category counts.
#'
#' This function is designed for use in interactive dashboards and tables,
#' where compact, human-readable summaries are preferred over exhaustive statistics.
#'
#' @param x A vector to summarise. May be numeric, Date, factor, character, or logical.
#'
#' @return A tibble containing summary statistics or category counts.
#'
#' @details
#' For Date vectors, mean and standard deviation are intentionally omitted
#' because they are rarely interpretable in practice.
#'
#' @seealso \code{\link[tibble]{tibble}}
#'
#' @examples
#' summarise_variable(rnorm(100))
#'
#' summarise_variable(as.Date("2020-01-01") + 0:10)
#'
#' summarise_variable(c("A", "B", "A", "C"))
#'
#' @export
summarise_variable <- function(x) {

  if (inherits(x, "Date")) {

    tibble::tibble(
      Statistic = c("Earliest", "Latest", "Median", "N", "Missing"),
      Value = c(
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        sum(!is.na(x)),
        sum(is.na(x))
      )
    )

  } else if (is.numeric(x)) {

    tibble::tibble(
      Statistic = c("Mean", "Median", "SD", "Min", "Max", "N", "Missing"),
      Value = c(
        signif(mean(x, na.rm = TRUE), 3),
        median(x, na.rm = TRUE),
        signif(stats::sd(x, na.rm = TRUE), 3),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        sum(!is.na(x)),
        sum(is.na(x))
      )) |>
      dplyr::mutate(
        Value = ifelse(Value %% 1 == 0,
                       as.character(as.integer(Value)),
                       as.character(Value))
      )

  } else {

    tibble::tibble(
      Category = names(table(x)),
      Count = as.integer(table(x))
    )
  }
}

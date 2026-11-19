# Prepare the table data for export, keeping the HTML-based display unchanged.
#
# The esbase_id field contains clickable HTML in the Shiny table. Export its
# hyperlink target rather than the HTML markup, so it remains usable in Excel.
esbase_export_data <- function(data) {
  if ("esbase_id" %in% names(data) && is.character(data$esbase_id)) {
    ids <- data$esbase_id
    links <- grepl("^\\s*<a\\b", ids, perl = TRUE) & !is.na(ids)
    if (any(links)) {
      anchor <- ids[links]
      matches <- regmatches(
        anchor,
        regexec("href\\s*=\\s*['\"]([^'\"]+)['\"]", anchor, perl = TRUE)
      )
      ids[links] <- vapply(seq_along(matches), function(i) {
        if (length(matches[[i]]) > 1L) {
          matches[[i]][2L]
        } else {
          trimws(gsub("<[^>]+>", "", anchor[i]))
        }
      }, character(1))
    }
    data$esbase_id <- ids
  }
  data
}

# UTF-8 with BOM, semicolon separator and comma decimals for Windows Excel.
write_esbase_csv <- function(data, file) {
  readr::write_excel_csv2(esbase_export_data(data), file, na = "")
}

# Reorder only when the order came from the currently selected DT columns.
# The browser reports zero-based indices relative to the original R columns.
reorder_export_columns <- function(data, col_state) {
  if (is.null(col_state) || is.null(col_state$columns) ||
      is.null(col_state$order)) {
    return(data)
  }

  source_columns <- as.character(unlist(col_state$columns, use.names = FALSE))
  order <- as.integer(unlist(col_state$order, use.names = FALSE))

  if (!identical(source_columns, names(data)) ||
      length(order) != ncol(data) ||
      anyNA(order) ||
      !identical(sort(order), seq_len(ncol(data)) - 1L)) {
    return(data)
  }

  # Visibility is indexed by current position, matching the reordered columns.
  if (!is.null(col_state$visible)) {
    visible <- unlist(col_state$visible, use.names = FALSE)
    if (!is.logical(visible) || length(visible) != ncol(data) || anyNA(visible)) {
      return(data)
    }
    order <- order[visible]
  }

  data[, order + 1L, drop = FALSE]
}

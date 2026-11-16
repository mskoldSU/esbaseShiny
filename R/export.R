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

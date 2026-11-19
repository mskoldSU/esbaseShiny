# Run from the repository root with: Rscript tests/check-csv-export.R
source("R/export.R")

sample <- data.frame(
  esbase_id = c('<a href="https://example.org/esbase/123">123</a>', "plain-id"),
  locality_name = c("Miljöövervakning, Rosendhol", "Lilla Lien"),
  species_swe = c("Älg", "Älg"),
  concentration = c(1.5, 2.25),
  stringsAsFactors = FALSE
)
before <- sample
file <- tempfile(fileext = ".csv")
write_esbase_csv(sample, file)

bytes <- readBin(file, what = "raw", n = file.info(file)$size)
stopifnot(identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf))))

result <- readr::read_csv2(file, show_col_types = FALSE)
stopifnot(
  identical(result$esbase_id, c("https://example.org/esbase/123", "plain-id")),
  identical(result$locality_name, sample$locality_name),
  identical(result$species_swe, sample$species_swe),
  isTRUE(all.equal(result$concentration, sample$concentration)),
  identical(sample, before)
)
unlink(file)
cat("CSV export checks passed\n")

# Simulate dragging the fourth column to the second position.
state <- list(
  columns = names(sample),
  order = c(0L, 3L, 1L, 2L)
)
reordered <- reorder_export_columns(sample, state)
stopifnot(
  identical(names(reordered),
            names(sample)[c(1L, 4L, 2L, 3L)]),
  identical(sample, before)
)

# Exports continue to use the Excel-safe writer after reordering.
file <- tempfile(fileext = ".csv")
write_esbase_csv(reordered, file)
exported <- readr::read_csv2(file, show_col_types = FALSE)
stopifnot(
  identical(names(exported), names(reordered)),
  identical(exported$esbase_id,
            c("https://example.org/esbase/123", "plain-id"))
)
unlink(file)

# Changing selected columns must invalidate the old browser order.
subset_data <- sample[, c("esbase_id", "species_swe"), drop = FALSE]
stopifnot(identical(reorder_export_columns(subset_data, state), subset_data))

# Invalid or absent reordering must preserve the input data.
stopifnot(
  identical(reorder_export_columns(sample, NULL), sample),
  identical(reorder_export_columns(sample,
            list(columns = names(sample), order = c(0L, 1L, 1L, 3L))), sample)
)
cat("Column reorder export checks passed\n")

# Hiding columns after dragging exports only the visible columns in visual order.
state$visible <- c(TRUE, FALSE, FALSE, TRUE)
stopifnot(
  identical(names(reorder_export_columns(sample, state)),
            names(sample)[c(1L, 3L)])
)
state$visible <- rep(FALSE, ncol(sample))
stopifnot(ncol(reorder_export_columns(sample, state)) == 0L)
state$visible <- c(TRUE, FALSE)
stopifnot(identical(reorder_export_columns(sample, state), sample))
cat("Column visibility export checks passed\n")

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
on.exit(unlink(file), add = TRUE)
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
cat("CSV export checks passed\n")

## Helpers for the supplementary materials ##

# Run all evaluated code chunks of a website vignette in a new environment and
# return that environment, so that supplementary documents can reuse the
# figures and tables the vignettes produce (instead of duplicating code).
# Chunks with eval = FALSE (e.g., long-running analyses whose results are
# saved in the data folder) are not run.
run_vignette <- function(name, root = "..") {
  qmd <- normalizePath(file.path(root, paste0(name, ".qmd")))
  r_file <- tempfile(fileext = ".R")
  writeLines(extract_r_chunks(qmd), r_file)
  env <- new.env(parent = globalenv())
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  # absorb any graphics drawn while running the code
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  invisible(utils::capture.output(
    suppressMessages(suppressWarnings(sys.source(r_file, envir = env)))
  ))
  env
}

# Extract the code of all evaluated R chunks of a Quarto document (knitr::purl
# cannot be used while another document is being knitted)
extract_r_chunks <- function(qmd) {
  lines <- readLines(qmd, warn = FALSE)
  starts <- grep("^```\\{r", lines)
  code <- character(0)
  for (start in starts) {
    end <- start + which(grepl("^```\\s*$", lines[(start + 1):length(lines)]))[1]
    header <- lines[start]
    body <- lines[(start + 1):(end - 1)]
    skip <- grepl("eval\\s*=\\s*FALSE", header) ||
      any(grepl("^#\\|\\s*eval:\\s*false", body))
    if (!skip) code <- c(code, body, "")
  }
  code
}

# Print an HTML table (or a list of table sections) in a results: asis chunk
print_tables <- function(...) {
  cat(paste(vapply(list(...), as.character, character(1)), collapse = "\n"))
}

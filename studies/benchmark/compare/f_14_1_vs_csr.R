# Figure 14-1 against the pilot's study report, page 151.
#
# The printed figure is an image with no numbers, so it cannot be compared cell by cell.
# The comparison has two parts: our ADTTE equals the official one (target
# adtte_vs_official), and the printed page is rendered next to our figure for a reviewer.

#' Render the report's Figure 14-1 page to PNG, to review next to ours.
#' @param csr_path Path of the pilot's study report (PDF).
#' @param path File to write. It stays out of git, like the report itself (ADR 0015).
#' @return `path`, for a file target.
render_csr_f_14_1 <- function(csr_path, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  bitmap <- pdftools::pdf_render_page(csr_path, page = 151, dpi = 110)
  png::writePNG(bitmap, path)
  path
}

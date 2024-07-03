# convert pdf to png
convert_pdfs_table <- function(path) {
  fn <- list.files(path, full.names = TRUE, pattern = ".pdf")

  fn_out <- gsub("pdf", "png", fn)

  for (i in seq_along(fn)) {
    pdf_convert(fn[i], filenames = fn_out[i], dpi = 300)
  }

}

convert_pdfs <- function(path) {
  fn <- list.files(path, full.names = TRUE, pattern = ".pdf")

  fn_out <- gsub("pdf", "png", basename(fn))
  fn_out <- file.path(dirname(fn), "png", fn_out)

  for (i in seq_along(fn)) {
    pdf_convert(fn[i], filenames = fn_out[i], dpi = 300)
  }

  return(NULL)
}

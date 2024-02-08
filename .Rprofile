
source_files <- function(path, pattern = "\\.R$") {
  # List all files in the specified path
  all_files <- list.files(path, pattern = pattern, full.names = TRUE)

  # Source each file
  sapply(all_files, source)
}

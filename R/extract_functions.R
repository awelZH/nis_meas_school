# extract file paths to csv for Einzeldaten
extract_csv_paths_einzel <- function(top_folder_path, num_dirs = Inf) {
  daten_dirs <- fs::dir_ls(top_folder_path, recurse = TRUE, glob = "*Daten") %>%
    head(num_dirs)

  if (length(daten_dirs) == 0) {
    cat("Found 0 'Daten' directories under", top_folder_path, "\n")
    return(NULL)
  }

  # Counters
  total_folders <- length(daten_dirs)
  counter <- 1

  paths_list <- purrr::map(daten_dirs, function(dir) {
    subfolders <- fs::dir_ls(dir, type = "directory")
    csv_files <- if (length(subfolders) > 0) {
      purrr::map(subfolders, ~ fs::dir_ls(.x, glob = "*.csv")) %>% unlist()
    } else {
      fs::dir_ls(dir, glob = "*.csv")
    }

    messort_code <- stringr::str_extract(dir, "\\d+_Daten")

    # Print the current progress with messort_code
    cat(sprintf("Processing 'Messort': %s (%d out of %d)\n", messort_code, counter, total_folders))
    counter <<- counter + 1

    list(csv_files = csv_files, messort_code = messort_code)
  })

  names(paths_list) <- purrr::map_chr(daten_dirs, ~ stringr::str_extract(.x, "\\d+_Daten"))
  return(paths_list)
}



# extract langzeit paths
extract_csv_paths_langzeit <- function(top_folder_path, num_dirs = Inf) {
  daten_dirs <- fs::dir_ls(top_folder_path, recurse = TRUE, glob = "*_Daten") %>%
    head(num_dirs)

  if (length(daten_dirs) == 0) {
    cat("Found 0 '_Daten' directories under", top_folder_path, "\n")
    return(NULL)
  }

  # Counters
  total_folders <- length(daten_dirs)
  counter <- 1

  # Creating a nested list with Messorte as first level and years as second level
  paths_list <- purrr::map(daten_dirs, function(dir) {
    messort_code <- stringr::str_extract(basename(dir), "\\d+_Daten")
    year <- basename(dirname(dir))
    csv_files <- fs::dir_ls(dir, glob = "*.csv")

    # Print the current progress with messort_code
    cat(sprintf("Processing 'Messort': %s, Year: %s (%d out of %d)\n", messort_code, year, counter, total_folders))
    counter <<- counter + 1

    list(year = year, csv_files = csv_files)
  }) %>%
    # Grouping by messort_code
    split(purrr::map_chr(daten_dirs, ~ stringr::str_extract(basename(.x), "\\d+_Daten")))

  # Further nesting each messort_code group by year
  purrr::map(paths_list, ~split(.x, purrr::map_chr(.x, "year")))
}

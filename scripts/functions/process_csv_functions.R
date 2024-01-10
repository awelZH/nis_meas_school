# processing the data for Einzeldaten
process_csv_data_einzel <- function(filtered_paths_list) {
  total_folders <- length(filtered_paths_list)
  counter <- 1


  map(filtered_paths_list, function(folder_info) {
    # Dynamically get the folder name which ends with "_Daten"
    folder_name <- names(folder_info)[1]  # Since each list has only one element with the pattern "_Daten"
    csv_files <- folder_info[[folder_name]]

    # Print the current progress of folders
    cat(sprintf("Processing folder %d out of %d\n", counter, total_folders))

    processed_files <- map(cli_progress_along(csv_files), function(i) {
      file_path <- csv_files[[i]]  # Use the index to get the actual file path
      file_contents <- fread(file = file_path, skip = "Fmin [Hz]") %>%
        clean_names()

      # Read the first 10 lines to extract date and time
      timestamp <- file_path %>%
        read_lines(n_max = 10) %>%
        str_extract(pattern = "(?<=Time;|Date;)[^;]+") %>%
        discard(is.na) %>%
        str_flatten(collapse = "T")

      # If Messort_Code is needed and can be derived from folder_name, extract it. Otherwise, remove it.
      file_contents %>%
        mutate(Zeitstempel = timestamp) %>%
        select(-v1) %>%
        mutate(Messort_Code = str_extract(folder_name, "\\d+"))
    }) %>% bind_rows()

    # Increment the counter after processing each folder
    counter <<- counter + 1
    processed_files
  })
}


# function to process langzeit data
process_csv_data_langzeit <- function(filtered_paths_list) {
  total_messorte <- length(filtered_paths_list)
  messort_counter <- 1

  process_files <- function(files, messort_code) {
    map(cli_progress_along(files), function(i) {
      file <- files[[i]]
      file_contents <- fread(file = file, skip = "Fmin [Hz]") %>%
        clean_names()

      # Read the first 10 lines to extract date and time
      timestamp <- file %>%
        read_lines(n_max = 10) %>%
        str_extract(pattern = "(?<=Time;|Date;)[^;]+") %>%
        discard(is.na) %>%
        str_flatten(collapse = "T")

      file_contents %>%
        mutate(Zeitstempel = timestamp, Messort_Code = messort_code) %>%
        select(-v1) %>%
        mutate(Messort_Code = str_extract(Messort_Code, "[[:digit:]]+"))
    }) %>% list_rbind()
  }

  processed_data <- map2(filtered_paths_list, names(filtered_paths_list), function(years_list, messort_code) {
    cat(sprintf("Processing 'Messort' %d out of %d: %s\n", messort_counter, total_messorte, messort_code))

    messort_processed_files <- map2(years_list, names(years_list), function(year_info, year) {
      map2(year_info, names(year_info), function(dir_info, dir) {
        csv_files <- dir_info$csv_files
        process_files(csv_files, messort_code)
      })
    })

    messort_counter <<- messort_counter + 1
    messort_processed_files
  })

  return(processed_data)
}

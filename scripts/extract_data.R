# load packages

library(cli)
library(purrr)
library(stringr)
library(vroom)
library(janitor)
library(readr)
library(vctrs)
library(dplyr)
library(fs)
library(data.table)

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

  paths_list <- map(daten_dirs, function(dir) {
    subfolders <- fs::dir_ls(dir, type = "directory")
    csv_files <- if (length(subfolders) > 0) {
      map(subfolders, ~ fs::dir_ls(.x, glob = "*.csv")) %>% unlist()
    } else {
      fs::dir_ls(dir, glob = "*.csv")
    }

    messort_code <- str_extract(dir, "\\d+_Daten")

    # Print the current progress with messort_code
    cat(sprintf("Processing 'Messort': %s (%d out of %d)\n", messort_code, counter, total_folders))
    counter <<- counter + 1

    list(csv_files = csv_files, messort_code = messort_code)
  })

  names(paths_list) <- map_chr(daten_dirs, ~ str_extract(.x, "\\d+_Daten"))
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
  paths_list <- map(daten_dirs, function(dir) {
    messort_code <- str_extract(basename(dir), "\\d+_Daten")
    year <- basename(dirname(dir))
    csv_files <- fs::dir_ls(dir, glob = "*.csv")

    # Print the current progress with messort_code
    cat(sprintf("Processing 'Messort': %s, Year: %s (%d out of %d)\n", messort_code, year, counter, total_folders))
    counter <<- counter + 1

    list(year = year, csv_files = csv_files)
  }) %>%
    # Grouping by messort_code
    split(map_chr(daten_dirs, ~ str_extract(basename(.x), "\\d+_Daten")))

  # Further nesting each messort_code group by year
  map(paths_list, ~split(.x, map_chr(.x, "year")))
}


# Function to filter out HEADER CSV files
filter_csv_files_einzel <- function(paths_list) {
  map(cli_progress_along(paths_list), function(i) {
    folder_info <- paths_list[[i]]
    folder_info$csv_files <- folder_info$csv_files %>%
      keep(~ !str_detect(.x, "_HEADER\\.csv"))
    folder_info
  })
}

filter_csv_files_langzeit <- function(paths_list) {
  # Helper function to filter out HEADER CSV files
  filter_header_csv <- function(csv_files) {
    csv_files[!grepl("_HEADER\\.csv$", csv_files)]
  }

  # Function to process each directory at the year level
  process_directory <- function(directory) {
    directory$csv_files <- filter_header_csv(directory$csv_files)
    directory
  }

  # Function to process each year within a 'Messort'
  process_year <- function(year) {
    map(year, process_directory)
  }

  # Function to process each 'Messort'
  process_messort <- function(messort) {
    map(messort, process_year)
  }

  map(paths_list, process_messort)
}




# processing the data for Einzeldaten
process_csv_data_einzel <- function(filtered_paths_list) {
  total_folders <- length(filtered_paths_list)
  counter <- 1

  map(filtered_paths_list, function(folder_info) {
    csv_files <- folder_info$csv_files

    # Print the current progress of folders
    cat(sprintf("Processing folder %d out of %d\n", counter, total_folders))

    processed_files <- map(cli_progress_along(csv_files), function(i) {
      file <- csv_files[[i]]
      file_contents <- fread(file = file, skip = "Fmin [Hz]") %>%
        clean_names()

      # Read the first 10 lines to extract date and time
      timestamp <- file %>%
        read_lines(n_max = 10) %>%
        str_extract(pattern = "(?<=Time;|Date;)[^;]+") %>%
        discard(is.na) %>%
        str_flatten(collapse = "T")

      file_contents %>%
        mutate(Zeitstempel = timestamp, Messort_Code = folder_info$messort_code) %>%
        select(-v1) %>%
        mutate(Messort_Code = str_extract(Messort_Code, "[[:digit:]]+"))
    }) %>% list_rbind()

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


# check for NA Einzeldaten
check_na_einzelmessungen <- function(einzelmessungen_list) {
  na_containing_dfs <- map(einzelmessungen_list, \(df, name) {
    if (any(is.na(df$value_v_m))) {
      return(name)
    }
    NULL
  }, name = names(einzelmessungen_list)) %>%
    compact() %>%
    unlist()

  return(na_containing_dfs)
}


# check for NA Langzeitmessungen
check_na_langzeitmessungen <- function(langzeitmessungen_list) {
  na_containing_paths <- c()

  for (messort_name in names(langzeitmessungen_list)) {
    messort_list <- langzeitmessungen_list[[messort_name]]

    for (year_name in names(messort_list)) {
      year_list <- messort_list[[year_name]]

      for (dir_name in names(year_list)) {
        df <- year_list[[dir_name]]

        if (any(is.na(df$value_v_m))) {
          path <- paste(messort_name, year_name, dir_name, sep = "/")
          na_containing_paths <- c(na_containing_paths, path)
        }
      }
    }
  }

  return(na_containing_paths)
}

# load packages

library(cli)
library(purrr)
library(stringr)
library(vroom)
library(janitor)
library(readr)
library(vctrs)
library(dplyr)

# define functions
extract_csv_paths_einzel <- function(top_folder_path, num_dirs = Inf) {
  daten_dirs <- fs::dir_ls(top_folder_path, recurse = TRUE, glob = "*Daten") %>%
    head(num_dirs)

  if (length(daten_dirs) == 0) {
    cat("Found 0 'Daten' directories under", top_folder_path, "\n")
    return(NULL)
  }

  paths_list <- map(daten_dirs, function(dir) {
    subfolders <- fs::dir_ls(dir, type = "directory")
    csv_files <- if (length(subfolders) > 0) {
      map(subfolders, ~ fs::dir_ls(.x, glob = "*.csv")) %>% unlist()
    } else {
      fs::dir_ls(dir, glob = "*.csv")
    }

    list(csv_files = csv_files, messort_code = str_extract(dir, "\\d+_Daten"))
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
filter_csv_files <- function(paths_list) {
  map(cli_progress_along(paths_list), function(i) {
    folder_info <- paths_list[[i]]
    folder_info$csv_files <- folder_info$csv_files %>%
      keep(~ !str_detect(.x, "_HEADER\\.csv"))
    folder_info
  })
}


# Get the paths of CSV files
top_folder_path <- "Schulhausmessungen"
csv_paths_list_2 <- extract_csv_paths(top_folder_path, num_dirs = )

csv_paths_list_5 <- extract_csv_paths(top_folder_path, num_dirs = 5)

csv_paths_list_all <- extract_csv_paths(top_folder_path, num_dirs = Inf)



# processing the data
process_csv_data <- function(filtered_paths_list) {
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
# Example usage

measurements_data <- process_csv_data(csv_paths_list_5)

measurements_data_all <- process_csv_data(csv_paths_list_all)


# save results

saveRDS(object = measurements_data_all, file = "Einzelmessungen_raw_list.RDS")


# flatten list

measurements_data_all_modified <- map(measurements_data_all, ~{
  if ("value_v_m_2" %in% names(.x)) {
    .x %>%
      mutate(
        value_v_m = coalesce(value_v_m, value_v_m_2)
      )
  } else {
    .x
  }
}, .progress = TRUE)



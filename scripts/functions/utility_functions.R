# Function to filter out HEADER CSV files
filter_csv_files_einzel <- function(paths_list) {
  # Map function with preservation of names
  map2(paths_list, names(paths_list), function(folder_info, name) {
    folder_info$csv_files <- folder_info$csv_files %>%
      keep(~ !str_detect(.x, "_HEADER\\.csv"))
    # Set the name for each element
    setNames(folder_info, name)
  }) %>% set_names(names(paths_list))
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


# pull github changes
update_project_from_github <- function() {
  # Define the command to pull from the remote repository
  git_command <- "git pull origin main"

  # Run the command in the shell
  system(git_command, intern = TRUE)
}

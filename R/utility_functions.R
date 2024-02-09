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

# Check if File is available from URL
check_file_availability <- function(file_url) {
  # Send a HEAD request to the URL
  response <- httr::HEAD(file_url)

  # Check if the status code is 200 (OK)
  if (httr::status_code(response) == 200) {
    return(TRUE)  # File is available
  } else {
    return(FALSE) # File is not available
  }
}


# function to check if files in folder and ogd_messorte are same (full load)
check_full_load <- function(csv_paths_list_all_einzel, csv_paths_list_all_langzeit){

  # get messorte IDs from OGD ressource
  existing_ogd_list <- retrieve_existing_data()


  messort_ids_ogd <- existing_ogd_list$Messorte %>%
    dplyr::select(Messort_Code)


  messort_ids_folder <- c(names(csv_paths_list_all_einzel), names(csv_paths_list_all_langzeit)) %>%
    stringr::str_extract("\\d+") %>%
    tibble("Messort_Code" = .) %>%
    dplyr::mutate(Messort_Code = as.numeric(Messort_Code))

  in_folder <- anti_join(messort_ids_folder, messort_ids_ogd)
  in_ogd <- anti_join(messort_ids_ogd, messort_ids_folder)

  if(nrow(in_folder) > 0){
    cli::cli_abort("In der Ordnerstruktur sind Messorte IDs vorhanden welche nicht im Messorte.csv als OGD veröffentlicht sind.
                   Dies sind die IDs: {in_folder$Messort_Code}")
  } else if (nrow(in_ogd) > 0){
    cli::cli_abort("In der OGD Ressource Messorte.csv sind Messorte IDs vorhanden welche nicht in der Ordnerstruktur gefunden wurden.
                   Dies sind die IDs: {in_ogd$Messort_Code}")
  } else {
    cli::cli_alert_success("Jede Messorte ID aus der OGD Ressource Messorte.csv hat ein Match in der Ordnerstruktur.")
  }

}


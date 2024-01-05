# function to get existing data from ogd ressource

retrieve_existing_data <- function(messwerte_url = "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004983.csv",
                                   messorte_url = "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv"){


  messwerte_df <- vroom::vroom(messwerte_url)
  messorte_df <- vroom::vroom(messorte_url)


  messwerte_df %>%
    filter(Messintervall == "Einmalige Messung") %>%
    distinct(Messort_Code) %>%
    deframe() -> unique_einzel_list


  messwerte_df %>%
    filter(Messintervall == "Wiederholungsmessung") %>%
    distinct(Messort_Code, Jahr) %>%
    group_by(Messort_Code) %>%
    summarise(Jahr = list(Jahr)) %>%
    deframe() %>%
    map(~ as.character(.x)) -> unique_langzeit_list



  list("Einzelmessungen" = unique_einzel_list, "Langzeitmessungen" = unique_langzeit_list, "Langzeitmessungen_ID_df" = messorte_df)

}



# function to clean einzeldaten
clean_einzel_list <- function(csv_list, id_list) {
  # Convert id_list to numeric for matching
  id_list <- as.numeric(id_list)

  # Extract numeric part from the names of csv_list for comparison
  csv_list_names_numeric <- sapply(names(csv_list), function(name) {
    as.numeric(str_extract(name, "\\d+"))
  })

  # Filter the csv_list to remove elements that match the ids in id_list
  filtered_list <- csv_list[!csv_list_names_numeric %in% id_list]

  return(filtered_list)
}


# function to clean langzeitdaten
clean_nested_list <- function(csv_list, df_nested_list) {
  # Process the names to extract Messort_Code
  names(csv_list) <- str_extract(names(csv_list), "\\d+")

  # Iterate over the csv_list
  csv_list <- imap(csv_list, function(year_list, messort_code) {
    if (messort_code %in% names(df_nested_list)) {
      # Keeping only those years not in df_nested_list
      year_list <- year_list[!names(year_list) %in% df_nested_list[[messort_code]]]
    }
    year_list
  })

  # Remove empty elements
  csv_list[lengths(csv_list) > 0]
}


delta_test <- clean_einzel_list(csv_paths_list_all, test_list$Einzelmessungen[1:200])

report_changes_in_list <- function(original_list, cleaned_list) {
  new_ids <- c()
  ids_with_new_years <- c()
  new_years_summary <- list()
  new_ids_years_summary <- list()

  # Get the list of all IDs from both lists
  all_ids <- unique(c(names(original_list), names(cleaned_list)))

  for (id in all_ids) {
    if (!id %in% names(original_list)) {
      # New ID introduced
      new_ids <- c(new_ids, id)
      new_ids_years_summary[[id]] <- names(cleaned_list[[id]])
    } else if (id %in% names(cleaned_list)) {
      # Compare years for existing IDs
      original_years <- names(original_list[[id]])
      cleaned_years <- names(cleaned_list[[id]])
      new_years <- setdiff(cleaned_years, original_years)

      if (length(new_years) > 0) {
        ids_with_new_years <- c(ids_with_new_years, id)
        new_years_summary[[id]] <- new_years
      }
    }
  }

  # Formatting the output
  if (length(new_ids) > 0) {
    cli::cli_alert_info("Neue Messorte IDs in Ornderstruktur welche noch nicht Teil der OGD Publikation sind:")
    cli::cli_ul()
    for (id in names(new_ids_years_summary)) {
      cli::cli_li("{.strong {id}} mit den Jahren {paste(new_ids_years_summary[[id]], collapse = ', ')}")
    }
    cli::cli_end()
  }
  if (length(ids_with_new_years) > 0) {
    cli::cli_alert_success("Neue Messungen in Ordnerstruktur für die folgenden IDs gefunden:")
    cli::cli_ul()
    for (id in names(new_years_summary)) {
      cli::cli_li("{.strong {id}} für die Jahre {paste(new_years_summary[[id]], collapse = ', ')}")
    }
    cli::cli_end()
  }

  invisible(new_ids)

}


report_new_ids_for_einzel_list <- function(original_list, cleaned_list) {
  # Identifizieren neuer IDs (nicht in original_list, aber in cleaned_list)
  new_ids <- setdiff(names(cleaned_list), names(original_list))

  # Reporting nur für neue IDs
  if (length(new_ids) > 0) {
    cli::cli_alert_info("Neue Messorte IDs in Ordnerstruktur welche noch nicht Teil der OGD Publikation sind:")
    cli::cli_ul()
    for (id in new_ids) {
      cli::cli_li(cli::style_bold(id))
    }
    cli::cli_end()
  } else {
    cli::cli_alert_info("Keine neuen Messorte IDs gefunden.")
  }

  return(new_ids)
}

report_new_ids_for_einzel_list(original_list = csv_paths_list_all, cleaned_list = delta_test)

report_changes_in_list(csv_paths_list_all_langzeit, cleaned_list = delta_test) -> test_return




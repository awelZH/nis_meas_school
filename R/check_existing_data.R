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



  list("Einzelmessungen" = unique_einzel_list, "Langzeitmessungen" = unique_langzeit_list, "Messorte" = messorte_df)

}



# function to clean einzeldaten (only returns Messorte which are "new" in the folder structure)
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


# function to clean langzeitdaten (only returns Messorte/Years which are "new" in the folder structure)
clean_langzeit_list <- function(csv_list, df_nested_list) {
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


# function to report the delta for langzeitdaten
report_delta_langzeit <- function(existing_ogd, delta_langzeit) {
  existing_ids <- names(existing_ogd$Langzeitmessungen)
  delta_ids <- names(delta_langzeit)

  new_ids <- setdiff(delta_ids, existing_ids)
  ids_with_new_years <- character()
  new_years_summary <- list()

  for (id in intersect(delta_ids, existing_ids)) {
    new_years <- setdiff(names(delta_langzeit[[id]]), names(existing_ogd$Langzeitmessungen[[id]]))

    if (length(new_years) > 0) {
      ids_with_new_years <- c(ids_with_new_years, id)
      new_years_summary[[id]] <- new_years
    }
  }

  if (length(new_ids) > 0) {
    cli_alert_info("Neue Messorte IDs in Ordnerstruktur welche noch nicht Teil der OGD Publikation sind:")
    cli_ul()
    cli_li(sprintf("{.strong %s}", paste(new_ids, collapse = ', ')))
    if (length(ids_with_new_years) > 0) {
      cli_text(sprintf("Neue Messungen fuer diese IDs fuer die Jahre %s",
                       paste(sapply(new_ids, function(id) paste(new_years_summary[[id]], collapse = ', ')),
                             collapse = ', ')))
    }
    cli_end()
  }

  if (length(ids_with_new_years) > 0) {
    cli_alert_success("Neue Messungen in Ordnerstruktur fuer die vorhandenen IDs gefunden:")
    cli_ul()
    for (id in ids_with_new_years) {
      cli_li(sprintf("{.strong %s} fuer die Jahre %s", id, paste(new_years_summary[[id]], collapse = ', ')))
    }
    cli_end()
  }

  if (length(new_ids) == 0 && length(ids_with_new_years) == 0) {
    cli_alert_info("Keine neuen Messorte oder Messungen fuer vorhandene IDs in der Ordnerstruktur gefunden.")
  }

  # Create a tibble with the results
  result <- tibble(ID = c(new_ids, ids_with_new_years),
                   New_Measurement_Years = sapply(c(new_ids, ids_with_new_years), function(id) {
                     if (id %in% new_ids) {
                       paste(new_years_summary[[id]], collapse = ', ')
                     } else {
                       paste(new_years_summary[[id]], collapse = ', ')
                     }
                   }),
                   Is_Totally_New = ifelse(ID %in% new_ids, "Totally New", "New Years Only"))

  invisible(result)
}

# function to report the delta for einzelmessungen
report_delta_einzel <- function(existing_ogd, delta_einzel) {
  existing_ids <- names(existing_ogd$Einzelmessungen)
  delta_ids <- names(delta_einzel)

  new_ids <- setdiff(delta_ids, existing_ids)

  if (length(new_ids) > 0) {
    cli_alert_info("Neue Messorte IDs in Ordnerstruktur welche noch nicht Teil der OGD Publikation sind:")
    cli_ul()
    for (new_id in new_ids) {
      cli_li(paste("{.strong", new_id, "}"))
    }
    cli_end()
  }

  # Create a tibble with the results
  result <- tibble(ID = new_ids,
                   Is_Totally_New = "Totally New")

  invisible(result)
}

# Function to check if the online resource has been updated
check_delta_vs_existing <- function(existing_messorte, delta_langzeit, delta_einzel) {
  # Get Messort_Codes from the existing Messorte
  existing_codes <- existing_messorte$Messort_Code

  # Get Messort_Codes from delta_langzeit and delta_einzel
  delta_langzeit_codes <- as.integer(delta_langzeit$ID)
  delta_einzel_codes <- delta_einzel$ID %>%
    str_extract(pattern = "\\d+") %>%
    as.integer()

    # Combine all Messort_Codes from deltas
  all_delta_codes <- c(delta_langzeit_codes, delta_einzel_codes)

  # Check if all delta codes are in the existing codes
  if (all(all_delta_codes %in% existing_codes)) {
    # Success message if all codes match
    cli_alert_success("Die OGD Ressource 'Messorte' scheint aktuell zu sein. Alle neuen Messungen in Ordnerstruktur finden sich in den Messorten wieder.")
  } else {
    # Error message if there are unmatched codes
    cli_alert_error_message("Die OGD Ressource 'Messorte' scheint NICHT aktuell zu sein. Es gibt neue Dateien in der Ordnerstruktur welche keine Messort_ID zugeordnet haben in der OGD Ressource. Bitte OGD Ressource anpassen bevor ein delta-load vorgenommen wird.")
  }
}

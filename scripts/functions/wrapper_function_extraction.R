extract <- function(delta_load = TRUE,
                    full_load = FALSE,
                    path_rohdaten_topfolder,
                    url_ogd_messwerte = "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004983.csv",
                    url_ogd_messorte = "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv") {
  # check for sanity of arguments
  assert_that(!are_equal(delta_load, full_load), msg = "delta_load und full_load können nicht gleichen Wert (true/false) haben!")

  assert_that(is.readable(path_rohdaten_topfolder), msg = "Pfad zu Rohdaten ist nicht vorhanden oder kein Zugriff!")


  if (delta_load == TRUE) {
    # check existing ogd ressources for Messorte & Jahre

    existing_ogd_list <- retrieve_existing_data(messwerte_url = url_ogd_messorte,
                                                messorte_url = url_ogd_messorte)

    # state success!
    cli::cli_alert_success("Existierende OGD Ressourcen heruntergeladen!")

    csv_paths_list_all_einzel <- extract_csv_paths_einzel(top_folder_path = "Schulhausmessungen", num_dirs = Inf) %>%
      filter_csv_files_einzel()

    # state success!
    cli::cli_alert_success("Pfade zu Einzelmessungsdaten wurden eingelesen!")


    csv_paths_list_all_langzeit <- extract_csv_paths_langzeit(top_folder_path = "Langzeit-Messungen", num_dirs = Inf) %>%
      filter_csv_files_langzeit()

    # state success!
    cli::cli_alert_success("Pfade zu Wiederholungsmessungsdaten wurden eingelesen!")


    # keep only the delta (Deleta all Messorte IDs and Years which are already present in the data)
    delta_paths_einzel <- clean_einzel_list(csv_list = csv_paths_list_all_einzel, id_list = existing_ogd_list$Einzelmessungen)


    delta_paths_langzeit <- clean_nested_list(csv_list = csv_paths_list_all_langzeit, df_nested_list = existing_ogd_list$Langzeitmessungen)

    # state success!
    cli::cli_alert_success("Delta bestimmt und Pfade bereinigt!")

  #TODO: checks and reports with Messorte_DF, afterwards: processing, saving in right format
    #TODO: code in check_existing_data.R anpassen für delta checks



  }

}

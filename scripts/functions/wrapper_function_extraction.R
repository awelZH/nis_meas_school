extract <- function(delta_load = TRUE,
                    full_load = FALSE,
                    path_rohdaten_topfolder,
                    url_ogd_messwerte = "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004983.csv",
                    url_ogd_messorte = "https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv") {
  # check for sanity of arguments
  assert_that(!are_equal(delta_load, full_load), msg = "delta_load und full_load können nicht gleichen Wert (true/false) haben!")

  assert_that(is.readable(path_rohdaten_topfolder), msg = "Pfad zu Rohdaten ist nicht vorhanden oder kein Zugriff!")

  assert_that(is.writeable("data/temp/extract/"), msg = "Kann den Ordner 'data/temp/extract/ nicht öffnen. Ordner wird benötigt um Daten zwischenzuspeichern.")



  if (delta_load == TRUE) {
    # check existing ogd ressources for Messorte & Jahre
    browser()

    cli::cli_alert_info("Delta Load wird durchgeführt.")

    existing_ogd_list <- retrieve_existing_data(messwerte_url = url_ogd_messwerte,
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


    delta_paths_langzeit <- clean_langzeit_list(csv_list = csv_paths_list_all_langzeit, df_nested_list = existing_ogd_list$Langzeitmessungen)

    # state success!
    cli::cli_alert_success("Delta bestimmt und Pfade bereinigt!")

    # check if there is new data (delta exists)
    if(length(delta_paths_einzel) == 0 && length(delta_paths_langzeit) == 0){
      cli::cli_abort("Kein Delta gefunden! Es scheint keine neuen Messungen in der Ordnerstruktur zu geben. Funktion bricht ab.")
    }

    # report the delta
    delta_report_einzel <- report_delta_einzel(existing_ogd, delta_paths_einzel)
    delta_report_langzeit <- report_delta_langzeit(existing_ogd, delta_paths_langzeit)



    # check the delta (and fail informatively if Messorte.csv OGD ressource is not up to date)
    check_delta_vs_existing(existing_ogd$Messorte, delta_report_langzeit, delta_report_einzel)

    # IF checks ran through: process the csv paths and read in data
    measurements_data_einzel <- process_csv_data_einzel(delta_paths_einzel)

    measurements_data_langzeit <- process_csv_data_langzeit(delta_paths_langzeit)


    # check for NA in measurement variable
    check_na_einzelmessungen(measurements_data_einzel)
    check_na_langzeitmessungen(measurements_data_langzeit)


    # bind Einzelmessungen and Langzeitmessungen together

    einzelmessungen_processed <- measurements_data_einzel %>%
      map(~ .x %>%
            select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)) %>%
      bind_rows()

    langzeitmessungen_processed <- measurements_data_langzeit %>%
      map(\(messort_list) {
        map(messort_list, \(year_list) {
          map(year_list, \(df) {
            if (is.data.frame(df)) {
              df %>%
                select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)
            }
          }) %>%
            bind_rows()
        }) %>%
          bind_rows()
      }) %>%
      bind_rows()

    # combine Langzeitmessungen & Einzelmessungen
    combined_data <- bind_rows(einzelmessungen_processed, langzeitmessungen_processed)


    # write data to data/temp/extract/rohdaten_messwerte.csv

    vroom::vroom_write(x = combined_data, file = "data/temp/extract/rohdaten_messwerte.csv", delim = ",")

    cli::cli_alert_success("Daten wurden in 'data/temp/extract/rohdaten_messwerte.csv' abgespeichert.")

  }


  if(full_load == TRUE){

    cli::cli_alert_info("Full Load wird durchgeführt.")

    csv_paths_list_all_einzel <- extract_csv_paths_einzel(top_folder_path = "Schulhausmessungen", num_dirs = Inf) %>%
      filter_csv_files_einzel()

    # state success!
    cli::cli_alert_success("Pfade zu Einzelmessungsdaten wurden eingelesen!")


    csv_paths_list_all_langzeit <- extract_csv_paths_langzeit(top_folder_path = "Langzeit-Messungen", num_dirs = Inf) %>%
      filter_csv_files_langzeit()

    # state success!
    cli::cli_alert_success("Pfade zu Wiederholungsmessungsdaten wurden eingelesen!")


    # process the csv paths and read in data
    measurements_data_einzel <- process_csv_data_einzel(csv_paths_list_all_einzel)

    measurements_data_langzeit <- process_csv_data_langzeit(csv_paths_list_all_langzeit)

    # check for NA in measurement variable
    check_na_einzelmessungen(measurements_data_einzel)
    check_na_langzeitmessungen(measurements_data_langzeit)


    # bind Einzelmessungen and Langzeitmessungen together

    einzelmessungen_processed <- measurements_data_einzel %>%
      map(~ .x %>%
            select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)) %>%
      bind_rows()

    langzeitmessungen_processed <- measurements_data_langzeit %>%
      map(\(messort_list) {
        map(messort_list, \(year_list) {
          map(year_list, \(df) {
            if (is.data.frame(df)) {
              df %>%
                select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)
            }
          }) %>%
            bind_rows()
        }) %>%
          bind_rows()
      }) %>%
      bind_rows()

    # combine Langzeitmessungen & Einzelmessungen
    combined_data <- bind_rows(einzelmessungen_processed, langzeitmessungen_processed)


    # write data to data/temp/extract/rohdaten_messwerte.csv

    vroom::vroom_write(x = combined_data, file = "data/temp/extract/rohdaten_messwerte.csv", delim = ",")

    cli::cli_alert_success("Daten wurden in 'data/temp/extract/rohdaten_messwerte.csv' abgespeichert.")
  }

}

#' function to start the whole extraction process
#'
#' @param delta_load boolean indicating if a delta load should be performed
#' @param full_load boolean indicating if a full load should be performed
#' @param path_rohdaten_topfolder character vector (string) pointing to folder where Measurement data is stored on local machine
#' @param url_ogd_messwerte character vector (string) representing URL to OGD Messwerte Ressource
#' @param url_ogd_messorte character vector (string) representing URL to OGD Messorte Ressource
#'
#' @return writes rohdaten_messwerte.csv into inst/extdata/temp/extract/
#' @export
#'
#'
extract <- function(delta_load = TRUE,
                    full_load = FALSE,
                    path_rohdaten_topfolder,
                    url_ogd_messwerte = "https://daten.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004983.csv",
                    url_ogd_messorte = "https://daten.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv") {
  # check for sanity of arguments
  assertthat::assert_that(!assertthat::are_equal(delta_load, full_load), msg = "delta_load und full_load koennen nicht gleichen Wert (true/false) haben!")

  assertthat::assert_that(assertthat::is.readable(path_rohdaten_topfolder), msg = "Pfad zu Rohdaten ist nicht vorhanden oder kein Zugriff!")

  assertthat::assert_that(assertthat::is.readable("inst/extdata/temp/extract/"), msg = "Kann den Ordner 'inst/extdata/temp/extract/ nicht oeffnen. Ordner wird benoetigt um Daten zwischenzuspeichern.")



  if (delta_load == TRUE) {
    # check existing ogd ressources for Messorte & Jahre


    cli::cli_alert_info("Delta Load wird durchgefuehrt.")

    existing_ogd_list <- retrieve_existing_data(messwerte_url = url_ogd_messwerte,
                                                messorte_url = url_ogd_messorte)

    # state success!
    cli::cli_alert_success("Existierende OGD Ressourcen heruntergeladen!")

    csv_paths_list_all_einzel <- extract_csv_paths_einzel(top_folder_path = fs::path(path_rohdaten_topfolder, "Schulhausmessungen"), num_dirs = Inf) %>%
      filter_csv_files_einzel()

    # state success!
    cli::cli_alert_success("Pfade zu Einzelmessungsdaten wurden eingelesen!")


    csv_paths_list_all_langzeit <- extract_csv_paths_langzeit(top_folder_path = fs::path(path_rohdaten_topfolder, "Langzeit-Messungen"), num_dirs = Inf) %>%
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
    delta_report_einzel <- report_delta_einzel(existing_ogd_list, delta_paths_einzel)
    delta_report_langzeit <- report_delta_langzeit(existing_ogd_list, delta_paths_langzeit)



    # check the delta (and fail informatively if Messorte.csv OGD ressource is not up to date)
    check_delta_vs_existing(existing_ogd_list$Messorte, delta_report_langzeit, delta_report_einzel)

    # IF checks ran through: process the csv paths and read in data
    measurements_data_einzel <- process_csv_data_einzel(delta_paths_einzel)

    measurements_data_langzeit <- process_csv_data_langzeit(delta_paths_langzeit)


    # check for NA in measurement variable
    check_na_einzelmessungen(measurements_data_einzel)
    check_na_langzeitmessungen(measurements_data_langzeit)


    # bind Einzelmessungen and Langzeitmessungen together

    einzelmessungen_processed <- measurements_data_einzel %>%
      purrr::map(~ .x %>%
            dplyr::select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)) %>%
      dplyr::bind_rows()

    langzeitmessungen_processed <- measurements_data_langzeit %>%
      purrr::map(\(messort_list) {
        purrr::map(messort_list, \(year_list) {
          purrr::map(year_list, \(df) {
            if (is.data.frame(df)) {
              df %>%
                dplyr::select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)
            }
          }) %>%
            dplyr::bind_rows()
        }) %>%
          dplyr::bind_rows()
      }) %>%
      dplyr::bind_rows()

    # combine Langzeitmessungen & Einzelmessungen
    combined_data <- dplyr::bind_rows(einzelmessungen_processed, langzeitmessungen_processed) %>%
      dplyr::select(Zeitstempel,Messort_Code,fmin_hz,fmax_hz,service_name,value_v_m) %>% # change order of columns
      dplyr::rename(Fmin_Hz = fmin_hz, Fmax_Hz = fmax_hz, Service_Name = service_name, Value_V_per_m = value_v_m)


    # write data to inst/extdata/temp/extract/rohdaten_messwerte.csv

    readr::write_delim(x = combined_data, file = "inst/extdata/temp/extract/rohdaten_messwerte.csv", delim = ",")

    cli::cli_alert_success("Daten wurden in 'inst/extdata/temp/extract/rohdaten_messwerte.csv' abgespeichert.")

  }


  if(full_load == TRUE){

    cli::cli_alert_info("Full Load wird durchgefuehrt.")

    csv_paths_list_all_einzel <- extract_csv_paths_einzel(top_folder_path = fs::path(path_rohdaten_topfolder, "Schulhausmessungen"), num_dirs = Inf) %>%
      filter_csv_files_einzel()

    # state success!
    cli::cli_alert_success("Pfade zu Einzelmessungsdaten wurden eingelesen!")


    csv_paths_list_all_langzeit <- extract_csv_paths_langzeit(top_folder_path = fs::path(path_rohdaten_topfolder, "Langzeit-Messungen"), num_dirs = Inf) %>%
      filter_csv_files_langzeit()

    # state success!
    cli::cli_alert_success("Pfade zu Wiederholungsmessungsdaten wurden eingelesen!")

    # check if OGD Messorte & IDs in folder match -> break if not
    check_full_load(csv_paths_list_all_einzel, csv_paths_list_all_langzeit)

    # process the csv paths and read in data
    measurements_data_einzel <- process_csv_data_einzel(csv_paths_list_all_einzel)


    measurements_data_langzeit <- process_csv_data_langzeit(csv_paths_list_all_langzeit)

    # check for NA in measurement variable
    check_na_einzelmessungen(measurements_data_einzel)
    check_na_langzeitmessungen(measurements_data_langzeit)


    # bind Einzelmessungen and Langzeitmessungen together

    einzelmessungen_processed <- measurements_data_einzel %>%
      purrr::map(~ .x %>%
            dplyr::select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)) %>%
      dplyr::bind_rows()

    langzeitmessungen_processed <- measurements_data_langzeit %>%
      purrr::map(\(messort_list) {
        purrr::map(messort_list, \(year_list) {
          purrr::map(year_list, \(df) {
            if (is.data.frame(df)) {
              df %>%
                dplyr::select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)
            }
          }) %>%
            dplyr::bind_rows()
        }) %>%
          dplyr::bind_rows()
      }) %>%
      dplyr::bind_rows()

    # combine Langzeitmessungen & Einzelmessungen
    combined_data <- dplyr::bind_rows(einzelmessungen_processed, langzeitmessungen_processed) %>%
      dplyr::select(Zeitstempel,Messort_Code,fmin_hz,fmax_hz,service_name,value_v_m) %>% # change order of columns
      dplyr::rename(Fmin_Hz = fmin_hz, Fmax_Hz = fmax_hz, Service_Name = service_name, Value_V_per_m = value_v_m)


    # write data to inst/extdata/temp/extract/rohdaten_messwerte.csv

    readr::write_delim(x = combined_data, file = "inst/extdata/temp/extract/rohdaten_messwerte.csv", delim = ",")

    cli::cli_alert_success("Daten wurden in 'inst/extdata/temp/extract/rohdaten_messwerte.csv' abgespeichert.")
  }

}

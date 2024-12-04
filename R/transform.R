#' Transform
#'
#' Erstelle die beiden Messwerte OGD Files (rohdaten_messwerte.zip und aufbereitete_messwerte.csv)
#' fuer die Visualisierung der Immissionsmessungen NIS im Kanton Zuerich als OGD
#'
#' @param full_load Boolean. Ob Full-Load ausgefuehrt werden sollte
#'
#'@export
#'
transform <- function(full_load = TRUE){

  ##---------------------------------------------------------------------------
  # 0. Parameter setzen und ueberpruefen
  ##---------------------------------------------------------------------------
  cli::cli_alert_info("Starte Transform Prozess")

  # Verhindere wissenschaftliche Darstellung von Zahlen
  options(scipen=999)

  # Datenpfad als String zum Rohdaten File. Dieses File wurde im vorherigen Schritt erzeugt.
  path_rohdaten_messwerte = "inst/extdata/temp/extract/rohdaten_messwerte.csv"
  #URL zum Rohdaten Messwerte Zip File (OGD) als String
  url_rohdaten_messwerte <- 'https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00005003.zip'
  # URL zum Schwellenwert File als String
  path_schwellenwerte <- 'inst/extdata/frequenzbaender_schwellenwerte.csv'
  # URL zum Messorte OGD File als String
  url_messorte = 'https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv'
  type_rolling_mean <- 'center' # Wie der Rolling mean berechnet wird
  rolling_mean_breite <- 60 # Wie viele Wertepro Wert im rolling mean beruecksichtigt werden
  Einheit_kurz <- "V/m" #Einheit der zu darzustellenden Werte (SI Einheiten) als String
  Einheit_lang <- "Volt pro Meter" #Einheit der zu darzustellenden Werte (ausgeschrieben) als String
  # Datenpfad als String wo alle Input Files von diesem Skript gespeichert werden
  path_to_transform_folder <- "inst/extdata/temp/transform/"
  # Datenpfad als String wo alle Export Files von diesem Skript gespeichert werden
  path_to_load_folder <- "inst/extdata/temp/load/"
  # Dateiname als String zum Rohdaten Messwerte File.
  aufbereite_messwerte_ogd_filename <- 'aufbereitete_messwerte'
  # Dateiname als String zum Rohdaten Messwerte File.
  rohdaten_messwerte_ogd_filename <- 'rohdaten_messwerte'
  path_to_rohdaten_folder <- 'inst/extdata/temp/transform/rohdaten/'

  # ueberpruefe, ob die Parameter den richtigen Typ haben. Ansonsten breche ab und werfe Fehlermeldung
  assertthat::assert_that(msg = "path_rohdaten_messwerte` must be a character" , is.character(path_rohdaten_messwerte))
  assertthat::assert_that(msg = "url_rohdaten_messwerte` must be a character" , is.character(url_rohdaten_messwerte))
  assertthat::assert_that(msg = "path_schwellenwerte` must be a character" , is.character(path_schwellenwerte))
  assertthat::assert_that(msg = "url_messorte` must be a character." , is.character(url_messorte))
  assertthat::assert_that(msg = "type_rolling_mean` must be a character." , is.character(type_rolling_mean))
  assertthat::assert_that(msg = "rolling_mean_breite` must be numeric" , is.numeric(rolling_mean_breite))
  assertthat::assert_that(msg = "Einheit_kurz` must be a character." , is.character(Einheit_kurz))
  assertthat::assert_that(msg = "Einheit_lang` must be a character." , is.character(Einheit_lang))
  assertthat::assert_that(msg = "path_to_transform_folder` must be a character." , is.character(path_to_transform_folder))
  assertthat::assert_that(msg = "aufbereite_messwerte_ogd_filename` must be a character." , is.character(aufbereite_messwerte_ogd_filename))
  assertthat::assert_that(msg = "rohdaten_messwerte_ogd_filename` must be a character." , is.character(rohdaten_messwerte_ogd_filename))
  assertthat::assert_that(msg = "path_to_rohdaten_folder` must be a character." , is.character(path_to_rohdaten_folder))

  # ueberpruefe, ob das Transform Verzeichnis verfuegbar ist
  if(!dir.exists(path_to_transform_folder)){
    cli::cli_abort(paste0(path_to_transform_folder, " Verzeichnis nicht verfuegbar oder der Zugriff auf das Verzeichnis ist nicht moeglich"))
  }

  # ueberpruefe, ob das Load Verzeichnis verfuegbar ist
  if(!dir.exists(path_to_load_folder)){
    cli::cli_abort(paste0(path_to_load_folder, " Verzeichnis nicht verfuegbar oder der Zugriff auf das Verzeichnis ist nicht moeglich"))
  }

  # Lade Schwellenwerte File und lade es in ein Data Frame
  if(file.exists(path_schwellenwerte)){
    df_schwellenwerte <- readr::read_csv(path_schwellenwerte, show_col_types = FALSE) # Lade Schwellenwerte File
  }else{
    cli::cli_abort("Schwellenwerte File nicht verfuegbar oder der Zugriff auf das File ist nicht moeglich")
  }

  ##---------------------------------------------------------------------------
  # 1. Lade benoetigte Daten und erstelle Zip OGD File
  ##---------------------------------------------------------------------------

  # Lade lokales Rohdaten CSV File und lade es in ein Data Frame
  if(file.exists(path_rohdaten_messwerte)){
    cli::cli_alert_info("Rohdaten CSV (lokal) wird eingelesen:")
    df_rohdaten_raw <- utils::read.csv(path_rohdaten_messwerte) # Lade Rohdaten File, welches als Delta oder Full Load vorliegt
  }else{
    cli::cli_abort("Kein lokales Rohdaten CSV File verfuegbar oder der Zugriff auf das File ist nicht moeglich")
  }

  # Lade Rohdaten OGD ZIP File und lade es in ein Data Frame
  if(check_file_availability(url_rohdaten_messwerte)){
    # Lade OGD Rohdaten und speichere sie als CSV
    utils::download.file(url_rohdaten_messwerte, paste0(path_to_transform_folder, "rohdaten_messwerte.zip"))
    df_rohdaten_messwerte_ogd <- readr::read_csv(archive::archive_read(paste0(path_to_transform_folder, rohdaten_messwerte_ogd_filename, ".zip")), show_col_types = FALSE)

  }else{
    cli::cli_abort("Kein OGD Rohdaten File verfuegbar oder der Zugriff auf das File ist nicht moeglich")
  }

  # Lade Messorte OGD File und lade es in ein Data Frame
  if(check_file_availability(url_messorte)){
    df_messorte <- utils::read.csv(url_messorte) # Lade Messorte File
  }else{
    cli::cli_abort("Messorte File nicht verfuegbar oder der Zugriff auf das File ist nicht moeglich")
  }

  # Erstelle Rohdaten dataframe. Wenn Full-Load ausgewaehlt, wird das geschriebene CSV aus dem Extract Skript direkt als Dataframe geladen
  if(full_load == TRUE){
    df_rohdaten <- df_rohdaten_raw %>%
      dplyr::arrange(Messort_Code, Zeitstempel)
  }else{
    # Fuege OGD und prozessiertes Rohdaten Messwerte File zusammen
    df_rohdaten <- rbind(df_rohdaten_raw, df_rohdaten_messwerte_ogd) %>%
      dplyr::distinct() %>%
      dplyr::arrange(Messort_Code, Zeitstempel)
  }

  cli::cli_alert_success("Rohdaten Zip (OGD) wurden erfolgreich eingelesen und mit dem lokalen Rohdaten zu einem Dataset zusammengefuegt")
  cli::cli_alert_info("Datum/Uhrzeit Spalten werden bereinigt:")

  ##---------------------------------------------------------------------------
  # 2. Schwellenwert-Bereinigung:
  ##---------------------------------------------------------------------------

  #Bereinige Datum und Uhrzeit Spalten

  #Zuerst werden Datum und Uhrzeit getrennt
  temp <- tidyr::separate(df_rohdaten, Zeitstempel, into = c("Datum", "Uhrzeit"), sep = "T")

  # Da die Rohdaten verschiedene Formate in den Zeitstempel haben, werden mehrere Schritte ausgefuehrt, um am Schluss ein einheitliches Datum Uhrzeit Format fuer alle zu erhalten
  temp$datum1 <- as.Date(suppressWarnings(lubridate::parse_date_time(temp$Datum, c("dmY", "mdY")))) # Mit diesem Code werden die meisten Formate erkennt.
  #Ein Format funktioniert nicht und muss im naechsten Schritt verarbeitet werden.
  temp$datum_corr <- as.Date(ifelse(is.na(temp$datum1), as.character(as.Date(temp$Datum, format = "%d.%m.%y")), as.character(temp$datum1)))

  # Erstelle eine Jahresspalte
  temp$Jahr <- format(temp$datum_corr, "%Y")

  #Fuege die korrigierte Datumspalte mit der Uhrzeitspalte zusammen um ein Timestamp zu erhalten.
  temp$Zeitstempel_corr <- paste0(as.character(temp$datum_corr), "T", as.character(temp$Uhrzeit))

  #Waehle die relevanten Spalten aus
  temp <- temp[c("Fmin_Hz", "Fmax_Hz", "Service_Name", "Value_V_per_m", "Messort_Code", "Jahr", "datum_corr", "Zeitstempel_corr")]

  # Formatiere die Datumspalten im Schwellenwerte Dataframe zu Datum
  df_schwellenwerte$gueltig_von <- as.Date(df_schwellenwerte$gueltig_von, "%d.%m.%Y")
  df_schwellenwerte$gueltig_bis <- as.Date(df_schwellenwerte$gueltig_bis, "%d.%m.%Y")

  # In der Spalte gueltig_bis koennen NA vorkommen. Dort wo NA vorkommen, heisst das, das die Schwellenwerte immer noch gueltig sind. Damit der spaetere Join funktioniert,
  # werden die NA mit dem aktuellen DAtum ueberschrieben.
  df_schwellenwerte <- df_schwellenwerte %>%
    dplyr::mutate(gueltig_bis = dplyr::if_else(is.na(gueltig_bis), Sys.Date(), gueltig_bis))
  # Ergaenzung GMA, 2024-12-04: ab Juni 2024 gibt es drei zusätzliche Eintraege:
  # Amateur/ISM433 | PMR/PAMR | Wetter Radar CH -> diese drei Eintraege werden vorderhand nicht mitberücksichtigt
  df_schwellenwerte <- df_schwellenwerte %>%
    filter(!is.na(Kategorie))

  cli::cli_alert_success("Bereinigung der Datum Uhrzeit Spalten wurde erfolgreich durchgefuehrt")

  # Merge Rohdaten mit Schwellenwerte Daten. Da ein Inner join gemacht wird, bleiben nur die Zeilen uebrig, welche in beiden Dataframe vorkommen. Damit werden z.B alte Services wie "Others I",
  # ausgeschlossen.
  by <- dplyr::join_by(Fmin_Hz == Freq_min, Fmax_Hz == Freq_max, between(datum_corr, gueltig_von, gueltig_bis))
  df_merged <- dplyr::inner_join(temp, df_schwellenwerte, by)

  # Zeige Anzahl nicht erfolgreiche Joins dem User an:
  number_of_anti_joins <- nrow(dplyr::anti_join(temp, df_schwellenwerte, by))

  # Speichere Kombinationen von Frequenzen und Jahr in einem Dataframe. Wird nicht fuer das Ausfuehren dieses Skript gebraucht, kann aber fuer Debug Zwecke gebraucht werden.
  dataset <- dplyr::anti_join(temp, df_schwellenwerte, by) %>%
    dplyr::distinct(Fmin_Hz, Fmax_Hz, Service_Name, Jahr, Messort_Code) %>%
    dplyr::arrange(Fmin_Hz, Jahr)

  #Exportiere die Kombinationen in ein File. Damit kann später herausgefunden werden, für welche Kombinationen es keinen Match gegeben hat.
  sink('nicht_prozessierte_messwerte.txt')
  print(dataset)
  sink()

  # Speichere Daten in Rohdaten Dataframe. Dieses wird im letzten Schritt dann zu dem Zip verarbeitet.
  df_rohdaten <- df_merged[c("Zeitstempel_corr", "Messort_Code", "Fmin_Hz", "Fmax_Hz", "Service_Name", "Value_V_per_m")] %>%
    dplyr::rename("Zeitstempel" = "Zeitstempel_corr") %>%
    dplyr::arrange(Messort_Code, Zeitstempel)

  cli::cli_alert_info(paste0(number_of_anti_joins, " Messwerten konnte keine Schwellenwerte hinzugefuegt werden."))
  cli::cli_alert_success("Schwellenwerte wurden erfolgreich den Messwerten hinzugefuegt", )


  # Fuehre Schwellenwertkorrektur durch
  df_merged$Value_V_per_m_corrected <- df_merged$Value_V_per_m - df_merged$Schwellenwert_MR_Vm

  # Ersetze die korrgierten Werte mit 0, wenn sie kleiner als 0 sind
  df_merged$Value_V_per_m_corrected <- ifelse(df_merged$Value_V_per_m_corrected < 0, 0, df_merged$Value_V_per_m_corrected)

  cli::cli_alert_success("Schwellenwertbereinigung erfolgreich durchgefuehrt")

  ##---------------------------------------------------------------------------
  # 3. Berechne rollierende Mittelwerte
  ##---------------------------------------------------------------------------

  # Erstelle neues Dataframe, wo die einzelnen Frequenzbaender in die Kategorien quadratisch summiert und danach die Wurzel gezogen wird.

  # Berechne rollierender Mittelwert
  df_grouped_rolling <- df_merged %>%
    dplyr::group_by(Jahr, Messort_Code, Kategorie, Zeitstempel_corr) %>%
    # summiert die quadrierten Argumente & gibt die Quadratwurzel einer Zahl zurueck
    dplyr::summarise(value_grouped = sqrt(sum(Value_V_per_m_corrected^2)), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Jahr, Messort_Code, Kategorie) %>% # Groupiere und berechne den rolling mean im naechsten Schritt pro Gruppe
    dplyr::mutate(sixmin_avg = zoo::rollapply(value_grouped, rolling_mean_breite ,mean,align=type_rolling_mean,fill=NA))


  # Bilde Mittelwerte
  df_grouped_max <- df_grouped_rolling %>%
    dplyr::rename('Service' = "Kategorie") %>%
    dplyr::group_by(Jahr, Messort_Code, Service) %>%
    dplyr::summarise(Wert = max(sixmin_avg, na.rm = TRUE), .groups = "drop")


  # Joine Information fuer finales Dataframe
  df_final <- merge(df_grouped_max, df_messorte[c('Messort_Code', 'Messort_Name', 'Messintervall')], by.x = 'Messort_Code', by.y = 'Messort_Code') %>%
    dplyr::mutate(Einheit_kurz = Einheit_kurz,
           Einheit_lang = Einheit_lang,
           Messgeraet_Typ = "SRM 3006") %>%
    dplyr::select(Jahr, Messort_Code, Messort_Name, Service, Wert, Einheit_kurz, Einheit_lang, Messintervall, Messgeraet_Typ)

  cli::cli_alert_success("Berechnung durchgefuehrt und weitere Informationen wurden hinzugefuegt")

  ##---------------------------------------------------------------------------
  # 4. Speichere Daten als CSV und Zip
  ##---------------------------------------------------------------------------

  #Exportiere die Kombinationen in ein File. Damit kann später herausgefunden werden, welche Messorte prozessiert und als OGD publiziert wurden.
  sink('prozessierte_messorte.txt')
  print(unique(df_final[c("Messort_Code", "Messort_Name")]))
  sink()

  cli::cli_alert_info("Speichere Rohdaten lokal:")
  #Erstelle temporaeres Verzeichnis und speichere CSV in Verzeichnis. Dieses Verzeichnis wird danach gezippt
  dir.create(paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename), recursive = TRUE, showWarnings = FALSE)

  # Speichere alle Rohdaten in ein CSV File in den temporaeren Ordner, welcher spaeter gezippt wird.
  readr::write_csv(df_rohdaten, file = paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename, "/0_alle_rohdaten", ".csv"), )

  # Teile dataframe in nested dataframe
  split_df <- split(df_rohdaten, list(df_rohdaten$Messort_Code))

  # Speichere zusaetzlich die Rohdaten pro Messort in ein CSV-File in den temporaeren Ordner, welcher spaeter gezippt wird.
  for (Messort_Code in  cli::cli_progress_along(names(split_df))) {
    Messort_Name <- df_messorte[df_messorte$Messort_Code == Messort_Code, 2]
    readr::write_csv(split_df[[Messort_Code]], file = paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename, "/", Messort_Code, "_", Messort_Name, ".csv"), )
  }
  cli::cli_progress_done()

  # Speichere Rohdaten File als Zip
  archive::archive_write_dir(archive = paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename, ".zip"), dir = paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename))

  # Loesche temporaerer Ordner
  unlink(paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename), recursive = TRUE)

  # Speichere aufbereites messwerte File als CSV
  readr::write_csv(df_final, file = paste0(path_to_load_folder, aufbereite_messwerte_ogd_filename, ".csv"))

  cli::cli_alert_success("aufbereitete_messwerte.csv wurde lokal gespeichert")

  cli::cli_alert_success("rohdaten_messwerte.zip wurde lokal gespeichert")
  cli::cli_alert_success("Transform Skript ist erfolgreich durchgelaufen")

  ##---------------------------------------------------------------------------

}

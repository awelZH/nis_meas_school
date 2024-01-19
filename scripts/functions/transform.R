#' Erstelle die beiden Messwerte OGD Files (rohdaten_messwerte.zip und aufbereitete_messwerte.csv)
#' für die Visualisierung der Immissionsmessungen NIS im Kanton Zürich als OGD

#'
#' @examples transform()
transform <- function(){


  ##---------------------------------------------------------------------------
  # 0. Parameter setzen und überprüfen
  ##---------------------------------------------------------------------------
  cli::cli_alert_info("Starte Transform Prozess")

  # Datenpfad als String zum Rohdaten File. Dieses File wurde im vorherigen Schritt erzeugt.
  path_rohdaten_messwerte = "data/temp/extract/rohdaten_messwerte.csv"
  #URL zum Rohdaten Messwerte Zip File (OGD) als String
  url_rohdaten_messwerte <- 'https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00005003.zip'
  # URL zum Schwellenwert File als String
  path_schwellenwerte <- 'data/frequenzbaender_schwellenwerte.csv'
  # URL zum Messorte OGD File als String
  url_messorte = 'https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv'
  type_rolling_mean <- 'center' # Wie der Rolling mean berechnet wird
  rolling_mean_breite <- 60 # Wie viele Wertepro Wert im rolling mean berücksichtigt werden
  Einheit_kurz <- "V/m" #Einheit der zu darzustellenden Werte (SI Einheiten) als String
  Einheit_lang <- "Volt pro Meter" #Einheit der zu darzustellenden Werte (ausgeschrieben) als String
  # Datenpfad als String wo alle Input Files von diesem Skript gespeichert werden
  path_to_transform_folder <- "data/temp/transform/"
  # Datenpfad als String wo alle Export Files von diesem Skript gespeichert werden
  path_to_load_folder <- "data/temp/load/"
  # Dateiname als String zum Rohdaten Messwerte File.
  aufbereite_messwerte_ogd_filename <- 'aufbereitete_messwerte'
  # Dateiname als String zum Rohdaten Messwerte File.
  rohdaten_messwerte_ogd_filename <- 'rohdaten_messwerte'
  path_to_rohdaten_folder <- 'data/temp/transform/rohdaten/'

  # Überprüfe, ob die Parameter den richtigen Typ haben. Ansonsten breche ab und werfe Fehlermeldung
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

  # Überprüfe, ob das Transform Verzeichnis verfügbar ist
  if(!dir.exists(path_to_transform_folder)){
    cli::cli_abort(paste0(path_to_transform_folder, " Verzeichnis nicht verfügbar oder der Zugriff auf das Verzeichnis ist nicht möglich"))
  }

  # Überprüfe, ob das Load Verzeichnis verfügbar ist
  if(!dir.exists(path_to_load_folder)){
    cli::cli_abort(paste0(path_to_load_folder, " Verzeichnis nicht verfügbar oder der Zugriff auf das Verzeichnis ist nicht möglich"))
  }

  # Lade Schwellenwerte File und lade es in ein Data Frame
  if(file.exists(path_schwellenwerte)){
    df_schwellenwerte <- readr::read_csv(path_schwellenwerte, show_col_types = FALSE) # Lade Schwellenwerte File
  }else{
    cli::cli_abort("Schwellenwerte File nicht verfügbar oder der Zugriff auf das File ist nicht möglich")
  }

   ##---------------------------------------------------------------------------
  # 1. Lade benötigte Daten und erstelle Zip OGD File
  ##---------------------------------------------------------------------------

  # Lade lokales Rohdaten CSV File und lade es in ein Data Frame
  if(file.exists(path_rohdaten_messwerte)){
    cli::cli_alert_info("Rohdaten CSV (lokal) wird eingelesen")
    df_rohdaten_raw <- read.csv(path_rohdaten_messwerte) # Lade Rohdaten File, welches als Delta oder Full Load vorliegt
  }else{
    cli::cli_abort("Kein lokales Rohdaten CSV File verfügbar oder der Zugriff auf das File ist nicht möglich")
  }

  # Lade Rohdaten OGD ZIP File und lade es in ein Data Frame
  if(check_file_availability(url_rohdaten_messwerte)){
    # Lade OGD Rohdaten und speichere sie als CSV
    download.file(url_rohdaten_messwerte, paste0(path_to_transform_folder, "rohdaten_messwerte.zip"))
    df_rohdaten_messwerte_ogd <- readr::read_csv(archive::archive_read(paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename, ".zip")), show_col_types = FALSE)

  }else{
    cli::cli_abort("Kein OGD Rohdaten File verfügbar oder der Zugriff auf das File ist nicht möglich")
  }

  # Lade Messorte OGD File und lade es in ein Data Frame
  if(check_file_availability(url_messorte)){
    df_messorte <- read.csv(url_messorte) # Lade Messorte File
  }else{
    cli::cli_abort("Messorte File nicht verfügbar oder der Zugriff auf das File ist nicht möglich")
  }

  # Erstelle Rohdaten Zip OGD File. Wird aus dem vom vorherigen Skript erzeugten CSV und dem OGD ZIP File erstellt.
  # Füge OGD und prozessiertes Rohdaten Messwerte File zusammen
  df_rohdaten <- rbind(df_rohdaten_raw, df_rohdaten_messwerte_ogd) %>%
    dplyr::distinct()

  cli::cli_alert_success("Rohdaten Zip (OGD) wurden erfolgreich eingelesen und mit dem lokalen Rohdaten zu einem Dataset zusammengefügt")


  ##---------------------------------------------------------------------------
  # 2. Schwellenwert-Bereinigung:
  ##---------------------------------------------------------------------------

  #Bereinige Datum und Uhrzeit Spalten

  #Zuerst werden Datum und Uhrzeit getrennt
  temp <- tidyr::separate(df_rohdaten, Zeitstempel, into = c("Datum", "Uhrzeit"), sep = "T")

  # Da die Rohdaten verschiedene Formate in den Zeitstempel haben, werden mehrere Schritte ausgeführt, um am Schluss ein einheitliches Datum Uhrzeit Format für alle zu erhalten
  temp$datum1 <- as.Date(suppressWarnings(lubridate::parse_date_time(temp$Datum, c("dmY", "mdY")))) # Mit diesem Code werden die meisten Formate erkennt.
  #Ein Format funktioniert nicht und muss im nächsten Schritt verarbeitet werden.
  temp$datum_corr <- as.Date(ifelse(is.na(temp$datum1), as.character(as.Date(temp$Datum, format = "%d.%m.%y")), as.character(temp$datum1)))

  # Erstelle eine Jahresspalte
  temp$Jahr <- format(temp$datum_corr, "%Y")

  #Füge die korrigierte Datumspalte mit der Uhrzeitspalte zusammen um ein Timestamp zu erhalten.
  temp$Zeitstempel_corr <- paste0(as.character(temp$datum_corr), "T", as.character(temp$Uhrzeit))

  #Wähle die relevanten Spalten aus
  temp <- temp[c("fmin_hz", "fmax_hz", "service_name", "value_v_m", "Messort_Code", "Jahr", "datum_corr", "Zeitstempel_corr")]

  # Formatiere die Datumspalten im Schwellenwerte Dataframe zu Datum
  df_schwellenwerte$gueltig_von <- as.Date(df_schwellenwerte$gueltig_von, "%d.%m.%Y")
  df_schwellenwerte$gueltig_bis <- as.Date(df_schwellenwerte$gueltig_bis, "%d.%m.%Y")

  # In der Spalte gueltig_bis können NA vorkommen. Dort wo NA vorkommen, heisst das, das die Schwellenwerte immer noch gültig sind. Damit der spätere Join funktioniert,
  # werden die NA mit dem aktuellen DAtum überschrieben.
  df_schwellenwerte <- df_schwellenwerte %>%
    mutate(gueltig_bis = if_else(is.na(gueltig_bis), Sys.Date(), gueltig_bis))

  cli::cli_alert_success("Bereinigung der Datum Uhrzeit Spalten wurde erfolgreich durchgeführt")

  # Merge Rohdaten mit Schwellenwerte Daten. Da ein Inner join gemacht wird, bleiben nur die Zeilen übrig, welche in beiden Dataframe vorkommen. Damit werden z.B alte Services wie "Others I",
  # ausgeschlossen.
  by <- join_by(fmin_hz == Freq_min, fmax_hz == Freq_max, between(datum_corr, gueltig_von, gueltig_bis))
  df_merged <- inner_join(temp, df_schwellenwerte, by)

  # Zeige Anzahl nicht erfolgreiche Joins dem User an:
  number_of_anti_joins <- nrow(anti_join(temp, df_schwellenwerte, by))

  cli::cli_alert_info(paste0(number_of_anti_joins, " Messwerten konnte keine Schwellenwerte hinzugefügt werden."))
  cli::cli_alert_success("Schwellenwerte wurden erfolgreich den Messwerten hinzugefügt")


  # Führe Schwellenwertkorrektur durch
  df_merged$Value_V_per_m_corrected <- df_merged$value_v_m - df_merged$Schwellenwert_MR_Vm

  # Ersetze die korrgierten Werte mit 0, wenn sie kleiner als 0 sind
  df_merged$Value_V_per_m_corrected <- ifelse(df_merged$Value_V_per_m_corrected < 0, 0, df_merged$Value_V_per_m_corrected)

  cli::cli_alert_success("Schwellenwertbereinigung erfolgreich durchgeführt")

  ##---------------------------------------------------------------------------
  # 3. Berechne rollierende Mittelwerte
  ##---------------------------------------------------------------------------

  # Erstelle neues Dataframe, wo die einzelnen Frequenzbänder in die Kategorien quadratisch summiert und danach die Wurzel gezogen wird.

  # Berechne rollierender Mittelwert
  df_grouped_rolling <- df_merged %>%
    dplyr::group_by(Jahr, Messort_Code, Kategorie, Zeitstempel_corr) %>%
    # summiert die quadrierten Argumente & gibt die Quadratwurzel einer Zahl zurück
    dplyr::summarise(value_grouped = sqrt(sum(Value_V_per_m_corrected^2)), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Jahr, Messort_Code, Kategorie) %>% # Groupiere und berechne den rolling mean im nächsten Schritt pro Gruppe
    dplyr::mutate(sixmin_avg = zoo::rollapply(value_grouped, rolling_mean_breite ,mean,align=type_rolling_mean,fill=NA))


  # Bilde Mittelwerte
  df_grouped_max <- df_grouped_rolling %>%
    rename('Service' = "Kategorie") %>%
    dplyr::group_by(Jahr, Messort_Code, Service) %>%
    dplyr::summarise(Wert = max(sixmin_avg, na.rm = TRUE), .groups = "drop")


  # Joine Information für finales Dataframe
  df_final <- merge(df_grouped_max, df_messorte[c('Messort_Code', 'Messort_Name', 'Messintervall')], by.x = 'Messort_Code', by.y = 'Messort_Code') %>%
    mutate(Einheit_kurz = Einheit_kurz,
           Einheit_lang = Einheit_lang,
           Messgeraet_Typ = "SRM 3006") %>%
    select(Jahr, Messort_Code, Messort_Name, Service, Wert, Einheit_kurz, Einheit_lang, Messintervall, Messgeraet_Typ)

  cli::cli_alert_success("Berechnung durchgeführt und weitere Informationen wurden hinzugefügt")

  ##---------------------------------------------------------------------------
  # 4. Speichere Daten als CSV und Zip
  ##---------------------------------------------------------------------------

  # Speichere aufbereites messwerte File als CSV
  readr::write_csv(df_final, file = paste0(path_to_load_folder, aufbereite_messwerte_ogd_filename, ".csv"))

  cli::cli_alert_success("aufbereitete_messwerte.csv wurde lokal gespeichert")

  #Erstelle temporäres Verzeichnis und speichere CSV in Verzeichnis. Dieses Verzeichnis wird danach gezippt
  dir.create(path_to_rohdaten_folder)
  readr::write_csv(df_rohdaten, file = paste0(path_to_rohdaten_folder, rohdaten_messwerte_ogd_filename, ".csv"))

  # Speichere Rohdaten File als Zip
  archive::archive_write_dir(archive = paste0(path_to_load_folder, rohdaten_messwerte_ogd_filename, ".zip"), dir = path_to_rohdaten_folder)

  # Lösche temporärer Ordner
  unlink(path_to_rohdaten_folder, recursive = TRUE)

  cli::cli_alert_success("rohdaten_messwerte.zip wurde lokal gespeichert")
  cli::cli_alert_success("Transform Skript ist erfolgreich durchgelaufen")

  ##---------------------------------------------------------------------------

}

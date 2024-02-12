#' Update MDV Messwerte Ressourcen der Immissionsmessungen NIS im Kanton Zürich als OGD
#'
#' Dieses Funktion eignet sich um die OGD Schulhausmessungsdaten (nur Messdaten) zu aktualisieren.
#' Achtung: Zur Aktualiserung des 'Messorte' CSV File -> über das GUI des MDV hochladen. Mit diesem Skript werden nur
#' die Messwerte Files (Rohdaten Messwerte & Aufbereitete Messwerte) aktualisiert.
#' Das OGD Dataset muss bereits in der Metadatenverwaltung angelegt sein und die richtige ID in den Parameter (siehe weiter unten) angegeben werden.
#' Es braucht Schreibrechte im MDV, welche mit den dazugehoerigen Username Passwort Kombination moeglich ist (siehe naechste Linie). Ausserdem braucht man einen Token, um das neuste zhMetadatenAPI Package von Github herunterzuladen.
#' Bitte beachte, dass im .Renviron File die Variabeln: 'mdv_user', 'mdv_pw' und 'ZH_METADATEN_API_TOKEN' mit dazugehoerigen Werten vorhanden ist.
#'
#'@export
#'
update <- function(){

  ##---------------------------------------------------------------------------
  # 0. Parameter setzen und überprüfen
  ##---------------------------------------------------------------------------
  cli::cli_alert_info("Starte Update Prozess")

  dataset_id = 6436
  testmode = FALSE # boolean, testmode = FALSE bedeutet, dass die Files auf das Produktive MDV geladen werden
  titel_rohdaten_mdv = "Rohdaten Messwerte" # Titel der Rohdaten Ressourcen im MDV als String.
  titel_aufbereitete_daten_mdv = "Aufbereitete Messwerte" # Titel der Aufbereitete Daten Ressourcen im MDV als String.
  mdv_user = Sys.getenv("mdv_user") # Username als String (in der Regel die Emailadresse des Users vom MDV)
  mdv_pw = Sys.getenv("mdv_pw") # Password als String für den Zugang des MDV
  path_rohdaten_messwerte = "inst/extdata/temp/load/rohdaten_messwerte.zip" # Datenpfad (lokal) als String zum Rohdaten Zip File
  path_aufbereitetem_file = "inst/extdata/temp/load/aufbereitete_messwerte.csv" # Datenpfad (lokal) als String zum Aufbereiteten CSV File

  # Überprüfe, ob die Argumente der Funktion sind im richtigen Datenformat. Ansonsten breche ab und werfe Fehlermeldung
  assertthat::assert_that(msg = "dataset_id` must be numeric" , is.numeric(dataset_id))
  assertthat::assert_that(msg = "testmode` must be a boolean" , is.logical(testmode))
  assertthat::assert_that(msg = "titel_rohdaten_mdv` must be a character." , is.character(titel_rohdaten_mdv))
  assertthat::assert_that(msg = "titel_aufbereitete_daten_mdv` must be a character." , is.character(titel_aufbereitete_daten_mdv))
  assertthat::assert_that(msg = "MDV Username fehlt im .Renviron File. Füge den Username der Variable mdv_user hinzu." , mdv_user != "")
  assertthat::assert_that(msg = "MDV Passwort fehlt im .Renviron File. Füge das Passwort der Variable mdv_pw hinzu." , mdv_pw != "")
  assertthat::assert_that(msg = "mdv_user` must be a character." , is.character(mdv_user))
  assertthat::assert_that(msg = "mdv_pw` must be a character." , is.character(mdv_pw))
  assertthat::assert_that(msg = "path_rohdaten_messwerte` must be a character." , is.character(path_rohdaten_messwerte))
  assertthat::assert_that(msg = "path_aufbereitem_file` must be a character." , is.character(path_aufbereitetem_file))

  # Überprüfe ob die lokalen Files unter den gewaehlten Pfaeden exisitieren
  assertthat::assert_that(msg = "Unter dem gewaehlten Pfad befindet sich kein Rohdaten File oder der Zugriff auf das File ist nicht moeglich" , file.exists(path_rohdaten_messwerte))
  assertthat::assert_that(msg = "Unter dem gewaehlten Pfad befindet sich kein Aufbereitetes Data File oder der Zugriff auf das File ist nicht moeglich" , file.exists(path_aufbereitetem_file))

  ##---------------------------------------------------------------------------
  # 1. Beziehe Ressourcen IDs für die Messwerte Ressourcen vom MDV
  ##---------------------------------------------------------------------------

  # Hole alle Ressourcen Information vom MDV für die gewaehlte Dataset_ID
  metadaten_ressourcen_mdv  <-  zhMetadatenAPI::get_distributions(dataset_id, mdv_user, mdv_pw, testmode = testmode, verbose = FALSE) %>%
  # Status der Ressourcen muss groesser als 0 sein
  dplyr::filter(STATUS >0)

  # Ressource ID der Rohdaten Ressource
  ressourcen_id_rohdaten <- metadaten_ressourcen_mdv[metadaten_ressourcen_mdv$LABEL == titel_rohdaten_mdv, "ID"][[1]]
  assertthat::assert_that(msg = "Es konnte keine Rohdaten ID vom MDV mit diesem Titel unter der gewaehlten Dataset_ID gefunden werden" , is.numeric(ressourcen_id_rohdaten))

  # Ressource ID der Aufbereitete Ressource
  ressourcen_id_aufbereitete_daten <- metadaten_ressourcen_mdv[metadaten_ressourcen_mdv$LABEL == titel_aufbereitete_daten_mdv, "ID"][[1]]
  assertthat::assert_that(msg = "Es konnte keine Aufbereitete Daten ID vom MDV mit diesem Titel unter der gewaehlten Dataset_ID gefunden werden" , is.numeric(ressourcen_id_aufbereitete_daten))

  ##---------------------------------------------------------------------------
  # 2. Upload der Ressourcen in den MDV
  ##---------------------------------------------------------------------------

  # Erstelle naechstes erwartetes Datum, wo Daten aktualisiert werden. In der Regel ist dies im naechsten Jahr
  next_expected_update_date <- as.character(paste(as.numeric(format(Sys.Date(), "%Y")) + 1, format(Sys.Date(), "%m-%d"), sep = "-"))

  ##---------------------------------------------------------------------------
  # 2.1 Update Rohdaten Messwerte
  ##---------------------------------------------------------------------------

  # Lade File in den MDV hoch
  try(zhMetadatenAPI::update_distribution(user=mdv_user,
                                          pw=mdv_pw,
                                          distribution_id = ressourcen_id_rohdaten,
                                          file_path = path_rohdaten_messwerte,
                                          modified_next= next_expected_update_date, # Datum des naechsten geplanten Update des Datasets
                                          ogd_flag = "true", # auf opendata.swiss publizieren
                                          zhweb_flag = "true", # auf zh.ch/opendata
                                          stat_server_flag = "true",
                                          status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                          right_id = 2,
                                          testmode = testmode))

  ##---------------------------------------------------------------------------
  # 2.2 Update Aufbereitetes Messwerte File
  ##---------------------------------------------------------------------------

  # Erstelle die Datum Metadaten aus dem Aufbereitetem Data File und dem aktuellen Datum
  min_date <- as.character(paste0(min(readr::read_csv(path_aufbereitetem_file, show_col_types = FALSE)$Jahr), "-01-01"))
  max_date <- as.character(paste0(max(readr::read_csv(path_aufbereitetem_file, show_col_types = FALSE)$Jahr), "-12-31"))


  # Lade File in den MDV hoch
  try(zhMetadatenAPI::update_distribution(user=mdv_user,
                                          pw=mdv_pw,
                                          distribution_id = ressourcen_id_aufbereitete_daten,
                                          file_path = path_aufbereitetem_file,
                                          modified_next= next_expected_update_date, # Datum des naechsten geplanten Update des Datasets
                                          start_date= min_date, # Start der Zeitspanne welche das Dataset abdeckt
                                          end_date= max_date, # Ende der Zeitspanne welche das Dataset abdeckt
                                          ogd_flag = "true", # auf opendata.swiss publizieren
                                          zhweb_flag = "true", # auf zh.ch/opendata
                                          stat_server_flag = "true",
                                          status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                          right_id = 2,
                                          testmode = testmode))

  cli::cli_alert_success("Update Skript ist erfolgreich durchgelaufen")

}
##---------------------------------------------------------------------------


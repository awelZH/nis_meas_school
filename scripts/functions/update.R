## ----------------------------------------------------------------------------------------------------------------
#' Update MDV Messwerte Ressourcen der Immissionsmessungen NIS im Kanton Zürich als OGD
#' Dieses Skript eignet sich um die OGD Schulhausmessungsdaten (nur Messdaten) zu aktualisieren.
#' Achtung: Zur Aktualiserung des 'Messorte' CSV File -> über das GUI des MDV hochladen. Mit diesem Skript werden nur
#' die Messwerte Files (Rohdaten Messwerte & Aufbereitete Messwerte) aktualisiert.
#' Das OGD Dataset muss bereits in der Metadatenverwaltung angelegt sein und die richtige ID in den Parameter (siehe weiter unten) angegeben werden.
#' Es braucht Schreibrechte im MDV, welche mit den dazugehörigen Username Passwort Kombination möglich ist (siehe nächste Linie). Ausserdem braucht man einen Token, um das neuste zhMetadatenAPI Package von Github herunterzuladen.
#' Bitte beachte, dass in deinem .Renviron File die Variabeln: 'mdv_user', 'mdv_pw' und 'ZH_METADATEN_API_TOKEN' mit dazugehörigen Werten vorhanden ist.
#' @param dataset_id numeric, Dataset ID aus der Metadatenverwaltung
#' @param testmode boolean, testmode = FALSE bedeutet, dass die Files auf das Produktive MDV geladen werden
#' @param titel_rohdaten_mdv Titel der Rohdaten Ressourcen im MDV als String.
#' @param titel_aufbereitete_daten_mdv Titel der Aufbereitete Daten Ressourcen im MDV als String.
#' @param mdv_user Username als String (in der Regel die Emailadresse des Users vom MDV)
#' @param mdv_pw Password als String für den Zugang des MDV
#' @param path_rohdaten_messwerte Datenpfad als String zum Rohdaten File. Dieses File wird in dieser Funktion im MDV hochgeladen
#' @param path_aufbereitetem_file Datenpfad als String zum Aufbereiteten File. Dieses File wird in dieser Funktion im MDV hochgeladen
#'
#'
#' @examples update(dataset_id = 6436, testmode = FALSE, titel_rohdaten_mdv = "Rohdaten Messwerte", titel_aufbereitete_daten_mdv = "Aufbereitete Messwerte", mdv_user = Sys.getenv("mdv_user"), mdv_pw = Sys.getenv("mdv_pw"), path_rohdaten_messwerte = "data/temp/rohdaten_messwerte.zip", path_aufbereitetem_file = 'data/temp/aufbereitete_messwerte.csv')
#'
update <- function(dataset_id, testmode, titel_rohdaten_mdv, titel_aufbereitete_daten_mdv, mdv_user, mdv_pw, path_rohdaten_messwerte, path_aufbereitem_file){

  # Überprüfe, ob die Argumente der Funktion sind im richtigen Datenformat. Ansonsten breche ab und werfe Fehlermeldung
  stopifnot("`dataset_id` must be numeric" = is.numeric(dataset_id))
  stopifnot("`testmode` must be a boolean" = is.logical(testmode))
  stopifnot("`titel_rohdaten_mdv` must be a character." = is.character(titel_rohdaten_mdv))
  stopifnot("`titel_aufbereitete_daten_mdv` must be a character." = is.character(titel_aufbereitete_daten_mdv))
  stopifnot("`mdv_user` must be a character." = is.character(mdv_user))
  stopifnot("`mdv_pw` must be a character." = is.character(mdv_pw))
  stopifnot("`path_rohdaten_messwerte` must be a character." = is.character(path_rohdaten_messwerte))
  stopifnot("`path_aufbereitem_file` must be a character." = is.character(path_aufbereitetem_file))

  # Überprüfe ob die lokalen Files unter den gewählten Pfäden exisitieren
  stopifnot("Unter dem gewählten Pfad befindet sich kein Rohdaten File oder der Zugriff auf das File ist nicht möglich" = file.exists(path_rohdaten_messwerte))
  stopifnot("Unter dem gewählten Pfad befindet sich kein Aufbereitetes Data File oder der Zugriff auf das File ist nicht möglich" = file.exists(path_aufbereitetem_file))

##---------------------------------------------------------------------------

# 1. Beziehe Ressourcen IDs für die Messwerte Ressourcen vom MDV

# Hole alle Ressourcen Information vom MDV für die gewählte Dataset_ID
metadaten_ressourcen_mdv  <-  zhMetadatenAPI::get_distributions(dataset_id, mdv_user, mdv_pw, testmode = testmode, verbose = FALSE) %>%
  # Status der Ressourcen muss grösser als 0 sein
  dplyr::filter(STATUS >0)

# Ressource ID der Rohdaten Ressource
ressourcen_id_rohdaten <- metadaten_ressourcen_mdv[metadaten_ressourcen_mdv$LABEL == titel_rohdaten_mdv, "ID"][[1]]
stopifnot("Es konnte keine Rohdaten ID vom MDV mit diesem Titel unter der gewählten Dataset_ID gefunden werden" = is.numeric(ressourcen_id_rohdaten))

# Ressource ID der Aufbereitete Ressource
ressourcen_id_aufbereitete_daten <- metadaten_ressourcen_mdv[metadaten_ressourcen_mdv$LABEL == titel_aufbereitete_daten_mdv, "ID"][[1]]
stopifnot("Es konnte keine Aufbereitete Daten ID vom MDV mit diesem Titel unter der gewählten Dataset_ID gefunden werden" = is.numeric(ressourcen_id_aufbereitete_daten))

## ----------------------------------------------------------------------------------------------------------------

# 2. Upload

  # Erstelle nächstes erwartetes Datum, wo Daten aktualisiert werden. In der Regel ist dies im nächsten Jahr
  next_expected_update_date <- as.character(paste(as.numeric(format(Sys.Date(), "%Y")) + 1, format(Sys.Date(), "%m-%d"), sep = "-"))

  # Update Rohdaten Messwerte

  # Lade File in den MDV hoch
  try(zhMetadatenAPI::update_distribution(user=mdv_user,
                                          pw=mdv_pw,
                                          distribution_id = ressourcen_id_rohdaten,
                                          file_path = path_rohdaten_messwerte,
                                          modified_next= next_expected_update_date, # Datum des nächsten geplanten Update des Datasets
                                          ogd_flag = "true", # auf opendata.swiss publizieren
                                          zhweb_flag = "true", # auf zh.ch/opendata
                                          stat_server_flag = "true",
                                          status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                          right_id = 2,
                                          testmode = testmode))

  # Update Aufbereitete Messwerte

  # Erstelle die Datum Metadaten aus dem Aufbereitetem Data File und dem aktuellen Datum
  min_date <- as.character(paste0(min(readr::read_csv(path_aufbereitetem_file)$Jahr), "-01-01"))
  max_date <- as.character(paste0(max(readr::read_csv(path_aufbereitetem_file)$Jahr), "-12-31"))


  # Lade File in den MDV hoch
  try(zhMetadatenAPI::update_distribution(user=mdv_user,
                                          pw=mdv_pw,
                                          distribution_id = ressourcen_id_aufbereitete_daten,
                                          file_path = path_aufbereitetem_file,
                                          modified_next= next_expected_update_date, # Datum des nächsten geplanten Update des Datasets
                                          start_date= min_date, # Start der Zeitspanne welche das Dataset abdeckt
                                          end_date= max_date, # Ende der Zeitspanne welche das Dataset abdeckt
                                          ogd_flag = "true", # auf opendata.swiss publizieren
                                          zhweb_flag = "true", # auf zh.ch/opendata
                                          stat_server_flag = "true",
                                          status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                          right_id = 2,
                                          testmode = testmode))

}
## ----------------------------------------------------------------------------------------------------------------


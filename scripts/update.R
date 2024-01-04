# Kurbeschreibung:
## Dieses Skript eignet sich um die OGD Schulhausmessungsdaten (nur Messdaten) zu aktualisieren.
## Achtung: Zur Aktualiserung des 'Messorte' CSV File -> über das GUI des MDV hochladen. Mit diesem Skript werden nur
## die Messwerte Files (Rohdaten Messwerte & Aufbereitete Messwerte) aktualisiert.

# Voraussetzungen:
## Das OGD Dataset muss bereits in der Metadatenverwaltung angelegt sein und die richtige ID in den Parameter (siehe weiter unten) angegeben werden.
## Es braucht Schreibrechte im MDV, welche mit den dazugehörigen Username Passwort Kombination möglich ist (siehe nächste Linie). Ausserdem braucht man einen Token, um das neuste zhMetadatenAPI Package von Github herunterzuladen.
## Bitte beachte, dass in deinem .Renviron File die Variabeln: 'mdv_user', 'mdv_pw' und 'ZH_METADATEN_API_TOKEN' mit dazugehörigen Werten vorhanden ist.

# Schritte
# 0. Verändere ggfs. Parameter
# 1. Packages und Daten laden
# 2. Erstelle pro Ressource, welche aktualisiert werden soll, die Metainformation (Lokaler Pfad, ID etc.)
# 3. Datenaufbereitung & Upload: Die "alten" Daten vom MDV werden heruntergeladen und mit den neuen Daten zu einem Dataframe zusammengefügt
# und dannach hochgeladen


## ----------------------------------------------------------------------------------------------------------------

# 0. Parameter:
## (nur ändern, wenn du sicher bist)
dataset_id                <- 6436  # Dataset ID aus der Metadatenverwaltung
testmode                  <- FALSE   # testmode = FALSE bedeutet, dass die Files auf das Produktive MDV geladen werden

# Login Information von der Metadatenverwaltung. Dieser User muss Schreibrechte auf den dazugehörigen Datensatz im MDV haben.
mdv_user                  <- Sys.getenv("mdv_user")
mdv_pw                    <- Sys.getenv("mdv_pw")

## Datenpfäde den lokalen Files, welche die Messdaten haben, wo noch nicht im MDV hochgeladen wurden:
path_rohdaten_messwerte_new   <- "~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/data/extract/rohdaten_messwerte.zip"
path_to_aufbereitem_file_new  <- '~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/data/transform/aufbereitete_messwerte.csv'

## Datenpfäde der lokalen Files, welche die neuen und bestehenden Messdaten aufweisen. Diese Files werden in diesem Skript neu erstellt und
## auf den MDV hochgeladen
path_rohdaten_messwerte   <- "~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/data/load/rohdaten_messwerte.zip"
path_to_aufbereitem_file  <- '~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/data/load/aufbereitete_messwerte.csv'

# Titel der beiden Messwerte Ressourcen im MDV
list_mdv_ressourcen_namen <- c("Rohdaten Messwerte", "Aufbereitete Messwerte")
##---------------------------------------------------------------------------

# 1. Packages installieren und laden

# Install newest package from github
githubmdvapikey           <- Sys.getenv("ZH_METADATEN_API_TOKEN")
remotes::install_github("statistikZH/zhMetadatenAPI", ref="master", auth_token=githubmdvapikey)

# Load packages
library(dplyr)

##---------------------------------------------------------------------------

# 2. Defniere pro Ressource, welche aktualisiert werden soll die Metainformation (Lokaler Pfad, ID etc.)

## Erstelle temporäres Dataframe mit den zu updatenden Ressourcen.
ressourcen_name_local_paths <- data.frame (name_of_file  = list_mdv_ressourcen_namen,
                                           path = c(path_rohdaten_messwerte_new, path_to_aufbereitem_file_new))


# Erstelle Dataframe, wo zu allen zu aktualisierenden Ressourcen die benötigten Information zusammengefügt werden.
# Zuerst werden die Ressourcen IDs vom MDV geholt.
metadaten_ressourcen_mdv    <-  zhMetadatenAPI::get_distributions(dataset_id, mdv_user, mdv_pw, testmode = testmode, verbose = FALSE) %>%
  dplyr::filter(STATUS >0) %>%
  dplyr::filter(LABEL %in% list_mdv_ressourcen_namen)

# Danach werden die Ressourcen ID mit den lokalen Dateipfäden zusammengefügt.
ressourcen_informationen <- merge(metadaten_ressourcen_mdv, ressourcen_name_local_paths, by.x = "LABEL", by.y = "name_of_file")

## ----------------------------------------------------------------------------------------------------------------

# 3. Datenaufbereitung & Upload: Die "alten" Daten vom MDV werden heruntergeladen und mit den neuen Daten zu einem Dataframe zusammengefügt
# und dannach hochgeladen

if(length(c(ressourcen_informationen$LABEL)) > 0){

  # Füge neue Daten zu den bestehenden Daten und lade die Daten im MDV hoch

  # Rohdaten Messwerte
  # Titel der Rohdaten Ressource
  titel  <- list_mdv_ressourcen_namen[1]
  # Lade lokale neue Daten

  temp_local  <- readr::read_csv(ressourcen_informationen[ressourcen_informationen$LABEL == titel,"path"])
  # Lade bestehende Daten vom MDV
  temp_mdv    <- readr::read_csv(ressourcen_informationen[ressourcen_informationen$LABEL == titel,"DOWNLOAD_URL"])

  # Füge beide Datenframes zusammen stelle sicher, dass es keinen Duplikate gibt. Danach speichere das File als Zip-File
  rbind(temp_local, temp_mdv) %>%
    distinct(.) %>%
    dplyr::arrange(Messort_Code) %>%
    readr::write_csv(., file.path(path_rohdaten_messwerte))

  # Lade File in den MDV hoch
  try(zhMetadatenAPI::update_distribution(user=mdv_user,
                                          pw=mdv_pw,
                                          distribution_id = ressourcen_informationen[ressourcen_informationen$LABEL == titel,"ID"],
                                          file_path = path_rohdaten_messwerte,
                                          modified_next= as.character(Sys.Date()+1), # Datum des nächsten geplanten Update des Datasets
                                          ogd_flag = "true", # auf opendata.swiss publizieren
                                          zhweb_flag = "true", # auf zh.ch/opendata
                                          stat_server_flag = "true",
                                          status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                          right_id = 2,
                                          testmode = testmode))

  # Aufbereitete Messwerte
  # Titel der Aufbereitete Ressource
  titel  <- list_mdv_ressourcen_namen[2]
  # Lade lokale neue Daten

  temp_local  <- readr::read_csv(ressourcen_informationen[ressourcen_informationen$LABEL == titel,"path"])
  # Lade bestehende Daten vom MDV
  temp_mdv    <- readr::read_csv(ressourcen_informationen[ressourcen_informationen$LABEL == titel,"DOWNLOAD_URL"])

  # Füge beide Datenframes zusammen stelle sicher, dass es keien Duplikate gibt
  temp_final <- rbind(temp_local, temp_mdv) %>%
    distinct(.) %>%
    dplyr::arrange(Messort_Code)

  min_year <- min(temp_final$Jahr)

  # Speichere dataframe als CSV, damit es in den MDV hochgeladen werden kann
  readr::write_csv(temp_final, file.path(path_to_aufbereitem_file))

  # Lade File in den MDV hoch
  try(zhMetadatenAPI::update_distribution(user=mdv_user,
                                          pw=mdv_pw,
                                          distribution_id = ressourcen_informationen[ressourcen_informationen$LABEL == titel,"ID"],
                                          file_path = path_to_aufbereitem_file,
                                          modified_next= as.character(Sys.Date()+1), # Datum des nächsten geplanten Update des Datasets
                                          start_date= as.character(paste0(min_year, "-01-01")), # Start der Zeitspanne welche das Dataset abdeckt
                                          end_date= as.character(paste0(format(Sys.Date(), "%Y"), "-12-31")), # Ende der Zeitspanne welche das Dataset abdeckt
                                          ogd_flag = "true", # auf opendata.swiss publizieren
                                          zhweb_flag = "true", # auf zh.ch/opendata
                                          stat_server_flag = "true",
                                          status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                          right_id = 2,
                                          testmode = testmode))

}else {
  print("No files in list_files_to_update. If you want to update files, add filename to this list")
}

## ----------------------------------------------------------------------------------------------------------------


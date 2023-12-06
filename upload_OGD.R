# Kurbeschreibung:
## Dieses Skript eignet sich um die OGD Schulhausmessungsdaten zu aktualisieren.
## Wenn du nicht alle OGD Files gleichzeitig updaten möchtest, verändere die Liste "list_files_to_update" unter dem Bereich Parameter.

# Voraussetzungen:
## Der OGD Dataset muss bereits in der Metadatenverwaltung angelegt sein und die richtige ID in den Parameter (siehe weiter unten) angegeben werden.
## Es braucht Schreibrechte im MDV, welche mit den dazugehörigen Username Passwort Kombination möglich ist (siehe nächste Linie). Ausserdem braucht man einen Token, um das neuste zhMetadatenAPI Package von Github herunterzuladen.
## Bitte beachte, dass in deinem .Renviron File die Variabeln: 'mdv_user', 'mdv_pw' und 'ZH_METADATEN_API_TOKEN' mit dazugehörigen Werten vorhanden ist.
## ----------------------------------------------------------------------------------------------------------------

# Parameter:
## (nur ändern, wenn du sicher bist)
dataset_id = 6436
# Alle die Filenamen, welche in dieser Liste sind, werden als OGD upgedated (sofern sie bereits bestehend sind)
list_files_to_update = c("Aubereitete_Messwerte", "Rohdaten_Messwerte", "Messorte")

# Login Information von der Metadatenverwaltung
mdv_user = Sys.getenv("mdv_user")
mdv_pw = Sys.getenv("mdv_pw")

## Path to local files:
path_aubereitete_messwerte = "data/aufbereitete_messwerte.csv"
path_rohdaten_messwerte = "data/rohdaten_messwerte.csv"
path_messorte = "data/messorte.csv"
## Create temp dataframe from
df_paths <- data.frame (name_of_file  = c("Aubereitete_Messwerte", "Rohdaten_Messwerte", "Messorte"),
                  path = c(path_aubereitete_messwerte, path_rohdaten_messwerte, path_messorte))
##---------------------------------------------------------------------------


## Install newest package from github
githubmdvapikey <- Sys.getenv("ZH_METADATEN_API_TOKEN")
remotes::install_github("statistikZH/zhMetadatenAPI", ref="master", auth_token=githubmdvapikey)
## ----------------------------------------------------------------------------------------------------------------

# Code:
## Load packages
library(dplyr)

# Create dataframe with information about distributions that are available on the MDV
temp <-  zhMetadatenAPI::get_distributions(dataset_id, mdv_user, mdv_pw, testmode = FALSE, verbose = FALSE) %>%
  dplyr::filter(STATUS >0) %>%
  dplyr::select(ID, DATASET_ID, LABEL) %>%
  # Filter only for the files, that should be updated
  dplyr::filter(LABEL %in% list_files_to_update)


# Join local paths to dataframe
merged_df <- merge(temp, df_paths, by.x = "LABEL", by.y = "name_of_file")
## ----------------------------------------------------------------------------------------------------------------

# Update distributions
if(length(list_files_to_update) > 0){

  for(filename in list_files_to_update){
    zhMetadatenAPI::update_distribution(user=mdv_user,
                                        pw=mdv_pw,
                                        distribution_id = merged_df[merged_df$LABEL == filename,"ID"],
                                        file_path = merged_df[merged_df$LABEL == filename,"path"],
                                        modified_next= as.character(Sys.Date()+1), # Datum des nächsten geplanten Update des Datasets
                                        start_date= as.character("2011-01-01"), # Start der Zeitspanne welche das Dataset abdeckt
                                        end_date= as.character(paste0(format(Sys.Date(), "%Y"), "-12-31")), # Ende der Zeitspanne welche das Dataset abdeckt
                                        ogd_flag = "true", # auf opendata.swiss publizieren
                                        zhweb_flag = "true", # auf zh.ch/opendata
                                        stat_server_flag = "true",
                                        status = 1, # ACHTUNG: Um direkt zu publizieren, muss status=3 gesetzt werden
                                        right_id = 2,
                                        testmode = FALSE)

  }
} else {
    print("No files in list_files_to_update. If you want to update files, add filename to this list")
  }

## ----------------------------------------------------------------------------------------------------------------
print("All set")

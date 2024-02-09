#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(cli, purrr, stringr, vroom, janitor, readr, vctrs, dplyr, fs, data.table,
               tibble, assertthat, zoo,archive,httr2,lubridate,stringr,tidyr,
               install = TRUE,
               update = FALSE)

# Installiere zhMetadatenAP Package von Github
remotes::install_github("statistikZH/zhMetadatenAPI", auth_token=Sys.getenv("ZH_METADATEN_API_TOKEN"))

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())

devtools::load_all(".")

#### Führe Berechnung und OGD Upload durch ==========================================================

# Downloade aktuellste Version des Codes von Github
update_project_from_github()

#Definiere Pfad, wo Rohdaten lokal liegen
path_rohdaten_topfolder <- "/home/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/"

# Entscheide ob ein Full oder Delta Load gemacht werden sollte
full_load <- FALSE
delta_load <- TRUE

# Lade alle benötigten Daten
extract(full_load = full_load, delta_load = delta_load, path_rohdaten_topfolder = path_rohdaten_topfolder)

# Führe Berechnung durch
transform(full_load = full_load)

# Lade OGD Daten hoch
update()

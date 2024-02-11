#### Definiere Variablen ==========================================================

#Definiere Pfad, wo Rohdaten lokal liegen
path_rohdaten_topfolder <- "/home/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/"

# Definiere Working directory
path_wd <- "~/gitrepos/nis_meas_school"
setwd(path_wd)

# Entscheide ob ein Full oder Delta Load gemacht werden sollte
full_load <- T
delta_load <- F

# Downloade aktuellste Version des Codes von Github
update_project_from_github()

devtools::load_all()
#source_files("R")
#### Führe Berechnung und OGD Upload durch ==========================================================

# Lade alle benötigten Daten
extract(full_load = full_load, delta_load = delta_load, path_rohdaten_topfolder = path_rohdaten_topfolder)

# Führe Berechnung durch
transform(full_load = full_load)

# Lade OGD Daten hoch
update()

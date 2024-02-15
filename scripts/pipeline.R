#### Definiere Variablen ==========================================================

#Definiere Pfad, wo Rohdaten lokal liegen
path_rohdaten_topfolder <- paste0(get_location(),"08_DS/01_Projekte/AWEL/2023_Schulhausmessung/")

# Entscheide ob ein Full oder Delta Load gemacht werden sollte
full_load <- TRUE
delta_load <- FALSE

devtools::load_all()
#### Führe Berechnung und OGD Upload durch ==========================================================

# Lade alle benötigten Daten
nisMeasSchool::extract(full_load = full_load, delta_load = delta_load, path_rohdaten_topfolder = path_rohdaten_topfolder)

# Führe Berechnung durch
nisMeasSchool::transform(full_load = full_load)

# Lade OGD Daten hoch
nisMeasSchool::update()

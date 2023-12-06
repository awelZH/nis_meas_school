# nis_meas_school
Repository for NIS measurements within the Canton of Zurich

# How to use
## main.R
If you want to update the OGD data with the newest data (difference load) please use this script. Note: This script only updates the difference between the available OGD dataset in the MDV and the folder you are providing, when running this script.

In the folder [scripts](scripts), you will find additional scripts for different purposes:

## load_all_data.R
Load all data available (full load) and generate a refined dataframe, where all measurement data is available. This dataset is saved to [data/aufbereitete_messwerte.csv](aufbereitete_messwerte.csv)
Note: The caluclations are not done in this script and if you want to to the yearly update, use the script [main.R](main.R).

## calculate.R
This scripts generates the dataset ([data/rohdaten_messwerte.csv](data/rohdaten_messwerte.csv)), which is later uploaded as OGD and used for visualisation. Note: The [data/aufbereitete_messwerte.csv](aufbereitete_messwerte.csv) needs to be available.

## upload_OGD.R
If you want to upload all or a selection of the OGD Files to the Metadatenverwaltung, use this script.

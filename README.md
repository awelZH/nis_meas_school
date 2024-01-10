# nis_meas_school
Repository to update the zhWeb visualization and OGD of the NIS measurements within the Canton of Zurich 

# How to use
## pipeline.R
Main script to calculate and update the OGD data.

### extract_data.R
Contains the function, that calculates the Rohdaten file, that is later needed in the transform and update functions.

## transform.R
Contains the function, that calculates the Messwerte file, that is later needed in the update function.

## update.R
Contains the function to update both Messwerte OGD Files.

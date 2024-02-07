# pipeline

# source the packages and functions
source_files(path = "scripts/functions")

# pull the newest changes of git-repo
update_project_from_github()

#Set path to home directory (Path to local nis_meas_shool github repo)
setwd("")

#Set path to local files
path_rohdaten_topfolder <- "~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/"


full_load <- FALSE
delta_load <- TRUE
# run the pipeline

# Call the extract function
extract(full_load = full_load, delta_load = delta_load, path_rohdaten_topfolder = path_rohdaten_topfolder)

# Call the transform function
transform(full_load = full_load)

# Call the update function
update()

# pipeline

# source the packages and functions
source_files(path = "scripts/functions")

# pull the newest changes of git-repo
update_project_from_github()

#Set path to local files
path_rohdaten_topfolder = "~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/"


# run the pipeline

# Call the extract function
extract(full_load = TRUE, delta_load = FALSE, path_rohdaten_topfolder = path_rohdaten_topfolder)

# Call the transform function
transform()

# Call the update function
update()

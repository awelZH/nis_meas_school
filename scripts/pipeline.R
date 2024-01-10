# pipeline

# source the packages and functions
source_files(path = "scripts/functions")

# pull the newest changes of git-repo
update_project_from_github()

# run the pipeline
extract(delta_load = TRUE, path_rohdaten_topfolder = "~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/")



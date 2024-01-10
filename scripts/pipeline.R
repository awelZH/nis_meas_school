# pipeline

# source the packages and functions
source_files(path = "scripts/functions")

# pull the newest changes of git-repo
update_project_from_github()

# run the pipeline
extract(delta_load = TRUE, path_rohdaten_topfolder = here::here())



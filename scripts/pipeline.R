#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(cli, purrr, stringr, vroom, janitor, readr, vctrs, dplyr, fs, data.table,
               tibble, assertthat, zoo,archive,httr2,lubridate,stringr,tidyr,
               install = TRUE,
               update = FALSE)

remotes::install_github("statistikZH/zhMetadatenAPI",
                        auth_token=Sys.getenv("ZH_METADATEN_API_TOKEN"))


# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())

devtools::load_all(".")

# pipeline

# source the packages and functions
#source_files(path = "scripts/functions")

# pull the newest changes of git-repo
update_project_from_github()

#Set path to local files
path_rohdaten_topfolder <- "/home/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/"

full_load <- FALSE
delta_load <- TRUE
# run the pipeline

# Call the extract function
extract(full_load = full_load, delta_load = delta_load, path_rohdaten_topfolder = path_rohdaten_topfolder)

# Call the transform function
transform(full_load = full_load)

# Call the update function
update()

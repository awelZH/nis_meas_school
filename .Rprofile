# source_files <- function(path, pattern = "\\.R$") {
#   # List all files in the specified path
#   all_files <- list.files(path, pattern = pattern, full.names = TRUE)
#
#   # Source each file
#   sapply(all_files, source)
# }

# Installiere zhMetadatenAP Package von Github
remotes::install_github("statistikZH/zhMetadatenAPI", auth_token=Sys.getenv("ZH_METADATEN_API_TOKEN"))

cat("Bitte überprüfe, ob die Pfäde ('path_rohdaten_topfolder' und 'path_wd') und Variabeln ('full_load' und 'delta_load') im scripts/pipeline.R Skript richtig gesetzt sind. Passe sie vor dem Ausführen des Skripts ggfs. noch an." )

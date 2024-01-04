# function to get existing data from ogd ressource
ogd_rohdaten_df <- read_csv(archive_read("Rohdaten_Export/KTZH_00002462_00005003.zip"), col_types = cols()) %>%
  dplyr::select(-...1)





ogd_rohdaten_df <- ogd_rohdaten_df %>%
  dplyr::select(Zeitstempel, Messort_Code) %>%
  mutate(Year = str_extract(Zeitstempel, "(\\d{4}|\\d{2})(?=T)")) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Year = if_else(Year < 100, Year + 2000, Year)
  )


ogd_rohdaten_df %>%
  distinct(Messort_Code, Year)


extract_data(delta_load = TRUE, full_load = FALSE, time_load = c("1-1-2020", "31-12-2023")){
  if(specific_load != FALSE && check_date(specific_load))
}

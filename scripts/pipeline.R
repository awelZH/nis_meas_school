# pipeline

# Get the paths of CSV files




csv_paths_list_5 <- extract_csv_paths_einzel(top_folder_path, num_dirs = 5)

csv_paths_list_all <- extract_csv_paths_einzel(top_folder_path = "Schulhausmessungen", num_dirs = Inf)

csv_paths_list_all_langzeit <- extract_csv_paths_langzeit(top_folder_path = "Langzeit-Messungen", num_dirs = Inf)


filtered_einzel <- filter_csv_files_einzel(paths_list = csv_paths_list_all)

filtered_langzeit <- filter_csv_files_langzeit(paths_list = csv_paths_list_all_langzeit)

# delta checks follow here

existing_ogd <- retrieve_existing_data()

# Removing specific years from the vector
years_to_remove <- c("2022", "2018")
existing_ogd$Langzeitmessungen[[17]] <- existing_ogd$Langzeitmessungen[[17]][!existing_ogd$Langzeitmessungen[[17]] %in% years_to_remove]


delta_einzel <- clean_einzel_list(filtered_einzel, id_list = existing_ogd$Einzelmessungen[6:23])
delta_langzeit <- clean_langzeit_list(filtered_langzeit, df_nested_list = existing_ogd$Langzeitmessungen)

# report the delta (invisibly returns a data frame for delta checks / not used in processing)

delta_report_einzel <- report_delta_einzel(existing_ogd, delta_einzel)
delta_report_langzeit <- report_delta_langzeit(existing_ogd, delta_langzeit)

# this should be used in the delta-load function as a assertion and skip condition
check_delta_vs_existing(existing_ogd$Messorte, delta_report_langzeit, delta_report_einzel)

# only after this step, the actual delta load with delta_einzel & delta_langzeit should be performed
# the full load does not need all this and will just handle all the data

# Example usage

measurements_data <- process_csv_data_einzel(csv_paths_list_5)

measurements_data_all_langzeit <- process_csv_data_langzeit(filtered_langzeit)



# save results

saveRDS(object = measurements_data_all, file = "Einzelmessungen_raw_list.RDS")
saveRDS(object = measurements_data_all_langzeit, file = "Langzeitmessungen_raw_list.RDS")


# check if the dataframe has NA values in the value_v_m_2 variable

check_na_einzelmessungen(Einzelmessungen_raw_list)
check_na_langzeitmessungen(Langzeitmessungen_raw_list)

# if no NA bind together the data frames
einzelmessungen_processed <- Einzelmessungen_raw_list %>%
  map(~ .x %>%
        select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)) %>%
  bind_rows()

langzeitmessungen_processed <- Langzeitmessungen_raw_list %>%
  map(\(messort_list) {
    map(messort_list, \(year_list) {
      map(year_list, \(df) {
        if (is.data.frame(df)) {
          df %>%
            select(fmin_hz, fmax_hz, service_name, value_v_m, Zeitstempel, Messort_Code)
        }
      }) %>%
        bind_rows()
    }) %>%
      bind_rows()
  }) %>%
  bind_rows()

# combine Langzeitmessungen & Einzelmessungen
combined_data <- bind_rows(einzelmessungen_processed, langzeitmessungen_processed)

# write to disk


zip_csv_to_disk <- function(df, zippedfile) {
  # init temp csv
  temp <- tempfile(fileext=".csv")
  # write temp csv
  write.csv(df, file=temp)
  # zip temp csv
  zip(zippedfile,temp)
  # delete temp csv
  unlink(temp)
}


vroom::vroom_write(x = combined_data, file = "rohdaten_messwerte_unzipped.csv")

zip_csv_to_disk(combined_data, zippedfile = "rohdaten_messwerte.csv.zip")

write.csv(combined_data, file=gzfile("rohdaten_messwerte.csv.gz"))


read_csv(file = "rohdaten_messwerte.csv.zip")

# pipeline

# Get the paths of CSV files
top_folder_path <- "Schulhausmessungen"

csv_paths_list_5 <- extract_csv_paths_einzel(top_folder_path, num_dirs = 5)

csv_paths_list_all <- extract_csv_paths_einzel(top_folder_path, num_dirs = Inf)

csv_paths_list_all_langzeit <- extract_csv_paths_langzeit(top_folder_path = "Langzeit-Messungen")


filtered_einzel <- filter_csv_files_einzel(paths_list = csv_paths_list_5)

filtered_langzeit <- filter_csv_files_langzeit(paths_list = csv_paths_list_all_langzeit)

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

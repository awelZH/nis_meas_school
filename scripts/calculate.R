# Kurbeschreibung:
## Dieses Skript eignet sich um die Berechnung der Werte, welche später für die Visualisierung gebraucht werden, durchzuführen

# 1. Packages und Daten laden
# 2. Schwellerenwerte-Bereinigung. Allen Messwerten wird die Korrekur abgezogen. Werte <0 werden mit 0 ersetzt.
# 3. Rollierende Mittelwert über eine 30 Minuten Messung wird gemacht. Dadurch gibt es nur noch 240 Messwerte pro Frequenzband
# 4. Dataframe wird als csv abgespeichert

# Voraussetzungen:
## Als Input braucht das Skript das File 'rohdaten_messwerte.csv'
## ----------------------------------------------------------------------------------------------------------------

# Parameter:
## (nur ändern, wenn du sicher bist)
path_to_rohdaten_file = 'data/rohdaten_messwerte.csv'
art_der_aggregation = 'center'
path_to_aufbereitem_file = 'data/aufbereite_messwerte.csv'
path_to_schwellenwerte = 'data/schwellenwerte.csv'
path_to_messorte_file = 'data/messorte.csv'
#path_to_schwellenwerte_github = 'https://github.com/awelZH/nis_meas_school/blob/8ed4362b4c72e4e51113e541d75ebc4e16f280dd/data/frequenzbaender_schwellenwerte.csv'
#github_pat = Sys.getenv("github_pat")
##---------------------------------------------------------------------------

# 1. Packages und Daten laden

# Lade genötigte Packages
library(dplyr)
install.packages("zoo")

# Lade benötigte Daten
df_rohdaten <- read.csv(path_to_rohdaten_file) # Load rohdaten file
df_schwellenwerte <- read.csv(path_to_schwellenwerte) # Load rohdaten file
df_messorte <- read.csv(path_to_messorte_file)
##---------------------------------------------------------------------------

# 2. Schwellenwert-Bereinigung:

#Konvertiere Zeitstempel Spalte zu datetime object
df_rohdaten$Zeitstempel <- as.POSIXct(df_rohdaten$Zeitstempel, format = "%Y-%m-%dT%H:%M:%S")
df_rohdaten$Jahr <- format(df_rohdaten$Zeitstempel, "%Y")

# Merge Rohdaten mit Schwellenwerte Daten
df_merged <- merge(df_rohdaten, df_schwellenwerte, by.x = c('Fmin_Hz', 'Fmax_Hz'), by.y = c('Freq_min', 'Freq_max'), all.x = TRUE)

# Führe Schwellenwertkorrektur durch
df_merged$Value_V_per_m_corrected <- df_merged$Value_V_per_m - df_merged$Schwellenwert_MR_Vm

# Ersetze die korrgierten Werte mit 0, wenn sie kleiner als 0 sind
df_merged$Value_V_per_m_corrected <- ifelse(df_merged$Value_V_per_m_corrected < 0, 0, df_merged$Value_V_per_m_corrected)
##---------------------------------------------------------------------------

# 3. Berechne rollierende Mittelwerte

# Erstelle neues Dataframe, wo die einzelnen Frequenzbänder in die Kategorien quadratisch summiert und danach die Wurzel gezogen wird.

# Berechne rollierender Mittelwert
df_grouped_rolling <- df_merged %>%
  dplyr::group_by(Zeitstempel, Jahr, Messort_Code, Kategorie, Messgeraet_Typ) %>%
  dplyr::summarise(value_grouped = sqrt(sum(Value_V_per_m_corrected^2)) # summiert die quadrierten Argumente & gibt die Quadratwurzel einer Zahl zurück
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = "Kategorie", values_from = "value_grouped") %>%
  dplyr::mutate(Mobilfunk_6min_avg = zoo::rollapply(Mobilfunk, 60,mean,align=art_der_aggregation,fill=NA),
                Rundfunk_6min_avg = zoo::rollapply(Rundfunk,60,mean,align=art_der_aggregation,fill=NA),
                WLAN_6min_avg = zoo::rollapply(WLAN, 60,mean,align=art_der_aggregation,fill=NA))

# Bilde Mittelwerte
df_grouped_mean <- df_grouped_rolling %>%
  dplyr::select(Jahr, Messort_Code, Mobilfunk_6min_avg, Rundfunk_6min_avg, WLAN_6min_avg, Messgeraet_Typ) %>%
  rename('Mobilfunk' = 'Mobilfunk_6min_avg', 'Rundfunk' = 'Rundfunk_6min_avg', 'WLAN' = 'WLAN_6min_avg') %>%
  tidyr::pivot_longer(cols = c('Mobilfunk', 'Rundfunk', 'WLAN'), names_to = 'Service', values_to = 'Wert') %>%
  dplyr::group_by(Jahr, Messort_Code, Service, Messgeraet_Typ) %>%
  dplyr::summarise(Wert = mean(Wert))

# Joine Information für finales Dataframe
df_final <- merge(df_grouped_mean, df_messorte[c('Messort_Code', 'Messort_Name', 'Messintervall')], by.x = 'Messort_Code', by.y = 'Messort_Code') %>%
  select(Jahr, Messort_Code, Messort_Name, Service, Wert, Messintervall, Messgeraet_Typ)

##---------------------------------------------------------------------------

# 4. Speichere Daten als CSV
write.csv(df_final, file = path_to_aufbereitem_file)
##---------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------

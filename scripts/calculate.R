# Kurbeschreibung:
## Dieses Skript eignet sich um die Berechnung der Werte, welche später für die Visualisierung gebraucht werden, durchzuführen

# 1. Packages und Daten laden
# 2. Bereinigung der Datum Spalten & Schwellerenwerte-Bereinigung. Allen Messwerten wird die Korrekur abgezogen. Werte <0 werden mit 0 ersetzt.
# 3. Rollierende Mittelwert über eine 6 min Messung wird gemacht. Dadurch resultieren 240 Messwerte pro Service, Messort und Jahr. Pro 240 Werte wird der maximale Wert bestimmt.
# Dadurch hat man pro Messort, Jahr und Service genau einen Wert.
# 4. Dataframe wird als csv abgespeichert

# Voraussetzungen:
## Als Input braucht das Skript das File 'rohdaten_messwerte.csv'
## ----------------------------------------------------------------------------------------------------------------

# Parameter:
## (nur ändern, wenn du sicher bist)
path_to_aufbereitem_file = '~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/aufbereitete_messwerte.csv'
path_rohdaten_messwerte = "~/file-server/file-server/08_DS/01_Projekte/AWEL/2023_Schulhausmessung/rohdaten_messwerte.zip"
url_to_schwellewerte <- 'https://raw.githubusercontent.com/awelZH/nis_meas_school/0ccd1a18257f2debd9f791530eb646d38761446a/data/frequenzbaender_schwellenwerte.csv'
path_to_messorte_file = 'https://www.web.statistik.zh.ch/ogd/daten/ressourcen/KTZH_00002462_00004924.csv'
art_der_aggregation = 'center' # Wie der Rolling mean berechnet wird
rolling_mean_breite = 60 # Wie viele Werte pro Wert im rolling mean berücksichtigt werden
Einheit_kurz = "V/m"
Einheit_lang = "Volt pro Meter"

##---------------------------------------------------------------------------

# 1. Packages und Daten laden

# Lade benötigte Packages
library(dplyr)
library(lubridate)
#install.packages("zoo")

# Lade benötigte Daten
df_rohdaten <- readr::read_csv(path_rohdaten_messwerte) # Lade Rohdaten File
df_schwellenwerte <- read.csv(url_to_schwellewerte) # Lade Schwellenwerte File
df_messorte <- read.csv(path_to_messorte_file) # Lade Schwellenwerte File
##---------------------------------------------------------------------------

# 2. Schwellenwert-Bereinigung:

#Bereinige Datum und Uhrzeit Spalten

#Zuerst werden Datum und Uhrzeit getrennt
temp <- tidyr::separate(df_rohdaten, Zeitstempel, into = c("Datum", "Uhrzeit"), sep = "T")

# Da die Rohdaten verschiedene Formate in den Zeitstempel haben, werden mehrere Schritte ausgeführt, um am Schluss ein einheitliches Datum Uhrzeit Format für alle zu erhalten
temp$datum1 <- as.Date(lubridate::parse_date_time(temp$Datum, c("dmY", "mdY"))) # Mit diesem Code werden die meisten Formate erkennt.
#Ein Format funktioniert nicht und muss im nächsten Schritt verarbeitet werden.
temp$datum_corr <- as.Date(ifelse(is.na(temp$datum1), as.character(as.Date(temp$Datum, format = "%d.%m.%y")), as.character(temp$datum1)))

# Erstelle eine Jahresspalte
temp$Jahr <- format(temp$datum_corr, "%Y")

#Füge die korrigierte Datumspalte mit der Uhrzeitspalte zusammen um ein Timestamp zu erhalten.
temp$Zeitstempel_corr <- paste0(as.character(temp$datum_corr), "T", as.character(temp$Uhrzeit))

#Wähle die relevanten Spalten aus
temp <- temp[c("fmin_hz", "fmax_hz", "service_name", "value_v_m", "Messort_Code", "Jahr", "datum_corr", "Zeitstempel_corr")]

# Formatiere die Datumspalten im Schwellenwerte Dataframe zu Datum
df_schwellenwerte$gueltig_von <- as.Date(df_schwellenwerte$gueltig_von, "%Y-%m-%d")
df_schwellenwerte$gueltig_bis <- as.Date(df_schwellenwerte$gueltig_bis, "%Y-%m-%d")

# In der Spalte gueltig_bis können NA vorkommen. Dort wo NA vorkommen, heisst das, das die Schwellenwerte immer noch gültig sind. Damit der spätere Join funktioniert,
# werden die NA mit dem aktuellen DAtum überschrieben.
df_schwellenwerte <- df_schwellenwerte %>%
  mutate(gueltig_bis = if_else(is.na(gueltig_bis), Sys.Date(), gueltig_bis))

# Merge Rohdaten mit Schwellenwerte Daten. Da ein Inner join gemacht wird, bleiben nur die Zeilen übrig, welche in beiden Dataframe vorkommen. Damit werden z.B alte Services wie "Others I",
# ausgeschlossen.
by <- join_by(fmin_hz == Freq_min, fmax_hz == Freq_max, between(datum_corr, gueltig_von, gueltig_bis))
df_merged <- inner_join(temp, df_schwellenwerte, by)

# Führe Schwellenwertkorrektur durch
df_merged$Value_V_per_m_corrected <- df_merged$value_v_m - df_merged$Schwellenwert_MR_Vm

# Ersetze die korrgierten Werte mit 0, wenn sie kleiner als 0 sind
df_merged$Value_V_per_m_corrected <- ifelse(df_merged$Value_V_per_m_corrected < 0, 0, df_merged$Value_V_per_m_corrected)
##---------------------------------------------------------------------------

# 3. Berechne rollierende Mittelwerte

# Erstelle neues Dataframe, wo die einzelnen Frequenzbänder in die Kategorien quadratisch summiert und danach die Wurzel gezogen wird.

# Berechne rollierender Mittelwert
df_grouped_rolling <- df_merged %>%
  dplyr::group_by(Jahr, Messort_Code, Kategorie, Zeitstempel_corr) %>%
  dplyr::summarise(value_grouped = sqrt(sum(Value_V_per_m_corrected^2)) # summiert die quadrierten Argumente & gibt die Quadratwurzel einer Zahl zurück
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Jahr, Messort_Code, Kategorie) %>% # Groupiere und berechne den rolling mean im nächsten Schritt pro Gruppe
  dplyr::mutate(sixmin_avg = zoo::rollapply(value_grouped, rolling_mean_breite ,mean,align=art_der_aggregation,fill=NA))

# Bilde Mittelwerte
df_grouped_max <- df_grouped_rolling %>%
  rename('Service' = "Kategorie") %>%
  dplyr::group_by(Jahr, Messort_Code, Service) %>%
  dplyr::summarise(Wert = max(sixmin_avg, na.rm = TRUE))

# Joine Information für finales Dataframe
df_final <- merge(df_grouped_max, df_messorte[c('Messort_Code', 'Messort_Name', 'Messintervall')], by.x = 'Messort_Code', by.y = 'Messort_Code') %>%
  mutate(Einheit_kurz = Einheit_kurz,
         Einheit_lang = Einheit_lang) %>%
  select(Jahr, Messort_Code, Messort_Name, Service, Wert, Einheit_kurz, Einheit_lang, Messintervall)


##---------------------------------------------------------------------------

# 4. Speichere Daten als CSV
write.csv(df_final, file = path_to_aufbereitem_file, row.names = FALSE)
##---------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------------

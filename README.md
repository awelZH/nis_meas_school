# nis_meas_school
Repository um die ZH Web Visualisierung, den OGD Datensatz und Ressourcen der NIS Strahlungsmessung des Kantons Zürich zu aktualisieren. 

### Setup

1. Clone das Repository an einen Ort, wo der Zugriff auf die Rohdaten gegeben ist und R-Skripte ausführbar sind.
2. Stelle sicher, dass in der .Renviron Datei die benötigten Variablen (mdv_user, mdv_pw, ZH_METADATEN_API_TOKEN) vorhanden sind.

### Voraussetzungen

- **Login-Daten** für die **Metadatenveraltung (MDV)** sind vorhanden. Um ein Login für die MDV zu erhalten muss die Schulung für Data Stewards
besucht werden. [Anmeldung zur MDV-Schulung](https://www.zh.ch/de/politik-staat/opendata/leitlinien.html)
- In der **MDV** ist der zu **aktualisierende Datensatz bereits
  vorhanden** und die ID bekannt
- Es wird ein Token benötigt, um das [zhMetadatenAPI](https://github.com/statistikZH/zhMetadatenAPI/tree/master) Package von Github herunterladen zu können. Dieser kann bei der
Fach- und Koordinationsstelle OGD bestellt werden <info@open.zh.ch>.

## Anwendungungsfälle

* Berechne und lade alle OGD Daten komplett neu hoch:
  * Öffne das scripts/pipeline.R Skript und überprüfe, ob die Variable mit dem Pfad zu den Rohdaten richtig gesetzt ist und ob die Variable 'full_load' = TRUE und die Variable 'delta_load' = FALSE gesetzt ist. 
  *  Führe das Skript aus

- Berechne und lade **nur** die neusten Daten als OGD hoch.
  * Öffne das scripts/pipeline.R Skript und überprüfe, ob die Variable mit dem Pfad zu den Rohdaten richtig gesetzt ist und ob die Variable 'full_load' = FALSE und die Variable 'delta_load' = TRUE gesetzt ist. 
  *  Führe das Skript aus

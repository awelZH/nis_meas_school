# nis_meas_school

Repository um die ZH Web Visualisierung, den OGD Datensatz und Ressourcen der NIS Strahlungsmessung des Kantons Zürich zu aktualisieren.

## Setup

1.  Clone das Repository an einen Ort, wo der Zugriff auf die Rohdaten gegeben ist und R-Skripte ausführbar sind. Entweder über den [klassischen download des
    Repos](https://github.com/awelZH/nis_meas_school/archive/refs/heads/main.zip)
    oder einen [direkten Download in der R-Studio
    Benutzeroberfläche.](https://happygitwithr.com/rstudio-git-github.html#clone-the-test-github-repository-to-your-computer-via-rstudio)
2.  Stelle sicher, dass in der .Renviron Datei die benötigten Variablen (mdv_user, mdv_pw, ZH_METADATEN_API_TOKEN) vorhanden sind.
3.  Damit die benötigten Packages von Github geladen werden können, muss bei Windows Systemen in der entsprechenden .gitconfig folgendes Statement enthalten sein:

> system("git config --global http.sslbackend schannel")

## Voraussetzungen

-   **Login-Daten** für die **Metadatenveraltung (MDV)** sind vorhanden. Um ein Login für die MDV zu erhalten muss die Schulung für Data Stewards besucht werden. [Anmeldung zur MDV-Schulung](https://www.zh.ch/de/politik-staat/opendata/leitlinien.html)
-   In der **MDV** ist der zu aktualisierende Datensatz und die Ressourcen bereits vorhanden.
-   Es wird ein Token benötigt, um das [zhMetadatenAPI](https://github.com/statistikZH/zhMetadatenAPI/tree/master) Package von Github herunterladen zu können. Dieser kann bei der Fach- und Koordinationsstelle OGD bestellt werden [info\@open.zh.ch](mailto:info@open.zh.ch){.email}.

## Szenarien
Es wird empfohlen, immer die aktuellste Version des Package herunterzuladen. Dies kann über die Schaltfläche "Pull" gemacht werden, sofern das Package bereits einmal geclont wurde. Falls es noch nicht geclont wurde bitte Schritte aus dem Kapitel "Setup" zuerst durchführen. 
Anleitung zum die aktuellste Version herunterzuladen:
1. Stelle sicher, dass du den "main" branch aktiv hast
2. Drücke auf "Pull" damit die aktuellste Version heruntergeladen wird
![image](https://github.com/awelZH/nis_meas_school/assets/46460424/de3ca1f4-a765-4b4d-884a-3100554951cb)

### Hilfe:
> Nach jedem Ausführen des [pipeline.R](scripts/pipeline.R) Skripts werden zwei CSV-Files (prozessierte_messorte.txt, nicht_prozessierte_messwerte.txt) erstellt. In diesen beiden Dateien finden man Informationen, welche Messorte prozessiert wurden und welche Messwerte nicht prozessiert wurden. Es ist good-practice, nach jedem Ausführen des pipeline.R-Skripts, den Inhalt dieser beiden CSV-Files zu überprüfen.

### Szenario 1: OGD Rohdaten und Messwerte um neue Messwerte aktualisieren (inkl. Aktualisieren der Visualisierungen im ZHWeb)
To do:
1. Messwerte müssen lokal am richtigen Ort und in der richtigen Struktur vorhanden sein.
2. Falls neue Messorte dazugekommen sind -> Eintrag in messorte.csv hinzufügen und messorte.csv als OGD manuell im MDV aktualisieren
3. [scripts/pipeline.R](scripts/pipeline.R): Überprüfe ob die Variablen (path_rohdaten_topfolder, full_load = FALSE, delta_load = TRUE) gesetzt sind
4. Starte das pipeline.R Skript

### Szenario 2: Gewisse OGD Rohdaten und Messwerte entfernen (inkl. Entfernung aus der Visualisierungen im ZHWeb)
To do: 
1. Sicherstellen, dass diese Messwerte nicht mehr lokal im Verzeichnis vorhanden sind, sondern verschoben/gelöscht wurden.
2. Eintrag in messort.csv löschen und messorte.csv als OGD manuell im MDV aktualisieren. 
3. [scripts/pipeline.R](scripts/pipeline.R): Überprüfe ob die Variablen (path_rohdaten_topfolder, full_load = TRUE, delta_load = FALSE) gesetzt sind
4. Starte das pipeline.R Skript

### Szenario 3: OGD Rohdaten und Messwerte anhand von anderen Schwellenwerten berechnen und aktualisieren (inkl. Aktualisieren der Visualisierungen im ZHWeb)
Könnte passieren, falls unsere jetzige Berechnung auf einem falschen Wert basiert. Es kann auch sein, dass neue Geräteeinstellungen (bzw. ein ganz neues Gerät) einen neuen Wert (ab Datum xy) erfordern.
To do:
1. Im [Schwellenwert-File](inst/extdata/frequenzbaender_schwellenwerte.csv) die Mutation durchführen und commiten.
3. [scripts/pipeline.R](scripts/pipeline.R): Überprüfe ob die Variablen (path_rohdaten_topfolder, full_load = TRUE, delta_load = FALSE) gesetzt sind
4. Starte das pipeline.R Skript

### Szenario 4: OGD Rohdaten und Messwerte anhand von anderen Services (Mobilfunk, Rundfunk etc.) berechnen und aktualisieren (inkl. Aktualisieren der Visualisierungen im ZHWeb)
Könnte schon passieren, z.B. falls die NISV mit neuen Grenzwerten daherkommt.
To do:
1. Im [Schwellenwert-File](inst/extdata/frequenzbaender_schwellenwerte.csv) die Mutation durchführen und commiten.
3. [scripts/pipeline.R](scripts/pipeline.R): Überprüfe ob die Variablen (path_rohdaten_topfolder, full_load = TRUE, delta_load = FALSE) gesetzt sind
4. Starte das pipeline.R Skript

### Szenario 5: Grenzwerte in Visualiserung im ZHWeb anpassen
To do:
1. Im [Schwellenwert-File](inst/extdata/frequenzbaender_schwellenwerte.csv) die Mutation durchführen und commiten.
2. Nun sollte es auch in der Visualisierung angepasst sein.


## Mögliche Fehlkonfigurationen:
### "Fehler/Vergesslichkeit": Ihr habt neue Messungen in der Ordnerstruktur abgelegt, jedoch messorte.csv OGD nicht aktualisiert oder umgekehrt. 
Kann passieren, nicht schlimm, da:
Das Skript beim "extract" Schritt informativ scheitern wird
To do:
1. Neue Messorte_IDs in messorte.csv OGD nachführen und OGD Ressource aktualisieren (manuell hochladen) und/oder die Messwerte in der Ordner Struktur in der korrekten Struktur ablegen.

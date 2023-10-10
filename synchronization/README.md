# Vorbereitungen
## Nutzerdaten hinterlegen
- Unix: Datei `.netrc` in `/home/<user>` erstellen    
- Windows: Datei `_netrc` in `%userprofile%` (Alias für `C:\Users\<user>`) erstellen

Inhalt: `machine <example.com> login <username> password <password>`     
z.B.: `machine epub-test.uni-regensburg.de login nds1234 password 1234`

Zusätzlich unter Unix: `chmod og-rwx ~/.netrc`

## Python-Pakete installieren
Notwendige Pakete installieren:     
`pip install -r [PFAD]/DTSevaluations/synchronization/requirements.txt`

# Skript manuell starten
Unter Unix zuerst Berechtigungen vergeben: `chmod a+x [PFAD]/DTSevaluations/synchronization/eprints_sword.py`    

Skript manuell ausführen (als Administrator unter Windows):       
`python "[PFAD]/DTSevaluations/synchronization/eprints_sword.py" -p [PFAD]/colorlearning -v`

# Skript automatisieren
## Windows
### Skript anlegen
Eine Datei "update_sword.cmd" anlegen (z.B. im Ordner "DTSevaluations\synchronization") und folgendes hineinschrieben: `python "[PFAD]\DTSevaluations\synchronization\eprints_sword.py" -p [PFAD]\colorlearning -v`    

Eine Beispiel-Datei mit dem Titel `update_sword.cmd` liegt schon bereit.
### Aktion erstellen
"Aufgabenplanung" unter Windows aufrufen und "Aufgabe erstellen"     
- **Allgemein**:
	- Beliebigen "Namen" eingeben   
	- "Mit höchsten Privilegien ausführen" aktivieren
	- "Konfigurieren für" und das laufende Betriebssystem auswählen (siehe `berechtigungen_task.PNG`)
- **Trigger**: beliebiger Trigger, z.B. "Bei Anmeldung"
- **Aktionen**:
	- "Neu" 
	- *Programm/Skript*: `[PFAD]\DTSevaluations\synchronization\update_sword.cmd`
	- *Argumente hinzufügen (optional)*: `> [PFAD]\DTSevaluations\synchronization\log.txt 2>&1`
## Unix
Zuerst überprüfen, ob ein Skript nach dem Booten ausgeführt werden darf:
1. `sudo systemctl status cron.service`
2. Um es zu aktivieren: `sudo systemctl enable cron.service`    

Einen *cronjob* anlegen:
1. `crontab -e`
2. `@reboot python [PFAD]/DTSevaluations/synchronization/eprints_sword.py > [PFAD]/DTSevaluations/synchronization/log.txt 2>&1`
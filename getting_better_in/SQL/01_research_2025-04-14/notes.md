## SQL-Kategorien

In SQL gibt es verschiedene Kategorien von Befehlen, die jeweils unterschiedliche Aufgaben erfüllen. Diese Kategorien sind:

### DDL (Data Definition Language)
DDL-Befehle werden verwendet, um die Struktur der Datenbank zu definieren oder zu ändern. Zu den wichtigsten DDL-Befehlen gehören:

- `CREATE`: Erzeugt eine neue Datenbank, Tabelle, Index oder andere Datenbankobjekte.
``` SQL
CREATE TABLE tabelle (
    spalte1 DATENTYP,
    spalte2 DATENTYP,
    ...
);
```

- `ALTER`: Ändert die Struktur einer bestehenden Datenbank oder Tabelle.
``` SQL
ALTER TABLE tabelle
ADD spalte DATENTYP;
```

- `DROP`: Löscht eine bestehende Datenbank, Tabelle oder ein anderes Datenbankobjekt.
``` SQL
DROP TABLE tabelle;
```

- `TRUNCATE`: Entfernt alle Zeilen aus einer Tabelle, ohne die Tabellenstruktur zu löschen.
``` SQL
TRUNCATE TABLE tabelle;
```

- `RENAME`: Ändert den Namen einer bestehenden Tabelle oder eines anderen Datenbankobjekts.
``` SQL
RENAME tabelle_alt TO tabelle_neu;
```

### DML (Data Manipulation Language)
DML-Befehle werden verwendet, um Daten in der Datenbank zu manipulieren. Zu den wichtigsten DML-Befehlen gehören:

- `INSERT`: Fügt neue Daten in eine Tabelle ein.
``` SQL
INSERT INTO tabelle (spalte1, spalte2, ...)
VALUES (wert1, wert2, ...);
```

- `UPDATE`: Ändert bestehende Daten in einer Tabelle.
``` SQL
UPDATE tabelle
SET spalte1 = wert1, spalte2 = wert2
WHERE bedingung;
```

- `DELETE`: Löscht Daten aus einer Tabelle.
``` SQL
DELETE FROM tabelle
WHERE bedingung;
```

- `MERGE`: Kombiniert Daten aus mehreren Tabellen basierend auf einer Bedingung.
``` SQL
MERGE INTO ziel_tabelle USING quelle_tabelle
ON (bedingung)
WHEN MATCHED THEN
    UPDATE SET ziel_spalte = quelle_spalte
WHEN NOT MATCHED THEN
    INSERT (spalte1, spalte2) VALUES (wert1, wert2);
```

### DQL (Data Query Language)
DQL-Befehle werden verwendet, um Daten aus der Datenbank abzufragen. Der wichtigste DQL-Befehl ist:

- `SELECT`: Wählt Daten aus einer oder mehreren Tabellen aus.
``` SQL
SELECT spalte1, spalte2
FROM tabelle
WHERE bedingung;
```
- `JOIN`: Verknüpft Daten aus mehreren Tabellen basierend auf einer gemeinsamen Spalte.
``` SQL
SELECT tabelle1.spalte1, tabelle2.spalte2
FROM tabelle1
JOIN tabelle2 ON tabelle1.gemeinsame_spalte = tabelle2.gemeinsame_spalte;
```
- `GROUP BY`: Gruppiert die Ergebnisse einer Abfrage basierend auf einer oder mehreren Spalten.
``` SQL
SELECT spalte1, COUNT(*)
FROM tabelle
GROUP BY spalte1;
```

- `HAVING`: Filtert die Ergebnisse einer `GROUP BY`-Abfrage basierend auf einer Bedingung.
``` SQL
SELECT spalte1, COUNT(*)
FROM tabelle
GROUP BY spalte1
HAVING COUNT(*) > 1;
```

- `WITH`: Definiert eine temporäre Ergebnismenge, die innerhalb einer `SELECT`-Abfrage verwendet werden kann (Common Table Expressions, CTE).
``` SQL
WITH cte_name AS (
    SELECT spalte1, COUNT(*)
    FROM tabelle
    GROUP BY spalte1
)
SELECT *
FROM cte_name
WHERE bedingung;
```

### DCL (Data Control Language)
DCL-Befehle werden verwendet, um die Zugriffsrechte und Berechtigungen in der Datenbank zu steuern. Zu den wichtigsten DCL-Befehlen gehören:
- `CREATE USER`: Erstellt einen neuen Benutzer in der Datenbank.
``` SQL
CREATE USER benutzer IDENTIFIED BY passwort;
```

- `GRANT`: Erteilt einem Benutzer bestimmte Rechte auf Datenbankobjekte.
``` SQL
GRANT SELECT, INSERT ON tabelle TO benutzer;
```

- `REVOKE`: Entzieht einem Benutzer bestimmte Rechte auf Datenbankobjekte.
``` SQL
REVOKE SELECT, INSERT ON tabelle FROM benutzer;
```

- `DROP USER`: Löscht einen Benutzer aus der Datenbank.
``` SQL
DROP USER benutzer;
```

- `SET ROLE`: Aktiviert oder deaktiviert eine Rolle für den aktuellen Benutzer.
``` SQL
SET ROLE rolle_name;
```

- `SHOW GRANTS`: Zeigt die Berechtigungen an, die einem Benutzer oder einer Rolle zugewiesen sind.
``` SQL
SHOW GRANTS FOR benutzer;
```

### TCL (Transaction Control Language)
TCL-Befehle werden verwendet, um Transaktionen in der Datenbank zu steuern. Zu den wichtigsten TCL-Befehlen gehören:

- `COMMIT`: Bestätigt alle Änderungen, die durch DML-Befehle vorgenommen wurden.
``` SQL
COMMIT;
```

- `ROLLBACK`: Macht alle Änderungen rückgängig, die seit dem letzten `COMMIT` vorgenommen wurden.
``` SQL
ROLLBACK;
```

- `SAVEPOINT`: Setzt einen Speicherpunkt innerhalb einer Transaktion, zu dem später zurückgekehrt werden kann.
``` SQL
SAVEPOINT speicherpunkt_name;
```

- `SET TRANSACTION`: Legt Eigenschaften für die aktuelle Transaktion fest, wie z.B. die Isolationsstufe.
``` SQL
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
```

Weitere SQL Vokale die ich keiner genauen Kategorie zuordnen kann:

- `EXPLAIN`: Zeigt den Ausführungsplan für eine Abfrage an, um die Leistung zu analysieren.
``` SQL
EXPLAIN SELECT spalte1, spalte2
FROM tabelle
WHERE bedingung;
```

- `ANALYZE`: Aktualisiert Statistiken über die Daten in einer Tabelle, um die Leistung von Abfragen zu verbessern.
``` SQL
ANALYZE TABLE tabelle;
```

- `VACUUM`: Bereinigt und optimiert die Datenbank, indem nicht mehr benötigter Speicherplatz freigegeben wird.
``` SQL
VACUUM tabelle;
```

- `LOCK`: Sperrt eine Tabelle oder Zeile, um sicherzustellen, dass keine anderen Transaktionen gleichzeitig darauf zugreifen.
``` SQL
LOCK TABLE tabelle IN EXCLUSIVE MODE;
```

- `UNLOCK`: Hebt die Sperre von einer Tabelle oder Zeile auf.
``` SQL
UNLOCK TABLE tabelle;
```

- `SET`: Ändert die Konfigurationseinstellungen der Datenbank.
``` SQL
SET parameter_name = wert;
```

- `SHOW`: Zeigt den aktuellen Wert eines Konfigurationsparameters an.
``` SQL
SHOW parameter_name;
```

- `USE`: Wechselt die aktuelle Datenbank.
``` SQL
USE datenbank_name;
```

- `DESCRIBE`: Zeigt die Struktur einer Tabelle an, einschließlich der Spaltennamen und Datentypen.
``` SQL
DESCRIBE tabelle;
```

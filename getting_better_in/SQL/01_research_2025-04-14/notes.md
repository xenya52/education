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
- `ROLLBACK`: Macht alle Änderungen rückgängig, die seit dem letzten `COMMIT` vorgenommen wurden.
- `SAVEPOINT`: Setzt einen Speicherpunkt innerhalb einer Transaktion, zu dem später zurückgekehrt werden kann.
- `SET TRANSACTION`: Legt Eigenschaften für die aktuelle Transaktion fest, wie z.B. die Isolationsstufe.

### Weitere SQL-Befehle
Neben den oben genannten Kategorien gibt es noch weitere wichtige SQL-Befehle, die in keine der Kategorien passen:
- `EXPLAIN`: Zeigt den Ausführungsplan einer SQL-Abfrage an, um deren Leistung zu analysieren.
- `DESCRIBE` oder `DESC`: Zeigt die Struktur einer Tabelle an, einschließlich der Spalten und deren Datentypen.
- `USE`: Wechselt die aktive Datenbank, auf die sich die nachfolgenden SQL-Befehle beziehen.
- `SHOW`: Zeigt Informationen über Datenbankobjekte an, wie z.B. Tabellen, Indizes oder Datenbanken.

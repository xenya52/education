📦 Strukturelle Diagramme (Structure Diagrams)
→ Beschreiben den statischen Aufbau des Systems.

| Diagrammtyp                   | Beschreibung                                                                 |
|------------------------------|------------------------------------------------------------------------------|
| **Klassendiagramm**          | Zeigt Klassen, Attribute, Methoden und Beziehungen (z. B. Vererbung).        |
| **Objektdiagramm**           | Zeigt konkrete Objektinstanzen zu einem bestimmten Zeitpunkt.                |
| **Komponentendiagramm**      | Stellt die Komponenten (Module, Libraries) und deren Schnittstellen dar.     |
| **Verteilungsdiagramm**      | Zeigt die physische Verteilung von Software auf Hardware.                    |
| **Paketdiagramm**            | Dient zur Strukturierung des Systems in logische Pakete.                     |
| **Kompositionsstrukturdiagramm** | Zeigt die interne Struktur einer Klasse und deren Bestandteile.             |
| **Profildiagramm** (UML Profile) | Dient zur Erweiterung der UML mit eigenen Stereotypen und Konventionen.     |

🔄 Verhaltensdiagramme (Behavior Diagrams)
→ Beschreiben Abläufe, Zustände und Interaktionen im System.

| Diagrammtyp                   | Beschreibung                                                                 |
|------------------------------|------------------------------------------------------------------------------|
| **Anwendungsfalldiagramm** (Use Case) | Zeigt die Funktionalität aus Sicht der Benutzer (Akteure).            |
| **Aktivitätsdiagramm**       | Modelliert Abläufe, Workflows und Prozesse mit Entscheidungen.               |
| **Zustandsdiagramm** (State Machine) | Beschreibt Zustände eines Objekts und deren Übergänge.                 |
| **Sequenzdiagramm**          | Zeigt zeitliche Abfolge von Nachrichten zwischen Objekten.                   |
| **Kommunikationsdiagramm**   | Zeigt Nachrichten zwischen Objekten – Fokus auf Struktur statt Zeit.        |
| **Interaktionsübersichtsdiagramm** | Kombiniert mehrere Interaktionen in einer Übersicht.                     |
| **Zeitverlaufsdiagramm** (Timing Diagram) | Zeigt Zustandsverläufe über die Zeit (z. B. bei Echtzeitsystemen). |

**Wichtig**
Ein PAP (Programmablaufplan) gehört nicht zur UML, aber er lässt sich grob der Familie der Verhaltensdiagramme zuordnen – genauer gesagt ist er funktional mit dem Aktivitätsdiagramm in der UML vergleichbar.

| Merkmal                    | PAP (Programmablaufplan)             | UML-Aktivitätsdiagramm             |
|---------------------------|--------------------------------------|------------------------------------|
| Standard                  | Nicht genormt, DIN 66001 (veraltet)  | UML-Standard                       |
| Verwendung                | Klassisch in Ausbildung / Industrie  | In moderner Softwareentwicklung    |
| Darstellung               | Mit Symbolen wie Rechtecken, Rhomben | Mit UML-Notation (Swimlanes, etc.)|
| Fokus                     | Programmtechnische Ablauflogik       | Prozesse, Workflows, Zustandsübergänge |

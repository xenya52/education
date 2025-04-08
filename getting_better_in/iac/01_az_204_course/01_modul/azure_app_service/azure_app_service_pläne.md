Mit einem App Service-Plan wird ein Satz mit Computeressourcen für eine auszuführende Web-App definiert.

### Was wird für jeden APP Service-Plan festgelegt?

- Betriebssystem (Windows, Linux)
- Region („USA, Westen“, „USA, Osten“ usw.)
- Anzahl von VM-Instanzen
- Größe von VM-Instanzen (Klein, Mittel, Groß)
- Tarif (Free, Shared, Basic, Standard, Premium, PremiumV2, PremiumV3, Isolated, IsolatedV2)


### Für was sind app service Pläne da?

Mit dem Tarif eines App Service-Plans wird ermittelt, welche App Service-Features Sie erhalten und welche Kosten für den Plan anfallen

### Was gibt es für Tarife?

- Freigegebene Compute-Ressourcen = Bei Free und Shared (Beide Basistarife) werden die Ressourcen auf einer VM mit anderen Kunden geteilt
  - horizontale Skalierung nicht möglich
- Dedizierte Compute-Ressourcen = Basic, Standard, Premium, PremiumV2 und PremiumV3 auf dedizierten Azure-VMs ausgeführt werden mit anderen Kunden geteilt
  - horizontale Skalierung möglich
- Isolierte Compute-Ressourcen = Isolated und IsolatedV2 auf dedizierten virtuellen Azure-Netzwerken ausgeführt werden und nicht mit anderen Kunden geteilt
  - maximalen Funktionen für die horizontale Skalierung

### Wie wird meine App ausgeführt und Skaliert?

**Dienstebene Free und shared**
- Eine App CPU-Minuten auf einer gemeinsamen VM-Instanz und kann nicht aufskaliert werden

**Auf Dienstebene Basic, Standard, Premium, PremiumV2, PremiumV3 und Isolated**
- Eine App kann auf mehreren VM-Instanzen ausgeführt werden, im App Service-Plan konfiguriert
- Unter einem App Service-Plan können mehrere Apps, auf derselben VM-Instanz ausgeführt werden
- Bei mehreren [Bereitstellungsslots](01_az_204_course/01_modul/azure_app_service/azure_bereitstellungssolts), werden alle Bereitstellungsslots ebenfalls auf denselben VM-Instanzen ausgeführt.
- Wenn Sie Diagnoseprotokolle aktivieren, Sicherungen durchführen oder WebJobs ausführen, werden hierfür auch CPU-Zyklen und Arbeitsspeicher auf diesen VM-Instanzen genutzt.
= *Skalierungseinheit der App Service-Apps*

### Was passiert, wenn meine App mehr Funktionen oder Features benötigt?

- App Service-Plan kann jederzeit zentral hoch- und herunterskaliert werden, hierzu wird einfach der Tarif für den Plan geandert
= Sie können die Leistung der App verbessern indem Computeressourcen von den anderen Apps isolieren, verschieben der App in einen Separaten App Service Plan

**Merke**
*Sie können möglicherweise Kosten sparen, indem Sie mehrere Apps in einem App Service-Plan zusammenfassen. Da alle Apps im selben App Service-Plan jedoch dieselben Computeressourcen gemeinsam nutzen, sollten Sie die Kapazität des App Service-Plans und die erwartete Last für die neue App kennen.*

### Wann wird Isoliert?

- Wenn die App resourcenintensiv ist
- Unabhängig von anderen Apps skalieren
- Die App benötigt ressourcen in einer anderen Geografischen Region

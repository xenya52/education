Jedes Entwicklungsteam verfügt über spezifische Anforderungen, die die Implementierung einer effizienten Bereitstellungspipeline in einem beliebigen Clouddienst erschweren können. App Service unterstützt sowohl die automatisierte als auch die manuelle Bereitstellung.

### Automatisierte Bereitstellung / Continuous Deployment

Ein Prozess, der dazu dient, neue *Funktionen* und *Fehlerkorrekturen* in einem **schnellen** und sich **wiederholenden Muster** mit **minimalen Auswirkungen** auf die Endbenutzer zu implementieren.

Unterstützt die Bereitstellung aus folgenden Quellen:
- Azure DevOps
- GitHub
- Bitbucket

### Manuelle Bereitstellung

Ihnen stehen verschiedene Optionen zur Verfügung, um Ihren Code manuell per Push in Azure zu übertragen:
- Azure CLI - Befehlszeilenschnittstelle ´´´az´´´
  - Beispielweis Azure CLI Befehl zum Bereitstellen einer App:
    ```bash
    az webapp up --name <app-name> --location <location>
    ```
- Git - Gängige Versionskontrollsysteme
  - Beispielweis Azure CLI Befehl zum Bereitstellen einer App:
    ```bash
    az webapp deployment source config --name <app-name> --resource-group <group-name> --repo-url <git-url> --branch <branch-name>
    ```
- ZIP-Bereitstellung - Hochladen einer ZIP-Datei mit dem Code
  - Beispielweis Azure CLI Befehl zum Hochladen einer ZIP-Datei:
    ```bash
    az webapp deployment source config-zip --resource-group <group-name> --name <app-name> --src <zip-file-path>
    ```
- [FTP/S](00_basics/ftp.md) - Dateiübertragungsprotokoll
  - Beispielweis Azure CLI Befehl zum Bereitstellen einer App:
    ```bash
    az webapp deployment source config --name <app-name> --resource-group <group-name> --repo-url <ftp-url> --branch <branch-name>
    ```

### Warum sollte ich Bereitstellungssolts verwenden?

Um eine Stagingumgebung zu erstellen die ich dann Später mit dem Produktionssolt tauschen kann.

- Dadurch wird die Ausfallzeit minimiert
- Fehler können in der Stagingumgebung behoben werden

### Wie kann ich meinen Code Kontinuierlich bereitstellen?

Jeder dieser Branches  sollte, kontinuierlich in einem Stagingslot bereitgestellt werden, um die Funktionalität zu testen und zu Bewerten, bevor sie in die Produktion übernommen wird.

### Wie stelle ich Kontinuierlich meine Container Bereit?

Das Image in einem Stagingslot bereitstellen, und tauschen Sie in die Produktion, um Ausfallzeiten zu vermeiden

- Erstellen Sie das Image, und kennzeichnen Sie es - Git-Commit-ID, dem Zeitstempel oder anderen identifizierbaren Informationen
- Pushen Sie das gekennzeichnete Image
- Aktualisieren Sie den Bereitstellungsslot mit dem neuen Imagetag - Die website startet automatisch neu

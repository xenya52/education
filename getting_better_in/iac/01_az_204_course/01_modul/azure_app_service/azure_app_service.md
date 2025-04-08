
Azure App Service ist ein HTTP-basierter Dienst zum Hosten von Webanwendungen, REST-APIs und mobilen Back-Ends. Sie können in Ihrer bevorzugten Programmiersprache oder Ihrem Lieblingsframework entwickeln. Anwendungen können in Windows- und Linux-basierten Umgebungen problemlos ausgeführt und skaliert werden.

### Was für reccources stellt Azure App Service zur Verfügung?

- die Anzahl der Kerne
- Menge des verfügbaren Arbeitsspeichers
- die Anzahl der Computerinstanzen erhöhen oder verringern, auf denen Ihre Web-App ausgeführt wird.

### Containerunterstüzung

- Unterstützt die Verwendung von Docker-Containern
- Verwendung von privaten Azure-Containerregistrierung möglich
- Verwendung von öffentlichen Docker-Hub
- Zum Orchestrieren (bereitstellen) von containerinstanzen
  - Multi-Container-Apps
  - Windows-Container
  - Docker Compose

### Supports CI/CD

- Azure DevOps
- GitHub
- Bitbucket
- lokale Git-Repository
= *Alle künftigen änderungen werden Automatisch mit dem Programm syncronisiert*

### Bereitstellungssolts

Bereitstellungsslots sind aktive Apps mit eigenen Hostnamen. Elemente für App-Inhalte und -Konfigurationen können zwischen zwei Bereitstellungsslots, einschließlich des Produktionsslots, ausgetauscht werden.

- gesonderten Bereitstellungsslot anstelle des Standardproduktionsslots sind möglich

### App Service unter Linux

- Können nativ unter Linux gehostet werden
- benutzerdefinierte Linux-Container ausgeführt werden. (Diese werden auch als Web-App für Container bezeichnet.)
- unterstützt viele sprachspezifische integrierte Images
  - Node.js, Java (JRE 8 und JRE 11), PHP, Python, .NET, Ruby...
- Bei mangelendem Support können benutzerdefinierte Container verwendet werden

Die Programmiersprachen und ihre unterstützten Versionen werden regelmäßig aktualisiert. Sie können die aktuelle Liste mithilfe des folgenden Befehls in Cloud Shell abrufen.

```bash
  az webapp list-runtimes --os-type linux
```

### Einschränkungen für App Service unter Linux

- wird im Tarif „Shared“ nicht unterstützt
- nur Features angezeigt, die aktuell für Linux-Apps geeignet sind
  - wenn neue Features hinzukommen, werden sie im Portal aktiviert
- Bereitstellung in integrierten Images wird Ihrem Code und Ihren Inhalten ein Azure Storage-basiertes Speichervolume für Webinhalte zugeordnet
  - Datenträgerwartezeit dieses Volumes ist höher und variabler als die Wartezeit des Containerdateisystems

**Merke**
*Apps, die intensiven schreibgeschützten Zugriff auf Inhaltsdateien erfordern, profitieren möglicherweise von der Option „benutzerdefinierter Container“, da die Dateien hierbei im Containerdateisystem platziert werden und nicht auf dem Inhaltsvolume.*

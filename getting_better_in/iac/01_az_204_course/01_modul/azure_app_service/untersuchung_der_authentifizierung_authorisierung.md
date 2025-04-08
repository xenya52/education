Der Azure App Service bietet eine integrierte Authentifizierungs- und Autorisierungsunterstützung. Sie können Benutzer und Benutzerinnen anmelden und auf Daten zugreifen, indem Sie nur wenig oder keinen Code in Ihrer Web-App, RESTful-API, Ihrem mobilen Back-End oder Ihren Azure Functions schreiben.

### Warum die eingebaute Authentifizierung verwenden?

Nutzung ist nicht erforderlich, aber sie bietet folgende Vorteile:
- Mit Azure App Service können Sie verschiedene Authentifizierungsfunktionen in Ihre Web-App oder API integrieren, ohne sie selbst zu implementieren.
- Auth ist direkt in die Plattform integriert und erfordert keine bestimmte Sprache, kein SDK, keine Sicherheitskenntnisse und keinen Code.
- Sie können mit mehreren Anmelde-Anbietern integrieren. Beispiel: Microsoft Entra ID, Facebook, Google, X.

**Merke**
*Viele Webframeworks sind mit Sicherheitsfunktionen gebündelt und können bei Bedarf verwendet werden. Wenn Sie mehr Flexibilität benötigen, als App Service bietet, können Sie auch eigene Hilfsprogramme schreiben.*

### Identitätsanbieter
| Anbieter                              | Anmeldungsendpunkt                | Leitfäden zur Vorgehensweise                          |
|---------------------------------------|-----------------------------------|-------------------------------------------------------|
| Microsoft Identity Platform           | /.auth/login/aad                  | Anmeldung bei App Service Microsoft-Identitätsplattform |
| Facebook                              | /.auth/login/facebook             | App Service Facebook-Anmeldung                        |
| Google                                | /.auth/login/google               | Google-Anmeldung App Service                          |
| X                                     | /.auth/login/twitter              | App Service X-Anmeldung                               |
| Ein beliebiger OpenID Connect-Anbieter | /.auth/login/<providerName>       | App Service OpenID Connect-Anmeldung                  |
| GitHub                                | /.auth/login/github               | App Service: mit GitHub anmelden                      |

### Wie Funktioniert es genau?

Wenn das Modul für Authentifizierung und Autorisierung, im Sandbox wie Ihr Anwendungscode ausgeführt
  - Authentifizieren von Benutzerinnen und Benutzern und Clients mit dem angegebenen Identitätsanbieter
  - Überprüfen, Speichern und Aktualisieren von OAuth-Token, die vom konfigurierten Identitätsanbieter ausgestellt wurden
  - Verwaltung der authentifizierten Sitzung
  - Einfügen von Identitätsinformationen in HTTP-Anforderungsheader

**Merke**
*Wenn das Modul wird separat von Ihrem Anwendungscode ausgeführt und kann über die Azure Resource Manager-Einstellungen oder per Konfigurationsdatei konfiguriert werden. Weder SDKs noch bestimmte Programmiersprachen oder Änderungen am Anwendungscode sind erforderlich.*

### Authentifizierungsfluss

Authentifizierungsflow ist für alle Anbieter gleich, unterscheidet sich jedoch abhängig davon, ob die Anmeldung mit dem SDK des Anbieters erfolgen soll.

### Schritte des Authentifizierungsflusses

| Schritt                              | Ohne Anbieter-SDK                                                                 | Mit Anbieter-SDK                                                                                          |
|--------------------------------------|----------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------|
| Anmeldung des Benutzers              | Client wird zu /.auth/login/<provider> umgeleitet.                               | Clientcode meldet Benutzer direkt mit dem Anbieter-SDK an und erhält ein Authentifizierungstoken. Informationen finden Sie in der Dokumentation des Anbieters. |
| Nachauthentifizierung                | Anbieter leitet Client zu /.auth/login/<provider>/callback um.                   | Clientcode sendet Token des Anbieters zur Überprüfung an /.auth/login/<provider>.                         |
| Einrichten der authentifizierten Sitzung | App Service fügt der Antwort ein authentifiziertes Cookie hinzu.                 | App Service gibt das eigene Authentifizierungstoken an den Clientcode zurück.                             |
| Bereitstellen von authentifiziertem Inhalt | Client schließt Authentifizierungscookie in nachfolgenden Anforderungen (die automatisch vom Browser verarbeitet werden) ein. | Clientcode stellt Authentifizierungstoken im X-ZUMO-AUTH-Header (der automatisch von Mobile Apps-Client-SDKs verarbeitet wird) bereit. |


### Wo werden die Tokens gespeichert?

App Service bietet einen integrierten Tokenspeicher. Dabei handelt es sich um ein Repository mit Token, die den Benutzern Ihrer Web-Apps, APIs oder nativen mobilen Apps zugeordnet sind. Wenn Sie die Authentifizierung für jeden Anbieter aktivieren, ist dieser Tokenspeicher sofort für Ihre App verfügbar


### Wie wird Protokolliert und nachverfolgt?

**Wenn man Anwendungsprotokollierung aktiviert** dann Ablaufverfolgungen für Authentifizierung und Autorisierung direkt in Ihren Protokolldateien gesammelt. Dadurch kann man bei einem unerwarteter Authentifizierungsfehler alle Details nachvollziehen.

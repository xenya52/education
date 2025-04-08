Standardmäßig sind Apps, die in App Service gehostet werden, direkt über das Internet zugänglich und können nur Endpunkte erreichen, die im Internet gehostet werden. Für viele Anwendungen müssen Sie jedoch den ein- und ausgehenden Netzwerkdatenverkehr kontrollieren.

### Was für Arten von Netzwerkfunktionen gibt es?
zwei Hauptbereitstellungstypen für [Azure App Service](az_204_course/01_modul/vocabulary/azure_app_service.md) Netzwerkfunktionen:
- mehrinstanzenfähigen öffentlichen Dienst, der App Service-Pläne in den Preis-SKUs (Tarifen) „Free“, „Shared“, „Basic“, „Standard“, „Premium“, „PremiumV2“ und „PremiumV3“ hostet
- App Service-Umgebung (App Service Environment, ASE) für einzelne Mandanten, die App Service-Pläne für SKUs in Tarifen des Typs „Isoliert“ direkt in Ihrem virtuellen Azure-Netzwerk hostet

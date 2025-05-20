# Java Beans Grundlagen

## Kurze erklärung

Eine JavaBean ist einfach ein Standard. Es handelt sich um eine reguläre Java-Klasse, außer dass sie bestimmten Konventionen folgt.

## Eigenschaften einer Bean

- Ein öffentlicher parameterloser Konstruktor
- Serialisierbarkeit in Java durch das interface aus java.io gewährleisten
- Alle attribute sind privat / Öffentliche Zugriffsmethoden (= public getter / setter)

## Serialisierbarkeit

Serialisierbarkeit einer Klasse wird dadurch ermöglicht, dass die Klasse das Interface java.io.Serializable implementiert. Klassen, die dieses Interface nicht implementieren, können ihren Zustand weder serialisieren noch deserialisieren. Alle Untertypen einer serialisierbaren Klasse sind ebenfalls serialisierbar. Das Serialisierungs-Interface hat keine Methoden oder Felder und dient lediglich dazu, die Semantik der Serialisierbarkeit zu kennzeichnen.
Mit anderen Worten: Serialisierbare Objekte können in Streams geschrieben werden und somit in Dateien, Objektdatenbanken oder Ähnliches.

## Unterschiede zwischen einer Bean und einer normalen Java Klasse

Es gibt keinen syntaktischen Unterschied zwischen einer JavaBean und einer anderen Klasse – eine Klasse ist eine JavaBean, wenn sie den Standards folgt.

## Warum gibt es den Begriff

Es gibt einen Begriff dafür, weil der Standard es Bibliotheken erlaubt, programmatisch mit Klasseninstanzen zu arbeiten, die auf eine vordefinierte Weise gestaltet sind. Zum Beispiel: Wenn eine Bibliothek ein beliebiges Objekt streamen soll, das Sie ihr übergeben, kann sie dies tun, da Ihr Objekt serialisierbar ist (vorausgesetzt, die Bibliothek setzt voraus, dass Ihre Objekte den JavaBean-Standards entsprechen).

### Use-Cases einer JavaBean

Die use cases lassen sich so nicht leicht definieren und sind von anwendung zu anwendung unterschiedlich und kommt somit ganz auf die Bean an (SpringBean, EnterpriseBean)

## Beispiele

### Problemstellung: Verwaltung von Benutzerdaten

Angenommen, wir möchten eine Anwendung erstellen, die Benutzerdaten wie Name, E-Mail-Adresse und Alter speichert und verwaltet. Eine JavaBean kann verwendet werden, um diese Daten zu kapseln und den Zugriff darauf zu standardisieren.

#### Lösung mit einer JavaBean

Eine JavaBean namens `User` könnte wie folgt aussehen:

```java
import java.io.Serializable;

public class User implements Serializable {
    // Private Attribute
    private String name;
    private String email;
    private int age;

    // Parameterloser Konstruktor
    public User() {}

    // Getter und Setter
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }
}
```

Diese JavaBean erfüllt die Standards: Sie hat einen parameterlosen Konstruktor, private Attribute und öffentliche Getter- und Setter-Methoden. Außerdem implementiert sie `Serializable`, sodass sie serialisiert werden kann.

#### Wie das Problem gelöst wird

Die JavaBean `User` ermöglicht es, Benutzerdaten in einer standardisierten Weise zu speichern und darauf zuzugreifen. Zum Beispiel kann eine Bibliothek, die Benutzerdaten in einer Datenbank speichert, die Getter- und Setter-Methoden verwenden, um die Daten zu lesen und zu schreiben. Da die Klasse serialisierbar ist, können die Benutzerdaten auch in einer Datei gespeichert oder über das Netzwerk übertragen werden.

## Quellen

1. [JavaBeans -- Wikipedia](https://de.wikipedia.org/wiki/JavaBeans)
2. [What is a JavaBean? -- Stack Overflow](https://stackoverflow.com/questions/3295496/what-is-a-javabean-exactly)
3. [JavaBean vs SpringBean vs Pojo](https://www.shaunabram.com/beans-vs-pojos/)

# Video to Java

## Java Platforms
Java SE (standard)
  - Java Standart Edition
  - Core plattform
  - Has all Core libaries and APIs every programmer should learn
  - Define Basic types and obkects to high-level classes
  - JDK (JavaDevelopment Toolkit)
    All the common classes and Toolkits
      JVM
      Development tools
      Deployment and monitoring
Java ME (micro)
  - Supset of Java SE
  - For Mobile devices
  - Small-footbrint on JVM
  - Small devices (Sensores, Embedded)
Java FX (eff,ects)
  - Rich internet Applications
  - User-interface API
  - Hardware-accelerated graphics
  - High performance clients
  - High level APIs to connect to remote Services
  - Modern look and feel
  - Rich user interfaces
Java EE (enterprise)
  - Java EE extends Java SE
  - Proviting an API and Runtime Enviroment
  - Enterprise Software
  - Large Scale
  - Distributed system
  - Consider Java EE instead of Java SE
  - SE, EE, FX are often clients from Java EE
  - Designed to create Multi-tiered, scalable, reliable and secure applications Short name is **Enterprise Application**

Each Java Platform provides a JVM and API
Run on any **compatible** system
Take the advantages of the Java language (Stability, User Development and Security)
Widely used
"Enterprise Applications"
All Java plattforms consits on a **Java Virtual Machine**(JVM)

1. Java Application
2. Application Programming Interface (API)
3. Java Virtual Machine (JVM)
4. Operationg System
5. Hardware

## Java EE

Java EE is there to quickly and easily develop an complex enterprise application. Readable and mmaintainable code.

### Architecture

Centered about the **runtime enviroment (usally called Container)**. This Container provides **services** so that the components can host lifecycle management, dependency injection and so on, These components use **well defined contracts (APIs)** to communicate with the Java EE infrastructure. **Components** needs to be packaged in a standart way. Before there getting deployed to a **Container**. Once deployed there are several components that can be accessed to use this **Container**. **Java SE APIs** can also be used by every **Java EE Component**.

### Container

The **container** is a **Runtime Enviroment**. His primary Role is to **hide technical complexity** and **enhance portability** to the **host applications**. Developer can **concentrate on writing business code** and the **container handles ressource management, multi-threading, ressource pulling** and all other **complex low level details**. Container > components.

### Components

The **container manages components**, components can be **static or dynamic Web pages** that processes requests, construct responses. The **components** can also be **server-side** classes, that **handle buisness code** and **process data**. We can even have **components** that can **access legacy systems**

### Services

The intend of an **container** is to **provide general services** to the **components** from the "outside". These services can be security, transaction management, naming, remote connectivity and so on. **Services can be configured** and the **configuration is isolated**. For example **one component** can have **security settings** that allow you **access** to a certain **Database and another level of Database access** from another **component** thats when **metadata** comes to play.

### Metadata

The **services support** is defined by some combination of **User code and Metadata**, that together follow a **contract for interacting with a container**. **Metadata** can take form of an **annotation** that is **written inside the code**. Or in an **external XML descriptor**. These extra information is giving by the **component to the container**. The **component** will then configure the **appropiate service** to the **component by deployment time**. The advantage for the developer, the **component doesnt have to create these services, it is just using them (= concept of inversion of control)**. So, the **container** takes out control** of our **buisness code** and **provides techical services**.

### Application v1

An **application** is an **aggregation of components**. For example, if you think of a **web application** you need a few **web-pages, web-resources (images, videos, stylesheets), buisness-components and database-access-components**. If the **metadata** is **defined** in an external **XML-file** rather than **annotations**. Than the aookucatuibs also needs these **external deployment descriptions**.

### Packaging

For these **applications** to be executed, we need to assemble all these **components** together into an **archive** and **deploy** it to the **container**. Once deployed **the application** is ready to run. The packaging has a standart format, so it can be executed in any **container** what increases portability.

### Application(s) v2

Once we depoly **one application** to a **container** we can deploy other ones. A **container** can handle **several applications** and **isolate them** from other applications. Each **applications** has its **own ressources, own components, own class-loader**. The container administers ech one separately, meaning that we can stop or restart only **one application**, while the **ther applications** are still answering user requets.

### Protocols

To interact with **one application Java EE defines** a set of allowed **protocols**, like HTTP, HTTP(S = SSL). It allows **RMI/IIOP** too. **RMI (Remote Method Invocation)** is a Java protocol to allow remote objects invocation **independently of the underlying protocol**. **RMI** over **Iiop (Internet Inter-ORB Protocol)** is an extension to integrate with **Corba**.. It allows **Java EE components** to invoke external **Corba objects**, using the **Iiop** protocol. **Corba objects** can be written in any language such as C, C++, COBOL as well as java.

### Clients

The **clients** of these **protocols** can be Web-Clients, web-applications, rest or soap web-services, mobile-applications, B2B web-browsers and so on... In short, anything that understands **Http, Https or RMI**. Every Java protocol can be a **Java EE** client.

### Distribution

This is what a distribution looks like a set of Java clients working in a **aggregation, or composition** with each other. We can **distribute** different **components** on the same **application** into **several containers** or **distribute the same application** several times. This allows **load-balancing** between the instances, but also **failover**. **Distribution** is one of the strengths of **Java EE** as it brings **scalability** and **availability in a transparent way**

## Java EE services

Many **service** are out there where the Java EE code can benefit from. Java EE depends on Java SE so we can benefit from **JAVA SE services**

### Java EE Service Tiers

The **components (services?)** are only a "logical" representation and the **components (services?)** dont have to restrict to the **services** of the layer they live in. In fact **Java EE** is very **modular** and there are enough **services** to build all sorts of **applications**. Ofc we dont need to use all of them in one **application**.

| Tier                     | Description                                                                                                                                                                                                                     | Key Services                                                                                                                                                                                                                     |
|--------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Java EE Business Tier** | Handles **business logic**, processes, and stores data.                                                                                                                                                                       | - **Persistence**: Maps objects to relational databases, includes a high-level query language.<br>- **Transaction Management**: Handles relational databases and messaging systems.<br>- **Batch Processing**: Defines non-interactive, long-running jobs.              |
| **Java EE Web Tier**      | Manages interaction between **web-clients** and the **business tier**.                                                                                                                                                        | - **Servlets**: Handle HTTP/HTTPS requests and generate dynamic web pages.<br>- **Expression Language**: Binds graphical components to backend services.<br>- **WebSockets**: Enables bidirectional communication between web-clients and servers.                     |
| **Interoperability Tier** | Facilitates interaction with **external services** and supports multiple protocols.                                                                                                                                           | - **SOAP and REST Services**: Used for B2B applications.<br>- **Messaging Services**: Enables asynchronous communication (point-to-point and publish-subscribe models).<br>- **Java Mail API**: Simplifies email handling.<br>- **Connector API**: Accesses databases, mainframes, or enterprise resources. |
| **Common Services**       | Provides services shared across tiers.                                                                                                                                                                                        | - **Dependency Injection**: Automatically injects resources or components.<br>- **Interception**: Adds extra behavior to invocations.<br>- **Security**: Authenticates and enforces access control.<br>- **Concurrency**: Manages concurrent tasks.                   |
| **Java SE**               | Forms the foundation of Java EE, providing essential features and APIs.                                                                                                                                                       | - **Database Access**: Lower-level API for database interaction.<br>- **Naming and Directory Interface**: Associates names with objects and finds them in directories.<br>- **XML Processing**: Handled by Java SE.<br>- **RMI over IIOP**: Manages low-level communication. |

#### Java EE Business Tier

The **business tier** is the one that handling the **business logic** and the one procssing and storing data. Because of that we'll find all the *services** to handle **persistence and transaction management**. In fact **transaction management** has been at the genesis of Java EE and is very powerful as it not only **deals with relational databases** but also with **messaging system**. The **service of persistence** does the **mapping between objects and relational databases**. The **service of persistence** also comes with a **high level query language** so we can query objects without worrying about the underlying database. The business provides **services to validate and constrain** our business model and witch **batch processing services** that allow developers to easily define **non-interactive, bulk oriented, long running jobs.

#### Java EE Web Tier

The **web tier** handles the interaction between the **web-clients** and the **business tier**. It has all the needed **services** to handle **Http and Https** requests trough whats called **servlets**. **Serlets** can generate dynamic Web-Pages written in HTML5. Todo the **binding** between **graphical components** to the **backend services** Java EE uses an **expression language** that takes its roots from the Java language itself. **Web-sockets** are also allowed by **Java EE** and are classified in the **Java EE Web Tier**. The **Websockets** allow **bidirectional conversations** between **web-clients** and **servers**.

#### Interoperability Tier

The **interoperability tier** allows interaction with **external services** and is one of the biggest **Java EE strengths**. It can **exposure or consume services** on top of **several protocols**. On **Http** we find the **SOAP- and REST-services**. **SOAP and REST** are heavily used in B2B applications. As for the exchange format, Java EE comes with **several services to process, parse, generat, transform and query Json**. There are also **messaging services**. **Messaging services** enables applications to communicate asynchronously trough messages. It supports **reliable point messaging** as well as the **publish subscribe model**. Very easily implementation in the **interoperability tier** to send emails, and this can be implemented very easily trough the Java Mail API. **Javas EE interoperability tier** has a **connector** API. These connectors allow you to access any kind of database, mainframe or even enterprise are any kind of **resource planning program**. The **Kinect API** lets you create your own connectors.

#### Services that are common (to most) tier

Finally, **Java EE** comes with a set of **services** that are common to most of these layers. **Dependency injection** is the most used one. Any resource, any component can be **injected** into any other component. These hides the creation and lookup of resources from the **application code**. It let the container to automatically insert references of all the requiered **components** or resources. At the very heart of the **container** is the ablitiy to **intercept** any **invocation** and add extra behaivor. This power is also at developers end and brings lots of flexibility to an **applications**. An application would not be **Java EE without some sort of security**. **Java EE** enables **services** to authenticate and enforce access controll upon users. It can define a contract between the **container** and an in-house authorization service provider, allowing custom authorization to be plugged in any component. **Concurrency** is also something very important in java.

#### Java SE

It is also important to stress out that **Java EE is a superset of Java SE**. This means that all features of the **Java SE** platform are valuable in **Java EE** as well as the **Java EE APIs**. There are all usefull, but some of them are **actually needed by Java EE** like Database access. **Java EE** gives a nice high level API that abstracts the code from the database structure, but this relies a **lower level API from Java SE**. **Java SE** has also a **naming and directory interface** that is used by **injection to associate names** to objects and then to **find these objects in a directory**. Json is a **"Java EE service"** but XML processing is done by **Java SE**. **RMI over Iiop** protocol manages low level communication between objects and **therefore is defined Java SE**. In **Java SE** we can **administer** the **container** and the **components** thanks to the mangement API defined in **Java SE**.
{{REWRITTEN_CODE}}

## Some other notes

- Java EE is open source, free to use standartized language
- Java SE runs everywhere, Java EE programs brings extra portability because the apps written with java ee are running in containers
- Deploy an application to different containers
- Java EE is good at integrating external systems (Supports HTTP and HTTPs)

## Java Deployment

- Local deployment: GlassFish, JBoss, WildFly, Tomcat, JOnAS
- Cloud: Oracle Cloud, Google Cloud, Jelastic, AWS
- Tools: Containers with Docker

## Packaging

.jar File = Java SE / Java EE applications
.war File = Web applications
.ear File = Enterprise applications

Easy deployment with one file (.jar, .war, .ear). In some cases you need to include a **deployment descriptor** which is an XML file that includes deployment settings or the **application module or component**. You use the **deployment descriptor** to descripe the deployment or runtime settings and specs that can be used when deployment is executed or the application runs on the server.

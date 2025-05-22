# Gradle basics

I wanna collect some information about gradle itself because it's very interersting if you're only working with maven to build your java projects.

## Folder structure

I wanna explain the **folder structure** of a `basic gradle init` more detailed.
This is what a basic **gradle project** looks like:
├── build
│   └── reports
│       └── configuration-cache
│           └── 9rt5xfprmkmc0nv5h4x2f2wul
│               └── 2bna91s0fyubu623pi7cfdvno
│                   └── configuration-cache-report.html
├── build.gradle.kts
├── gradle
│   ├── libs.versions.toml
│   └── wrapper
│       ├── gradle-wrapper.jar
│       └── gradle-wrapper.properties
├── gradle.properties
├── gradlew
├── gradlew.bat
├── ObjectDiagram.txt
├── README.md
└── settings.gradle.kts

More detailed explanation for some *special files*:

- `settings.gradle.kts` - Sets up some high level configurations for a project
- `build.gradle` - Is the high script file to build the gradle project
- `gradlew` - Is the wrapper script for macOS and linux
- `gradlew.bat` - Is the wrapper script for Windows

## Gradle components

**projects, build scripts, tasks, plugins**

**Project** is the highest level of the gradle concept. It's a container for everything that graidle knows about the application.

Each gradle project can have a **build script** (build.gradle).

Gradle **tasks** are individual build actions, running from the command line. `./gradlew <task-name>`

Gradle **plugin** can be applied to the build script what will automatically adds tasks to the project.

## Using the gradle wrapper (linux / macOS view)

`./gradlew tasks` - Shows me all available tasks that I can run currently in this freshly build project

## Domain specific language

Groovy or Kotlin can be DSL (Domain specific languages) for the Gradle enviroment.

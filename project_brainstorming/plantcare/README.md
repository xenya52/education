# Plantcare

## Overview

TODO

## Project goal

This project aims to create a simple and easy-to-use plant care application that helps users take care of their plants. The application will provide additonal informations on how to care on your manually added plant, including watering schedules, light requirements, and other care tips. Furthermore you should be able to work with a in build watering timer, who reminds you when you need to water your plants. The goal is to make plant care accessible and enjoyable for everyone, regardless of their level of experience and a pretty UI.

## Features

- Add, delete and manage plants
  - Plant with name
  - Species,
  - Watering schedule
  - Light requirements
  - care tips (notes)

## Architecture

- For the **Plantcare application itself**, I wanna use the MVC pattern. The model will be responsible for managing the data, the view will handle the user interface, and the controller will manage the interaction between the model and the view.
- For the **database**, I will use SQLite as a lightweight and easy-to-use database solution. SQLite is a self-contained, serverless, and zero-configuration database engine that is perfect for small to medium-sized applications. It is also cross-platform and can be easily integrated into the application.

## Technologies

- **Programming Language**: TypeScript
- **Framework**: React
- **Database**: SQLite
- **State Management**: Redux
- **UI Library**: Material-UI
- **Testing**: Jest, React Testing Library
- **Build Tool**: npm
- **Version Control**: Git

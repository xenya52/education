# Plantcare

---

## Overview

TODO

---

## Project Goal

The goal of this project is to create a plant care application that:
- Allows users to add, delete, and manage their plants.
- Provides detailed care information for each plant.
- Includes a watering timer to remind users when to water their plants.
- Offers a clean and intuitive user interface.

---

## Features

- **Plant Management**:
  - Add, delete, and manage plants.
  - Store plant details such as:
    - Name
    - Species
    - Watering schedule
    - Light requirements
    - Care tips (notes)
- **Watering Timer**:
  - Notify users when it's time to water their plants.
- **User-Friendly Interface**:
  - Clean and visually appealing UI built with Material-UI.

---

## Architecture

The application follows a **3-tier architecture**:

1. **Presentation Layer (Frontend)**:
   - Built with React and Material-UI.
   - Handles user interactions and displays plant data.
   - Communicates with the backend via RESTful APIs.
   - Uses `react-router-dom` for navigation and routing.

2. **Business Logic Layer (Backend)**:
   - Built with Node.js and Express.js.
   - Exposes RESTful APIs for CRUD operations on plants:
     - `GET /plants`: Fetch all plants.
     - `GET /plants/:id`: Fetch a specific plant.
     - `POST /plants`: Add a new plant.
     - `PUT /plants/:id`: Update a plant.
     - `DELETE /plants/:id`: Delete a plant.
   - Validates and sanitizes user input.
   - Handles errors and enforces security.

3. **Data Layer (Database)**:
   - Uses SQLite as a lightweight and serverless database.
   - Stores plant data in a `plants` table.
   - Provides efficient CRUD operations via SQL queries.

---

## Technologies

- **Programming Language**: TypeScript
- **Frontend Framework**: React
- **Backend Framework**: Node.js with Express.js
- **Build Tool**: Vite
- **Database**: SQLite
- **State Management**: React Context API (or Redux if needed)
- **UI Library**: Material-UI
- **Testing**: Jest, React Testing Library
- **Version Control**: Git

---

## Development Workflow

### **Frontend**
1. Built with React and styled using Material-UI.
2. Fetches data from the backend via RESTful APIs.
3. Handles routing with `react-router-dom`.

### **Backend**
1. Built with Node.js and Express.js.
2. Handles business logic and database interactions.
3. Exposes RESTful APIs for the frontend.

### **Database**
1. SQLite database stores plant data.
2. CRUD operations are abstracted into a repository layer.

---

## Future Enhancements

- **Authentication**:
  - Add user accounts to allow multiple users to manage their own plants.
- **Mobile Support**:
  - Make the application fully responsive for mobile devices.
- **Advanced Notifications**:
  - Add push notifications for watering reminders.
- **Plant Image Upload**:
  - Allow users to upload images of their plants.

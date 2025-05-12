const path = require("path");
const betterSqlite3 = require("better-sqlite3");

// Path to the SQLite database file
const dbPath = path.resolve(__dirname, "plantcare.db");

// Initialize the database connection
const db = betterSqlite3(dbPath);

// Ensure the `plants` table exists
try {
  db.exec(`
    CREATE TABLE IF NOT EXISTS plants (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      species TEXT NOT NULL,
      watering_schedule INTEGER NOT NULL,
      light_requirements TEXT NOT NULL,
      care_tips TEXT
    )
  `);
  console.log("Database initialized and `plants` table ensured.");
} catch (error) {
  console.error("Error initializing database:", error);
  process.exit(1); // Exit the process if table creation fails
}

module.exports = db;

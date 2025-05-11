import Database from "better-sqlite3";

// Initialize the SQLite database
const db = new Database("plantcare.db", { verbose: console.log });

// Create the plants table if it doesn't exist
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

// Define the Plant type
export interface Plant {
  id?: number;
  name: string;
  species: string;
  watering_schedule: number;
  light_requirements: string;
  care_tips?: string;
}

// Model functions
const PlantModel = {
  // Create a new plant
  createPlant: (plant: Omit<Plant, "id">): number => {
    const stmt = db.prepare(`
      INSERT INTO plants (name, species, watering_schedule, light_requirements, care_tips)
      VALUES (?, ?, ?, ?, ?)
    `);
    const result = stmt.run(
      plant.name,
      plant.species,
      plant.watering_schedule,
      plant.light_requirements,
      plant.care_tips || null,
    );
    return result.lastInsertRowid as number;
  },

  // Get all plants
  getAllPlants: (): Plant[] => {
    const stmt = db.prepare(`SELECT * FROM plants`);
    return stmt.all() as Plant[];
  },

  // Get a plant by ID
  getPlantById: (id: number): Plant | undefined => {
    const stmt = db.prepare(`SELECT * FROM plants WHERE id = ?`);
    return stmt.get(id) as Plant | undefined;
  },

  // Update a plant
  updatePlant: (id: number, plant: Omit<Plant, "id">): boolean => {
    const stmt = db.prepare(`
      UPDATE plants
      SET name = ?, species = ?, watering_schedule = ?, light_requirements = ?, care_tips = ?
      WHERE id = ?
    `);
    const result = stmt.run(
      plant.name,
      plant.species,
      plant.watering_schedule,
      plant.light_requirements,
      plant.care_tips || null,
      id,
    );
    return result.changes > 0;
  },

  // Delete a plant
  deletePlant: (id: number): boolean => {
    const stmt = db.prepare(`DELETE FROM plants WHERE id = ?`);
    const result = stmt.run(id);
    return result.changes > 0;
  },
};

export default PlantModel;

const db = require("better-sqlite3")("plantcare.db");

const PlantRepository = {
  getAllPlants: () => db.prepare("SELECT * FROM plants").all(),
  createPlant: (plant) =>
    db
      .prepare(
        "INSERT INTO plants (name, species, watering_schedule, light_requirements, care_tips) VALUES (?, ?, ?, ?, ?)",
      )
      .run(
        plant.name,
        plant.species,
        plant.watering_schedule,
        plant.light_requirements,
        plant.care_tips,
      ).lastInsertRowid,
  updatePlant: (id, plant) =>
    db
      .prepare(
        "UPDATE plants SET name = ?, species = ?, watering_schedule = ?, light_requirements = ?, care_tips = ? WHERE id = ?",
      )
      .run(
        plant.name,
        plant.species,
        plant.watering_schedule,
        plant.light_requirements,
        plant.care_tips,
        id,
      ).changes > 0,
  deletePlant: (id) =>
    db.prepare("DELETE FROM plants WHERE id = ?").run(id).changes > 0,
};

module.exports = PlantRepository;

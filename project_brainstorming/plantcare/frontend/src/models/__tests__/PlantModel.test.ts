import PlantModel from "../PlantModel";
import type { Plant } from "../PlantModel";

beforeEach(() => {
  // Clear the plants table before each test
  const db = require("better-sqlite3")("plantcare.db");
  db.exec("DELETE FROM plants");
});

describe("PlantModel", () => {
  it("should create a new plant", () => {
    const plantData: Omit<Plant, "id"> = {
      name: "Test Plant",
      species: "Test Species",
      watering_schedule: 3,
      light_requirements: "Bright Light",
      care_tips: "Test care tips",
    };

    const plantId = PlantModel.createPlant(plantData);
    expect(plantId).toBeGreaterThan(0);

    const createdPlant = PlantModel.getPlantById(plantId);
    expect(createdPlant).toMatchObject(plantData);
  });

  it("should fetch all plants", () => {
    const plantData: Omit<Plant, "id"> = {
      name: "Test Plant",
      species: "Test Species",
      watering_schedule: 3,
      light_requirements: "Bright Light",
      care_tips: "Test care tips",
    };

    PlantModel.createPlant(plantData);
    const plants = PlantModel.getAllPlants();
    expect(plants.length).toBeGreaterThan(0);
  });

  it("should fetch a plant by ID", () => {
    const plantData: Omit<Plant, "id"> = {
      name: "Test Plant",
      species: "Test Species",
      watering_schedule: 3,
      light_requirements: "Bright Light",
      care_tips: "Test care tips",
    };

    const plantId = PlantModel.createPlant(plantData);
    const plant = PlantModel.getPlantById(plantId);

    expect(plant).toBeDefined();
    expect(plant?.name).toBe("Test Plant");
  });

  it("should update a plant", () => {
    const plantData: Omit<Plant, "id"> = {
      name: "Test Plant",
      species: "Test Species",
      watering_schedule: 3,
      light_requirements: "Bright Light",
      care_tips: "Test care tips",
    };

    const plantId = PlantModel.createPlant(plantData);

    const updated = PlantModel.updatePlant(plantId, {
      name: "Updated Plant",
      species: "Updated Species",
      watering_schedule: 5,
      light_requirements: "Low Light",
      care_tips: "Updated care tips",
    });

    expect(updated).toBe(true);

    const updatedPlant = PlantModel.getPlantById(plantId);
    expect(updatedPlant?.name).toBe("Updated Plant");
  });

  it("should delete a plant", () => {
    const plantData: Omit<Plant, "id"> = {
      name: "Test Plant",
      species: "Test Species",
      watering_schedule: 3,
      light_requirements: "Bright Light",
      care_tips: "Test care tips",
    };

    const plantId = PlantModel.createPlant(plantData);

    const deleted = PlantModel.deletePlant(plantId);
    expect(deleted).toBe(true);

    const deletedPlant = PlantModel.getPlantById(plantId);
    expect(deletedPlant).toBeUndefined();
  });
});

const request = require("supertest");
const app = require("../server");
const db = require("../db"); // Import the database connection

describe("Plant API", () => {
  // Run this before each test to reset the database
  beforeEach(() => {
    // Clear the plants table
    db.exec("DELETE FROM plants");

    // Insert a test plant
    db.exec(`
      INSERT INTO plants (id, name, species, watering_schedule, light_requirements, care_tips)
      VALUES (1, 'Test Plant', 'Test Species', 3, 'Bright Light', 'Test care tips')
    `);
  });

  it("should fetch all plants", async () => {
    const response = await request(app).get("/plants");
    expect(response.status).toBe(200);
    expect(Array.isArray(response.body)).toBe(true);
    expect(response.body.length).toBe(1); // Ensure the test plant is present
  });

  it("should add a new plant", async () => {
    const newPlant = {
      name: "New Plant",
      species: "New Species",
      watering_schedule: 5,
      light_requirements: "Low Light",
      care_tips: "New care tips",
    };

    const response = await request(app).post("/plants").send(newPlant);
    expect(response.status).toBe(200);
    expect(response.body.id).toBeDefined();
  });

  it("should update a plant", async () => {
    const updatedPlant = {
      name: "Updated Plant",
      species: "Updated Species",
      watering_schedule: 7,
      light_requirements: "Medium Light",
      care_tips: "Updated care tips",
    };

    const response = await request(app).put("/plants/1").send(updatedPlant);
    expect(response.status).toBe(200);
    expect(response.body.success).toBe(true);
  });

  it("should delete a plant", async () => {
    const response = await request(app).delete("/plants/1");
    expect(response.status).toBe(200);
    expect(response.body.success).toBe(true);
  });
});

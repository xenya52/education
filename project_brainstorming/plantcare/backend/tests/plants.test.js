const request = require("supertest");

const app = require("../server"); // Adjust the path to your server file

describe("Plants API", () => {
  let plantId;

  // Test GET /plants
  it("should fetch all plants", async () => {
    const response = await request(app).get("/plants");
    expect(response.statusCode).toBe(200);
    expect(Array.isArray(response.body)).toBe(true);
  });

  // Test POST /plants
  it("should add a new plant", async () => {
    const newPlant = {
      name: "Test Plant",
      species: "Test Species",
      watering_schedule: 7,
      light_requirements: "Medium",
      care_tips: "Water weekly",
    };

    const response = await request(app).post("/plants").send(newPlant);
    expect(response.statusCode).toBe(200);
    expect(response.body).toHaveProperty("id");
    plantId = response.body.id; // Save the plant ID for later tests
  });

  // Test PUT /plants/:id
  it("should update an existing plant", async () => {
    const updatedPlant = {
      name: "Updated Plant",
      species: "Updated Species",
      watering_schedule: 5,
      light_requirements: "Low",
      care_tips: "Water every 5 days",
    };

    const response = await request(app)
      .put(`/plants/${plantId}`)
      .send(updatedPlant);
    expect(response.statusCode).toBe(200);
    expect(response.body).toHaveProperty("success", true);
  });

  // Test DELETE /plants/:id
  it("should delete a plant", async () => {
    const response = await request(app).delete(`/plants/${plantId}`);
    expect(response.statusCode).toBe(200);
    expect(response.body).toHaveProperty("success", true);
  });

  // Test DELETE /plants/:id for non-existent plant
  it("should return 404 for deleting a non-existent plant", async () => {
    const response = await request(app).delete(`/plants/99999`);
    expect(response.statusCode).toBe(404);
    expect(response.body).toHaveProperty("error", "Plant not found");
  });
});

const express = require("express");
const PlantRepository = require("../PlantRepository");

const router = express.Router(); // Initialize the router

// Get all plants
router.get("/", (req, res) => {
  try {
    const plants = PlantRepository.getAllPlants();
    res.json(plants);
  } catch (error) {
    console.error("Error fetching plants:", error);
    res.status(500).json({ error: "Failed to fetch plants" });
  }
});

// Add a new plant
router.post("/", (req, res) => {
  try {
    const { name, species, watering_schedule, light_requirements, care_tips } =
      req.body;

    // Validate watering_schedule
    if (watering_schedule < 0) {
      return res
        .status(400)
        .json({ error: "Watering schedule cannot be negative." });
    }

    const plantId = PlantRepository.createPlant({
      name,
      species,
      watering_schedule,
      light_requirements,
      care_tips,
    });
    res.json({ id: plantId });
  } catch (error) {
    console.error("Error adding plant:", error);
    res.status(500).json({ error: "Failed to add plant" });
  }
});

// Update a plant
router.put("/:id", (req, res) => {
  try {
    const { name, species, watering_schedule, light_requirements, care_tips } =
      req.body;

    // Validate watering_schedule
    if (watering_schedule < 0) {
      return res
        .status(400)
        .json({ error: "Watering schedule cannot be negative." });
    }

    const plantId = parseInt(req.params.id, 10);
    const updated = PlantRepository.updatePlant(plantId, {
      name,
      species,
      watering_schedule,
      light_requirements,
      care_tips,
    });

    if (!updated) {
      return res.status(404).json({ error: "Plant not found" });
    }

    res.json({ success: updated });
  } catch (error) {
    console.error("Error updating plant:", error);
    res.status(500).json({ error: "Failed to update plant" });
  }
});

// Delete a plant
router.delete("/:id", (req, res) => {
  try {
    const plantId = parseInt(req.params.id, 10);
    const deleted = PlantRepository.deletePlant(plantId);
    if (!deleted) {
      return res.status(404).json({ error: "Plant not found" });
    }
    res.json({ success: deleted });
  } catch (error) {
    console.error("Error deleting plant:", error);
    res.status(500).json({ error: "Failed to delete plant" });
  }
});

// Export the router
module.exports = router;

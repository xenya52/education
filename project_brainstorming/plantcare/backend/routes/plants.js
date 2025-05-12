// backend/routes/plants.js
const express = require("express");
const PlantRepository = require("../PlantRepository");

const router = express.Router();

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
    const plantData = req.body;
    const plantId = PlantRepository.createPlant(plantData);
    res.json({ id: plantId });
  } catch (error) {
    console.error("Error adding plant:", error);
    res.status(500).json({ error: "Failed to add plant" });
  }
});

// Update a plant
router.put("/:id", (req, res) => {
  try {
    const plantId = parseInt(req.params.id, 10);
    const updated = PlantRepository.updatePlant(plantId, req.body);
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

module.exports = router;

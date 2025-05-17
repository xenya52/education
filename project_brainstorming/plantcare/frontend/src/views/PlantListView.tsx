import React, { useEffect, useState } from "react";
import {
  List,
  ListItem,
  ListItemText,
  Typography,
  Button,
} from "@mui/material";
import { useNavigate } from "react-router-dom"; // Import useNavigate
import type { Plant } from "../models/PlantModel";

const PlantListView: React.FC = () => {
  const [plants, setPlants] = useState<Plant[]>([]);
  const navigate = useNavigate(); // Initialize navigate

  useEffect(() => {
    // Fetch all plants from the backend API
    const fetchPlants = async () => {
      try {
        const response = await fetch("http://localhost:3001/plants");
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const data = await response.json();
        setPlants(data);
      } catch (error) {
        console.error("Error fetching plants:", error);
      }
    };

    fetchPlants();
  }, []);

  // Function to handle plant deletion
  const handleDelete = async (plantId: number) => {
    try {
      const response = await fetch(`http://localhost:3001/plants/${plantId}`, {
        method: "DELETE",
      });
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      // Remove the deleted plant from the state
      setPlants((prevPlants) =>
        prevPlants.filter((plant) => plant.id !== plantId),
      );
    } catch (error) {
      console.error("Error deleting plant:", error);
    }
  };

  return (
    <>
      <Typography variant="h4" gutterBottom>
        My Plants
      </Typography>
      <List>
        {plants.map((plant) => (
          <ListItem key={plant.id}>
            <ListItemText
              primary={plant.name}
              secondary={`Species: ${plant.species}, Watering: Every ${plant.watering_schedule} days`}
            />
            <Button
              variant="outlined"
              color="primary"
              onClick={() => navigate(`/plants/edit/${plant.id}`)}
              style={{ marginRight: "10px" }}
            >
              Edit
            </Button>
            <Button
              variant="outlined"
              color="secondary"
              onClick={() => plant.id !== undefined && handleDelete(plant.id)}
            >
              Delete
            </Button>
          </ListItem>
        ))}
      </List>
      <Button
        variant="contained"
        color="primary"
        style={{ marginTop: "20px" }}
        onClick={() => navigate("/plants/add")} // Use navigate here
      >
        Add New Plant
      </Button>
    </>
  );
};

export default PlantListView;

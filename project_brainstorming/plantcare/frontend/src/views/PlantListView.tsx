import React, { useEffect, useState } from "react";
import {
  List,
  ListItem,
  ListItemText,
  Typography,
  Button,
} from "@mui/material";
import type { Plant } from "../models/PlantModel";

const PlantListView: React.FC = () => {
  const [plants, setPlants] = useState<Plant[]>([]);

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
          </ListItem>
        ))}
      </List>
      <Button
        variant="contained"
        color="primary"
        style={{ marginTop: "20px" }}
        onClick={() => navigate("/plants/add")}
      >
        Add New Plant
      </Button>
      <Button
        variant="outlined"
        color="primary"
        onClick={() => navigate(`/plants/edit/${plant.id}`)}
      >
        Edit
      </Button>
    </>
  );
};

export default PlantListView;

import React, { useState, useEffect } from "react";
import { useNavigate, useParams } from "react-router-dom";
import {
  TextField,
  Button,
  Typography,
  Container,
  Box,
  CircularProgress,
} from "@mui/material";

const AddEditPlantView: React.FC = () => {
  const navigate = useNavigate();
  const { id } = useParams<{ id: string }>();
  const isEditMode = Boolean(id);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [plant, setPlant] = useState({
    name: "",
    species: "",
    watering_schedule: "",
    light_requirements: "",
    care_tips: "",
  });

  useEffect(() => {
    if (isEditMode) {
      setLoading(true);
      fetch(`http://localhost:3001/plants/${id}`)
        .then((response) => {
          if (!response.ok) {
            throw new Error("Failed to fetch plant details");
          }
          return response.json();
        })
        .then((data) => {
          setPlant(data);
          setLoading(false);
        })
        .catch((error) => {
          setError(error.message);
          setLoading(false);
        });
    }
  }, [id, isEditMode]);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value } = e.target;

    // Prevent negative numbers for watering_schedule
    if (name === "watering_schedule" && Number(value) < 0) {
      setError("Watering schedule cannot be negative.");
      return;
    }

    setError(null); // Clear any previous errors
    setPlant((prev) => ({ ...prev, [name]: value }));
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError(null);

    const method = isEditMode ? "PUT" : "POST";
    const url = isEditMode
      ? `http://localhost:3001/plants/${id}`
      : "http://localhost:3001/plants";

    fetch(url, {
      method,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(plant),
    })
      .then((response) => {
        if (!response.ok) {
          throw new Error(
            isEditMode ? "Failed to update plant" : "Failed to add plant",
          );
        }
        return response.json();
      })
      .then(() => {
        navigate("/");
      })
      .catch((error) => {
        setError(error.message);
        setLoading(false);
      });
  };

  if (loading) {
    return (
      <Container>
        <Box
          display="flex"
          justifyContent="center"
          alignItems="center"
          height="100vh"
        >
          <CircularProgress />
        </Box>
      </Container>
    );
  }

  return (
    <Container>
      <Typography variant="h4" gutterBottom>
        {isEditMode ? "Edit Plant" : "Add Plant"}
      </Typography>
      {error && (
        <Typography color="error" gutterBottom>
          {error}
        </Typography>
      )}
      <form onSubmit={handleSubmit}>
        <TextField
          label="Name"
          name="name"
          value={plant.name}
          onChange={handleChange}
          fullWidth
          margin="normal"
          required
        />
        <TextField
          label="Species"
          name="species"
          value={plant.species}
          onChange={handleChange}
          fullWidth
          margin="normal"
          required
        />
        <TextField
          label="Watering Schedule (days)"
          name="watering_schedule"
          value={plant.watering_schedule}
          onChange={handleChange}
          fullWidth
          margin="normal"
          required
          type="number"
          inputProps={{ min: 0 }} // Prevent negative numbers in the UI
        />
        <TextField
          label="Light Requirements"
          name="light_requirements"
          value={plant.light_requirements}
          onChange={handleChange}
          fullWidth
          margin="normal"
          required
        />
        <TextField
          label="Care Tips"
          name="care_tips"
          value={plant.care_tips}
          onChange={handleChange}
          fullWidth
          margin="normal"
          multiline
          rows={4}
        />
        <Box mt={2}>
          <Button type="submit" variant="contained" color="primary">
            {isEditMode ? "Update Plant" : "Add Plant"}
          </Button>
        </Box>
      </form>
    </Container>
  );
};

export default AddEditPlantView;

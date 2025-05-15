import React from "react";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import { BrowserRouter as Router, Route, Routes } from "react-router-dom";
import AddEditPlantView from "../AddEditPlantView";

// Mock the fetch API
global.fetch = jest.fn();

describe("AddEditPlantView", () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("renders the Add Plant form", () => {
    render(
      <Router>
        <Routes>
          <Route path="/" element={<AddEditPlantView />} />
        </Routes>
      </Router>,
    );

    expect(screen.getByText("Add Plant")).toBeInTheDocument();
    expect(screen.getByLabelText("Name")).toBeInTheDocument();
    expect(screen.getByLabelText("Species")).toBeInTheDocument();
    expect(
      screen.getByLabelText("Watering Schedule (days)"),
    ).toBeInTheDocument();
    expect(screen.getByLabelText("Light Requirements")).toBeInTheDocument();
    expect(screen.getByLabelText("Care Tips")).toBeInTheDocument();
    expect(
      screen.getByRole("button", { name: /add plant/i }),
    ).toBeInTheDocument();
  });

  it("renders the Edit Plant form with preloaded data", async () => {
    // Mock the fetch call to preload plant data
    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: true,
      json: async () => ({
        name: "Test Plant",
        species: "Test Species",
        watering_schedule: 3,
        light_requirements: "Bright Light",
        care_tips: "Test care tips",
      }),
    });

    render(
      <Router>
        <Routes>
          <Route path="/plants/edit/:id" element={<AddEditPlantView />} />
        </Routes>
      </Router>,
    );

    // Wait for the data to load
    await waitFor(() => {
      expect(screen.getByDisplayValue("Test Plant")).toBeInTheDocument();
      expect(screen.getByDisplayValue("Test Species")).toBeInTheDocument();
      expect(screen.getByDisplayValue("3")).toBeInTheDocument();
      expect(screen.getByDisplayValue("Bright Light")).toBeInTheDocument();
      expect(screen.getByDisplayValue("Test care tips")).toBeInTheDocument();
    });
  });

  it("submits the Add Plant form", async () => {
    // Mock the fetch call for form submission
    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: true,
      json: async () => ({ id: 1 }),
    });

    render(
      <Router>
        <Routes>
          <Route path="/" element={<AddEditPlantView />} />
        </Routes>
      </Router>,
    );

    // Fill out the form
    fireEvent.change(screen.getByLabelText("Name"), {
      target: { value: "New Plant" },
    });
    fireEvent.change(screen.getByLabelText("Species"), {
      target: { value: "New Species" },
    });
    fireEvent.change(screen.getByLabelText("Watering Schedule (days)"), {
      target: { value: "5" },
    });
    fireEvent.change(screen.getByLabelText("Light Requirements"), {
      target: { value: "Low Light" },
    });
    fireEvent.change(screen.getByLabelText("Care Tips"), {
      target: { value: "New care tips" },
    });

    // Submit the form
    fireEvent.click(screen.getByRole("button", { name: /add plant/i }));

    // Wait for the API call to complete
    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith(
        "http://localhost:3001/plants",
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            name: "New Plant",
            species: "New Species",
            watering_schedule: "5",
            light_requirements: "Low Light",
            care_tips: "New care tips",
          }),
        },
      );
    });
  });

  it("handles API errors gracefully", async () => {
    // Mock the fetch call to simulate an error
    (global.fetch as jest.Mock).mockRejectedValueOnce(
      new Error("Failed to add plant"),
    );

    render(
      <Router>
        <Routes>
          <Route path="/" element={<AddEditPlantView />} />
        </Routes>
      </Router>,
    );

    // Fill out the form
    fireEvent.change(screen.getByLabelText("Name"), {
      target: { value: "New Plant" },
    });
    fireEvent.change(screen.getByLabelText("Species"), {
      target: { value: "New Species" },
    });
    fireEvent.change(screen.getByLabelText("Watering Schedule (days)"), {
      target: { value: "5" },
    });
    fireEvent.change(screen.getByLabelText("Light Requirements"), {
      target: { value: "Low Light" },
    });
    fireEvent.change(screen.getByLabelText("Care Tips"), {
      target: { value: "New care tips" },
    });

    // Submit the form
    fireEvent.click(screen.getByRole("button", { name: /add plant/i }));

    // Wait for the error to be displayed
    await waitFor(() => {
      expect(screen.getByText("Failed to add plant")).toBeInTheDocument();
    });
  });

  it("deletes a plant", async () => {
    // Mock the fetch call for deleting a plant
    (global.fetch as jest.Mock).mockResolvedValueOnce({
      ok: true,
    });

    render(
      <Router>
        <Routes>
          <Route path="/" element={<AddEditPlantView />} />
        </Routes>
      </Router>,
    );

    // Simulate clicking the delete button
    const deleteButton = screen.getByRole("button", { name: /delete plant/i });
    fireEvent.click(deleteButton);

    // Wait for the API call to complete
    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith(
        "http://localhost:3001/plants/1", // Assuming plant ID is 1
        {
          method: "DELETE",
        },
      );
    });

    // Ensure the plant is removed from the UI
    await waitFor(() => {
      expect(screen.queryByText("Test Plant")).not.toBeInTheDocument();
    });
  });
});

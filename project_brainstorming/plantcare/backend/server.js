const express = require("express");
const cors = require("cors");
const plantsRoutes = require("./routes/plants"); // Import the plants routes

// Initialize the Express app
const app = express();
app.use(cors());
app.use(express.json());

// Use the plants routes
app.use("/plants", plantsRoutes);

// Global error handler (optional)
app.use((err, req, res, next) => {
  console.error("Unhandled error:", err);
  res.status(500).json({ error: "An unexpected error occurred" });
});

// Export the app for testing
module.exports = app;

// Start the server only if not in test mode
if (require.main === module) {
  const PORT = process.env.PORT || 3001; // Use environment variable for port
  app.listen(PORT, () => {
    console.log(`Backend server running on http://localhost:${PORT}`);
  });
}

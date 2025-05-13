import React from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import Layout from "./views/Layout";
import PlantListView from "./views/PlantListView";
import AddEditPlantView from "./views/AddEditPlantView";

const App: React.FC = () => {
  return (
    <Router>
      <Layout>
        <Routes>
          <Route path="/" element={<PlantListView />} />
          <Route path="/plants/add" element={<AddEditPlantView />} />
          <Route path="/plants/edit/:id" element={<AddEditPlantView />} />
        </Routes>
      </Layout>
    </Router>
  );
};

export default App;

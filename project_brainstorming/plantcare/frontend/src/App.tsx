import React from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import Layout from "./views/Layout";
import PlantListView from "./views/PlantListView";

const App: React.FC = () => {
  return (
    <Router>
      <Layout>
        <Routes>
          <Route path="/" element={<PlantListView />} />
          {/* Add routes for Add/Edit Plant and Plant Details later */}
        </Routes>
      </Layout>
    </Router>
  );
};

export default App;

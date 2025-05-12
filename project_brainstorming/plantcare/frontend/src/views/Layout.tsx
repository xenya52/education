import React from "react";
import { AppBar, Toolbar, Typography, Container } from "@mui/material";

const Layout: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  return (
    <>
      <AppBar position="static">
        <Toolbar>
          <Typography variant="h6">Plantcare</Typography>
        </Toolbar>
      </AppBar>
      <Container style={{ marginTop: "20px" }}>{children}</Container>
    </>
  );
};

export default Layout;

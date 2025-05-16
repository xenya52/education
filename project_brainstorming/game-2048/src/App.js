import React from "react";
import { Provider } from "react-redux";
import store from "./redux/store";
import GameBoard from "./components/GameBoard";

function App() {
  return (
    <Provider store={store}>
      <div className="App">
        <h1>2048 Game</h1>
        <GameBoard />
      </div>
    </Provider>
  );
}

export default App;

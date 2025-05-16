import { createSlice } from '@reduxjs/toolkit';
import { initializeBoard, moveTiles, checkGameOver } from '../services/gameLogic';

const initialState = {
  board: initializeBoard(),
  score: 0,
  gameOver: false,
};

const gameSlice = createSlice({
  name: 'game',
  initialState,
  reducers: {
    resetGame: (state) => {
      state.board = initializeBoard();
      state.score = 0;
      state.gameOver = false;
    },
    move: (state, action) => {
      const { newBoard, newScore } = moveTiles(state.board, action.payload);
      state.board = newBoard;
      state.score += newScore;
      state.gameOver = checkGameOver(newBoard);
    },
  },
});

export const { resetGame, move } = gameSlice.actions;
export default gameSlice.reducer;
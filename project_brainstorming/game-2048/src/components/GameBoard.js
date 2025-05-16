import React from "react";
import { useSelector, useDispatch } from "react-redux";
import { move, resetGame } from "../redux/gameSlice";
import "../styles/GameBoard.css";

const GameBoard = () => {
  const dispatch = useDispatch();
  const { board, score, gameOver } = useSelector((state) => state.game);

  const handleKeyDown = React.useCallback(
    (event) => {
      if (gameOver) return;

      switch (event.key) {
        case "ArrowUp":
          dispatch(move("up"));
          break;
        case "ArrowDown":
          dispatch(move("down"));
          break;
        case "ArrowLeft":
          dispatch(move("left"));
          break;
        case "ArrowRight":
          dispatch(move("right"));
          break;
        default:
          break;
      }
    },
    [dispatch, gameOver],
  );

  React.useEffect(() => {
    window.addEventListener("keydown", handleKeyDown);
    return () => {
      window.removeEventListener("keydown", handleKeyDown);
    };
  }, [gameOver, handleKeyDown]);

  const renderTile = (value, rowIndex, colIndex) => {
    const tileClass = value === 0 ? "tile empty" : `tile tile-${value}`;
    return (
      <div
        key={`${rowIndex}-${colIndex}`}
        className={tileClass}
        style={{ margin: "5px" }}
      >
        {value !== 0 && value}
      </div>
    );
  };

  return (
    <div className="game-container">
      <div className="scoreboard">
        <div>Score: {score}</div>
        {gameOver && <div className="game-over">Game Over!</div>}
      </div>
      <div
        className="game-board"
        style={{
          display: "grid",
          gridTemplateColumns: `repeat(${board[0].length}, 1fr)`,
          gap: "5px",
        }}
      >
        {board.flatMap((row, rowIndex) =>
          row.map((value, colIndex) => renderTile(value, rowIndex, colIndex)),
        )}
      </div>
      <button className="reset-button" onClick={() => dispatch(resetGame())}>
        Reset Game
      </button>
    </div>
  );
};

export default GameBoard;

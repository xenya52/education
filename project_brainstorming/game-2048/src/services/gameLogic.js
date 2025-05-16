const BOARD_SIZE = 4;

export function initializeBoard() {
  const board = Array.from({ length: BOARD_SIZE }, () =>
    Array(BOARD_SIZE).fill(0),
  );
  addRandomTile(board);
  addRandomTile(board);
  return board;
}

export function addRandomTile(board) {
  const emptyTiles = [];
  for (let row = 0; row < BOARD_SIZE; row++) {
    for (let col = 0; col < BOARD_SIZE; col++) {
      if (board[row][col] === 0) {
        emptyTiles.push({ row, col });
      }
    }
  }

  if (emptyTiles.length > 0) {
    const { row, col } =
      emptyTiles[Math.floor(Math.random() * emptyTiles.length)];
    board[row][col] = Math.random() < 0.9 ? 2 : 4;
  }
}

export function moveTiles(board, direction) {
  let newBoard = cloneBoard(board);
  let score = 0;

  if (direction === "up" || direction === "down") {
    for (let col = 0; col < BOARD_SIZE; col++) {
      let column = [];
      for (let row = 0; row < BOARD_SIZE; row++) {
        column.push(newBoard[row][col]);
      }
      const { mergedRow: mergedColumn, rowScore: columnScore } = mergeTiles(
        direction === "up" ? column : column.reverse(),
      );
      score += columnScore;
      column = direction === "up" ? mergedColumn : mergedColumn.reverse();
      for (let row = 0; row < BOARD_SIZE; row++) {
        newBoard[row][col] = column[row];
      }
    }
  } else if (direction === "left" || direction === "right") {
    for (let row = 0; row < BOARD_SIZE; row++) {
      const { mergedRow, rowScore } = mergeTiles(
        direction === "left" ? newBoard[row] : newBoard[row].reverse(),
      );
      score += rowScore;
      newBoard[row] = direction === "left" ? mergedRow : mergedRow.reverse();
    }
  }

  if (!boardsEqual(board, newBoard)) {
    addRandomTile(newBoard);
  }

  return { newBoard, newScore: score, gameOver: checkGameOver(newBoard) };
}

function mergeTiles(line) {
  let mergedLine = line.filter((value) => value !== 0);
  let score = 0;

  for (let i = 0; i < mergedLine.length - 1; i++) {
    if (mergedLine[i] === mergedLine[i + 1]) {
      mergedLine[i] *= 2;
      score += mergedLine[i];
      mergedLine.splice(i + 1, 1);
    }
  }

  while (mergedLine.length < BOARD_SIZE) {
    mergedLine.push(0);
  }

  return { mergedRow: mergedLine, rowScore: score };
}

function cloneBoard(board) {
  return board.map((row) => [...row]);
}

function boardsEqual(board1, board2) {
  for (let row = 0; row < BOARD_SIZE; row++) {
    for (let col = 0; col < BOARD_SIZE; col++) {
      if (board1[row][col] !== board2[row][col]) {
        return false;
      }
    }
  }
  return true;
}

export function checkGameOver(board) {
  for (let row = 0; row < BOARD_SIZE; row++) {
    for (let col = 0; col < BOARD_SIZE; col++) {
      if (board[row][col] === 0) return false;
      if (row > 0 && board[row][col] === board[row - 1][col]) return false;
      if (row < BOARD_SIZE - 1 && board[row][col] === board[row + 1][col])
        return false;
      if (col > 0 && board[row][col] === board[row][col - 1]) return false;
      if (col < BOARD_SIZE - 1 && board[row][col] === board[row][col + 1])
        return false;
    }
  }
  return true;
}

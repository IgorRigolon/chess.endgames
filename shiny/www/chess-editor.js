var board;

$(document).ready(function() {
  board = Chessboard('myBoard', {
    draggable: true,
    dropOffBoard: 'trash',
    sparePieces: true,
    position: 'empty',
    pieceTheme: 'img/{piece}.svg'
  });
  
  $('#clearBtn').on('click', function() {
    board.clear();
  });
  
  // Send FEN when search buttons are clicked
  $('#search_material').on('click', function() {
    sendFenToShiny();
  });
  
  $('#search_position').on('click', function() {
    sendFenToShiny();
  });
});

function sendFenToShiny() {
  if (board) {
    var fen = board.fen();
    Shiny.setInputValue('current_fen', fen, {priority: "event"});
  }
}
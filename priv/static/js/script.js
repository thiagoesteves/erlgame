// Declare Global Variables
const snake_game = "erlgame_snake_sm";
var arena = {
  row:    21,
  column: 21,
};
var websocket;
var player = document.getElementById("player");
var output = document.getElementById("output");
var board_game = document.querySelector('.game');
var player_points = document.getElementById("current player");
var best_player_points = document.getElementById("best player");
// Register Callbacks
document.onkeydown = checkKey;

function connectServer()
{
    wsHost = "ws://" + window.location.host + "/websocket";
    showErlangOutputScreen('<b>Connecting to: ' +  wsHost + '</b>');
    websocket = new WebSocket(wsHost);
    // Register Callbacks
    websocket.onopen    = function(evt) { onOpen(evt) };
    websocket.onclose   = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror   = function(evt) { onError(evt) };
};

function showErlangOutputScreen(html) {
  var el = document.createElement("p");
  el.innerHTML = html;
  output.insertBefore(el, output.firstChild);
};

function showPlayerPoints(points) {
  player_points.innerHTML =
  `<div>PLAYER</div><div>`+player.value+`</div><div>POINTS</div><div>`+points+`</div>`;
};

function updateSnakeBoard(json_from_erlang) {
  var obj = JSON.parse(json_from_erlang);
  console.log(obj);
  // Parse Snake position
  snake_position = Object.entries(obj.snake).map( ([k,v]) => v );
  var drawning='';
  for (var row=(arena.row-1); row>=0; row--) {
    for (var column=0; column<arena.column; column++) {
      if (obj.food.x === column && obj.food.y === row) {
        drawning += `<div style="background-color: red;">@</div>`;
      } else if (snake_position.find(elem => elem.x === column && elem.y === row)) {
        drawning += `<div style="background-color: lightblue;"> </div>`;
      } else {
        drawning += `<div> </div>`;
      }
    }
  }
  // Print game board
  board_game.innerHTML = drawning;
  // Print Player points
  showPlayerPoints(obj.points);
};

function updateSnakeBesties(json_from_erlang) {
  var obj = JSON.parse(json_from_erlang);
  // Parse Snake besties
  const besties = Object.entries(obj.best_players.erlgame_snake_sm).map( 
    ([k,v]) =>  
    `<div>BEST PLAYER</div><div>`+k+`</div><div>POINTS</div><div>`+v+`</div>`);
  best_player_points.innerHTML = besties.join("");
};

function onOpen(evt) {
  showErlangOutputScreen('<span style="color: green;">CONNECTED</span>');
  requestSnakeBestPlayer();
};

function onClose(evt) {
  showErlangOutputScreen('<span style="color: red;"> Disconnected by '+ evt.reason +'</span>');
  // Check if a Game Over was received
  if (evt.reason.match(/^(?=.*Game)(?=.*Over)/) || {}) {
    gameOver();
  }
};

function onMessage(evt) {
  showErlangOutputScreen('<span style="color: blue;">' + evt.data + '</span>');
  switch (evt.data) {
    // Snake game update info
    case (evt.data.match(/^(?=.*update)(?=.*erlgame_snake_sm)/) || {}).input:
      updateSnakeBoard(evt.data);
      break;
    // Snake game best player
    case (evt.data.match(/^(?=.*best_players)(?=.*erlgame_snake_sm)/) || {}).input:
      updateSnakeBesties(evt.data);
      break;
    // Invalid message
    default:
      console.log("Didn't match" + evt.data);
      break;
  };
};

function onError(evt) {
	showErlangOutputScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
};

function checkKey(e) {

  e = e || window.event;

  if (e.keyCode == '38') {
    e.preventDefault(); // Avoid scrolling up/down/left/right
    sendAction("up");
  }
  else if (e.keyCode == '40') {
    e.preventDefault();
    sendAction("down");
  }
  else if (e.keyCode == '37') {
    e.preventDefault();
    sendAction("left");
  }
  else if (e.keyCode == '39') {
    e.preventDefault();
    sendAction("right");
  }
}

function sendAction(move_player) {
  // Compose message to be sent
  var msg = {
    game: snake_game,
    action: move_player,
    user: player.value
  };
  sendMsg(msg);
};

function createSnakeGame() {
  // Compose message to be sent
  var msg = {
    game: snake_game,
    user: player.value
  };
  sendMsg(msg);
  // Hide button
  document.getElementById('play').style.visibility = 'hidden';
};

function requestSnakeBestPlayer() {
  // Compose message to be sent
  var msg = {
    request: "get_best_player",
    game: snake_game,
  };
  sendMsg(msg);
};

// Check if websocket is open and send it
function sendMsg(msg) {
	if (websocket.readyState == websocket.OPEN) {
		websocket.send(JSON.stringify(msg));
	} else {
		showErlangOutputScreen('websocket is not connected');
	};
};

function gameOver() {
  console.log("Game Over");
};

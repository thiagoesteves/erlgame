// Declare Global Variables
var arena = {
  row:    21,
  column: 21,
};
var websocket;
var player = document.getElementById("player");
var output = document.getElementById("output");
var board_game = document.querySelector('.game');
// Register Callbacks
document.onkeydown = checkKey;

function connectServer()
{
    wsHost = "ws://" + window.location.host + "/websocket";
    showScreen('<b>Connecting to: ' +  wsHost + '</b>');
    websocket = new WebSocket(wsHost);
    // Register Callbacks
    websocket.onopen    = function(evt) { onOpen(evt) };
    websocket.onclose   = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror   = function(evt) { onError(evt) };
};

function showScreen(html) {
  var el = document.createElement("p");
  el.innerHTML = html;
  output.insertBefore(el, output.firstChild);
};

function updateBoard(json_from_erlang) {
  // console.log(json_from_erlang);
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
};

function onOpen(evt) {
  showScreen('<span style="color: green;">CONNECTED</span>');
  // createGame("Thiago");
};

function onClose(evt) {
	showScreen('<span style="color: red;">DISCONNECTED</span>');
};

function onMessage(evt) {
  showScreen('<span style="color: blue;">' + evt.data + '</span>');
  updateBoard(evt.data);
};

function onError(evt) {
	showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
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
    action: move_player,
    user: player.value
  };
  sendMsg(msg);
};

function createGame() {
  // Compose message to be sent
  var msg = {
    user: player.value
  };
  sendMsg(msg);
};

//Check if websocket is open and send it
function sendMsg(msg) {
	if (websocket.readyState == websocket.OPEN) {
		websocket.send(JSON.stringify(msg));
	} else {
		showScreen('websocket is not connected');
	};
};

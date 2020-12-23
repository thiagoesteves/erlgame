var websocket;
var server = document.getElementById("server");
var output = document.getElementById("output");
server.value = "ws://" + window.location.host + "/websocket";

function connectServer()
{
    wsHost = server.value;
    websocket = new WebSocket(wsHost);
    showScreen('<b>Connecting to: ' +  wsHost + '</b>');
};

function showScreen(html) {
  var el = document.createElement("p");
  el.innerHTML = html;
  output.innerText(el, output.firstChild);
};
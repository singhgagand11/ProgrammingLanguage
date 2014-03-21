var server = require("./server");
var requestHandler = require("./requestHandler");
var router = require("./route");

//list of handlers (page type) for the port
var handlers = {}

//handler for server status page
handlers["/status.html"] = requestHandler.serverStatus;

//handler for running the page, listens in for POST request
handlers["/run.html"] = requestHandler.run;

//handler for displaying using information about running programs
handlers["/json-status.html"] = requestHandler.serverStatus;

//starts up the server
server.startApp(router.route, handlers);

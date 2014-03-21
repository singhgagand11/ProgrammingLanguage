var http = require("http");
var url = require("url");
var kport = 8888;

/*
 * Starts the server up on the kport that listens in 
 * for request, which get routed to the correct handler.
 */
function startApp(route, handle) {
   function onRequest(request, response) {
      //represents the pathname

      var pathname = url.parse(request.url).pathname;

      
      //route the path to handle the request
      route(handle, request, response, pathname);
     }
      console.log("starting server at port " + kport);
     //create a server which will listen for request
     //on port 8888
     http.createServer(onRequest).listen(kport);
}


exports.startApp = startApp;

var url = require("url");

/* redirects the request to the server to the correct 
 * handler based on the pathname (/run, /json-status, or etc)
 */
function route (handlers, request, response, pathname) {
   console.log(pathname);
   if (typeof handlers[pathname] == 'function') {
      handlers[pathname](request, response);  
   } else {
      console.log("No request handler found");
      response.writeHead(404, {"Content-Type" : "text/plain"});
      response.write("404 Not Found");
      response.end();
   }
   
} 

exports.route = route;

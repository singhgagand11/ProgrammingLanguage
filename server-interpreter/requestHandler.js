var interp = require("./interp");
var cp = require('child_process');
var http = require('http');
/* 
 * global variables for the children process, which are
 * POST request of JSON string of programs to run on the
 * server. The variable is used to query status of each
 * running program.
 *
 * Keys are process pid and value are program object
 * for more on program object see interp.js.  
 */
var childrens = new Object();

/* 
 * a global variable use to print information about
 * executing method/program 
 */
var debug = true;

/*
 * Prints messages to the console only if debug mode is on.
 * @param message : message to print to the console
 */
function consoleF(message) {
   if(debug) {
      console.log(message);
  }
}

/*
 * Writes the name and mood of all running program to 
 * the request hostname in a JSON string.
 * i.e if a program name MyProgram and Dr.Chipper were running
 * then the string would be [{"name": "MyProgram","mood": "sulky"},
 * {"name":"Dr. Chipper","mood":"relentlessly optimistic"}]
 * 
 * @param request host request where web action was taken place
 * @prarm response a object used to provide data back to the 
 *        host 
 */
function serverStatus(request, response) {
   consoleF("serverStatus" );
   
   //SET the response header to 200, text/html content
   response.writeHead(200, {"Content-Type": "text/html"});
   
   //SET childArray to empty array of programs
   var childArray = [];   

   //FOR every child in children DO  
   for (child  in childrens) { 
      //append the childi program into childArray
      childArray.push(childrens[child]);
   }

   //write the array of programs as JSON string to the response
   response.write(JSON.stringify(childArray));
   //close the response page
   response.end();
}

/*
 * A handler for running programs. The program are recieved/encoded in
 * JSON string which are send through POST request. The string is 
 * converted to JSON object which is interperter in interp.js file
 * in a seperate thread. Following MV from (MVC) design principle
 * a handler is set, which lets child notify parent of changes in their 
 * mood, or when the child is about to exit. Setting the mood allows
 * the child to be added to children query and is removed on exit. 
 * 
 * The string follows the following format :
 * {name:<programname>,mood:<programmood>,kont:<programkont>}
 * <programkont> = 
 *            |{ty:"base-k",args:[]}
 *            |{ty:"loop-k",args:[<number>,<exp>,<programkont>]}
 *            |{ty:"seq-k",args:[[<exp>,...],<programkont>]}
 *
 * <exp> = 
 *     |{ty:"sleep",args:[<number>]}
 *     | {ty:"set-mood",args:[<string>]}
 *     | {ty:"loop",args:[<number>,<exp>]}
 *     | {ty:"goto",args:[<string>,<number>]}
 *     | {ty:"seq",args:[<exp>,...]}
 */
function run(request, response ) {
   //SET data to empty string
   var data = "";
   
   //create on handler which queries the postData being send 
   request.on("data", function(postData) {
      //append the postData to data
      data+=postData;
   });

   //create a end event handler 
   request.on("end", function() {
      //IF data was receieved THEN
      if(data !== "") {
         //SET json to json object created from data
         var json = JSON.parse(data);  
         //IF json is a valid JSON object THEN
         if(json != 0) {
            consoleF("json: " + json);
            //TELL the responser that the valid data was recieved
            response.write("json " + data + 
                           " encoding recieved. Thanks!");
            //SET child to new process of interp type
            var child = cp.fork(__dirname + '/interp.js');
            
            //SET a message handler to talk to the child           
            child.on('message', function (message) {
                //IF child send a "sendPost" message THEN
                if(message.message === "sendPost") {
                   //CALL send with options and data
                   send(message.options, 
                            message.data);
                //ELSE IF child send a "noftifyParent" message THEN
                } else if (message.message === "notifyParent") {
                   //queue the child in the map of childrens by 
                   //childid and app
                   childrens[message.childid] = message.app;
                }
            });

            //SET a exit handler on the child
            child.on('exit', function(code, signal) {
               consoleF("exiting...");
               //remove the child from childrens map
               delete childrens[child.pid]
            });
            //call child to interp the json onject
            child.send( {call: "interp", args: json});
          }
      }
      //END the response handler
      response.end();
   });   
}

/*
 * Send chunk of data to options. On error, error message
 * is printed on the console.
 * @param Options object which contains a host name, 
 *        port, path, and header options. See node.js 
 *        http module for more information.
 * @param data chunk of data to send over to options
 *  
 */
function send(options, data) {
   //SET var to http request for host, port, and path
   //in options
   var req = http.request(options, function(res) {
      //print out the res statuscode
      console.log('STATUS: ' + res.statusCode);
      //print out the json string 
      console.log('HEADERS: ' + JSON.stringify(res.headers));
      res.setEncoding('utf8');
      //create a on handler to print the data to the consoleF
      res.on('data', function (chunk) {
         consoleF('BODY: ' + chunk);
      });
   });
   
   //set a error handler to print error to the console
   req.on('error', function(e) {
      console.log('problem with request: ' + e.message);
   });

   // write data to request body
   req.write(data);
   //close the http request 
   req.end();
}


/** exports for serverStaus **/
exports.serverStatus = serverStatus;
/** exports for run **/
exports.run = run;
